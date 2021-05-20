library(dplyr)
library(runner)
library(tidyr)
library(doParallel)
library(ggplot2)


## BASE MODEL CONSTANTS

MODEL_N<-10000

PROPORTION_B<-0.7
# Bear in mind that if this isn't 50:50 then it's easy for the high absolute number of connections in the smaller population to let the disease run harder in that population
# May well be accurate!

SAME_POPULATION_CONNECTION<-0.8

BASE_PROB<-0.006
MITIG_PROB<-0.002
#transmission probabilities with mitigations off or on (eg 0.006,0.002)

MITIG_START<-200
MITIG_STOP<-40
#cases to start and stop mitigations



OVERALL_VACCINE_RATE<-0.002
#proportion per day vaccinated


VACCINE_EQUALITY<-1
#1 = same in A/B, <1 = less in A, >1= less in B

VACCINE_START_DAY<-20

sspread<-0 # add superspreaders if 1



VACCINATE_A_RATE<-OVERALL_VACCINE_RATE/(PROPORTION_B/VACCINE_EQUALITY+1-PROPORTION_B)*MODEL_N*(1-PROPORTION_B)
VACCINATE_B_RATE<-OVERALL_VACCINE_RATE/(PROPORTION_B+VACCINE_EQUALITY-PROPORTION_B*VACCINE_EQUALITY)*MODEL_N*PROPORTION_B


MC_LOOPS<-16


SingleRun<-function(iter) {
  library(dplyr)
  library(runner)
  
  
  people<-data.frame(
    id=seq.int(1,MODEL_N),
    connections=pmax(as.integer(rlnorm(MODEL_N,log(20),log(1.3)))+5,1),  ##logsd from 1 = no variation, to 1.5 = plenty (up to 100ish), 2 = loads (up to 300ish). NB adjusting this will affect mean
    population=ifelse(runif(MODEL_N)<PROPORTION_B,"B","A"),
    state="S",
    sday=0
  )
  
  
  
  if (sspread==1){
    ## add superspreaders by redistributing connections (so that number of connections remains constant)
    people$connections<-people$connections-1
    randp<-sample.int(MODEL_N,MODEL_N/100)
    people$connections[people$id %in% randp]<-people$connections[people$id %in% randp]+100
    
    people$connections<-people$connections-1
    randp<-sample.int(MODEL_N,MODEL_N/200)
    people$connections[people$id %in% randp]<-people$connections[people$id %in% randp]+200
    
    people$connections<-people$connections-1
    randp<-sample.int(MODEL_N,MODEL_N/200)
    people$connections[people$id %in% randp]<-people$connections[people$id %in% randp]+200
    
    
    people$connections<-people$connections-1
    randp<-sample.int(MODEL_N,MODEL_N/1000)
    people$connections[people$id %in% randp]<-people$connections[people$id %in% randp]+1000
    
  }
  
  
  
  print(mean(people$connections))
  
  
  # connections<-as.data.frame(lapply(people,rep,people$connections))
  # connections$id2<-sample(people$id,nrow(connections),replace=TRUE)
  
  connectionsAA<-as.data.frame(lapply(people[people$population=="A",],rep,people[people$population=="A",]$connections*SAME_POPULATION_CONNECTION))
  connectionsAA$id2<-sample(people[people$population=="A",]$id,nrow(connectionsAA),replace=TRUE)
  
  connectionsAB<-as.data.frame(lapply(people[people$population=="A",],rep,people[people$population=="A",]$connections*(1-SAME_POPULATION_CONNECTION)))
  connectionsAB$id2<-sample(people[people$population=="B",]$id,nrow(connectionsAB),replace=TRUE)
  
  connectionsBB<-as.data.frame(lapply(people[people$population=="B",],rep,people[people$population=="B",]$connections*SAME_POPULATION_CONNECTION))
  connectionsBB$id2<-sample(people[people$population=="B",]$id,nrow(connectionsBB),replace=TRUE)
  
  connectionsBA<-as.data.frame(lapply(people[people$population=="B",],rep,people[people$population=="B",]$connections*(1-SAME_POPULATION_CONNECTION)))
  connectionsBA$id2<-sample(people[people$population=="A",]$id,nrow(connectionsBA),replace=TRUE)
  
  connections<-rbind(connectionsAA,connectionsAB,connectionsBB,connectionsBA)
  
  people$state[people$id<20]<-"E" #initial exposure of 20 people
  
  day<-0
  result<-data.frame(
    day=0,
    S=MODEL_N,
    E=0,
    I=0,
    R=0,
    V=0
  )
  
  resultA<-result
  resultB<-result
  
  cnum<-nrow(connections)
  
  mittimes<-0
  prob<-BASE_PROB #initial probability of transmission for each connection per day
  
  ## Loop
  repeat{
    
    day<-day+1
    people$sday<-people$sday+1
    people$state[people$state=="E"&people$sday==4]<-"EI"  ## E to I on day 4
    people$sday[people$state=="EI"]<-0
    people$state[people$state=="EI"]<-"I"
    
    people$state[people$state=="I"&people$sday==7]<-"IR"  ## I to R on day 7
    people$sday[people$state=="IR"]<-0
    people$state[people$state=="IR"]<-"R"
    
    connections$transmit<-ifelse((people$state[connections$id]=="I" | people$state[connections$id2]=="I") & runif(cnum)<prob,1,0) ##p(transmit) 0.004 to each contact
    conn_tmp<-connections[connections$transmit==1,]
    inf<-unique(c(conn_tmp$id,conn_tmp$id2))
    ## 2x slower and also includes the "I" people
    ## however probability is based on each connection so (correctly) more connected people will have multiple goes at being infected
    
    # people$receive<-0
    # people$receive[people$id %in% connections$id2[connections$id %in% people$id[people$state=="I"]]]<-1
    # people$receive[people$id %in% connections$id[connections$id2 %in% people$id[people$state=="I"]]]<-1
    # people$receive[people$receive==1]<-ifelse(runif(sum(people$receive==1))<0.004,1,0)
    # inf<-unique(people$id[people$receive==1])
    ## faster and doesn't include the "I" people
    ## however not as good - as probability in this instance is fixed per person (regardless of how many infected connections they have)
    
    people$state[(people$id %in% inf) & people$state=="S"]<-"SE"
    people$sday[people$state=="SE"]<-0
    people$state[people$state=="SE"]<-"E"
    
    
    if (day>=VACCINE_START_DAY){
      vaccs<-sample(which(people$population=="A"&people$state!="V"),min(nrow(people[people$population=="A"&people$state!="V",]),VACCINATE_A_RATE))
      people[vaccs,"state"]<-ifelse(people[vaccs,"state"]=="S","V",people[vaccs,"state"])
      #people[sample(which(people$population=="B"&people$state!="V"),min(nrow(people[people$population=="B"&people$state!="V",]),VACCINATE_B_RATE)),"state"]<-"V"
      vaccs<-sample(which(people$population=="B"&people$state!="V"),min(nrow(people[people$population=="B"&people$state!="V",]),VACCINATE_B_RATE))
      people[vaccs,"state"]<-ifelse(people[vaccs,"state"]=="S","V",people[vaccs,"state"])
      
      #slightly complex so that we don't set people to "V" if they actually caught it earlier; for reporting purposes. Commented line would do it quicker if that didn't matter!
    }
    
    
    
    
    tab<-table(people$state)
    result<-rbind(result,data.frame(
      day=day,
      S=tab["S"],
      E=tab["E"],
      I=tab["I"],
      R=tab["R"],
      V=tab["V"],
      row.names=NULL
    ))
    
    tabA<-table(people[people$population=="A",]$state)
    resultA<-rbind(resultA,data.frame(
      day=day,
      S=tabA["S"],
      E=tabA["E"],
      I=tabA["I"],
      R=tabA["R"],
      V=tabA["V"],
      row.names=NULL
    ))
    
    tabB<-table(people[people$population=="B",]$state)
    resultB<-rbind(resultB,data.frame(
      day=day,
      S=tabB["S"],
      E=tabB["E"],
      I=tabB["I"],
      R=tabB["R"],
      V=tabB["V"],
      row.names=NULL
    ))
    
    if(day==1000) {break}
    if(is.na(tab["E"]) && is.na(tab["I"])) {break}
    
    
    ## Adjust transmission probabilities here for distancing etc
    if(is.na(tab["I"])) {tab["I"]<-0}
    if(tab["I"]>MITIG_START & mittimes<99) {
      if(prob>MITIG_PROB) {print(paste0("mitigations on - day ",day))
        prob<-MITIG_PROB
        mittimes<-mittimes+1}} ## social distancing/mitigations on
    if(tab["I"]<MITIG_STOP) {
      if(prob<BASE_PROB) {print(paste0("mitigations off - day ",day))
        prob<-BASE_PROB}} ## back to normal!
    
  }
  
  result$Reff<-result$E/result$I*7/4 #7 days infected vs 4 days exposed
  
  result<-result %>% 
    mutate(
      mean = mean_run(`Reff`,14,idx=day)
    )
  result$mean[is.na(result$mean)]<-0
  
  
  combined<-merge(resultA,resultB,by="day",suffixes=c("A","B"))
  combined<-merge(combined,result,by="day")
  combined$iter<-iter

  
  return(combined)
  
}



## Single run

# cl <- makeCluster(8) 
# registerDoParallel(cl)  
# 
# Overall<-foreach(i=1:MC_LOOPS,.combine='rbind') %dopar% {SingleRun(i)}
# Overall[is.na(Overall)]<-0
# 
# stopCluster(cl)
# 
# 
# #fill extra data as different runs will have stopped at different points
# Overall<-Overall %>% 
#   complete(day,iter) %>% 
#   group_by(iter) %>% 
#   fill(everything())
# 
# 
# # 
# # 
# # library(ggplot2)
# # print(ggplot(result)+
# #         geom_line(aes(x=day,y=R),colour="red",size=2) +
# #         geom_line(aes(x=day,y=I),size=2) +
# #         geom_line(aes(x=day,y=mean*2000), colour="green",size=2) +
# #         ylab("Cases")+
# #         xlab("Day")+
# #         scale_y_continuous(
# #           name="Cases",
# #           sec.axis = sec_axis(~./2000, name="Effective R")
# #         ) +
# #         theme_classic(base_size=18)
# # )
# # 
# # 
# # print(ggplot(resultA)+
# #         geom_line(aes(x=day,y=I/(1-PROPORTION_B)),size=2,colour="red") +
# #         #  geom_line(aes(x=day,y=V),size=2,colour="darkred") +
# #         geom_line(data=resultB,aes(x=day,y=I/PROPORTION_B),size=2,colour="green") +
# #         #  geom_line(data=resultB,aes(x=day,y=V),size=2,colour="darkgreen") +
# #         ylab("Cases")+
# #         xlab("Day")+
# #         scale_y_continuous(
# #           name="Cases scaled to total population",
# #         ) +
# #         theme_classic(base_size=18)
# # )
# # 
# # 
# 
# PandemicGraph<-Overall %>% 
#   group_by(day) %>% 
#   summarise(IA=sum(IA)/MC_LOOPS,IB=sum(IB)/MC_LOOPS,I=sum(I)/MC_LOOPS,Reff=sum(mean)/MC_LOOPS,R=mean(R),RA=mean(RA),RB=mean(RB))
# #use sum/MC_LOOPS because otherwise it will ignore loops that finished early and have a falsely high mean
# 
# 
# print(paste0("Mean total infections:",max(PandemicGraph$R)))
# 
# 
# library(ggplot2)
# 
# print(ggplot(PandemicGraph)+
#         geom_line(aes(x=day,y=IA/(1-PROPORTION_B)),size=2,colour="darkred") +
#         #  geom_line(aes(x=day,y=V),size=2,colour="darkred") +
#         geom_line(aes(x=day,y=IB/PROPORTION_B),size=2,colour="darkgreen") +
#         geom_line(aes(x=day,y=R),size=2,colour="darkblue")+
#         #  geom_line(data=resultB,aes(x=day,y=V),size=2,colour="darkgreen") +
#         ylab("Cases")+
#         xlab("Day")+
#         scale_y_continuous(
#           name="Cases scaled to total population",
#         ) +
#         theme_classic(base_size=18)
# )


## Iterate over a variable - eg vaccine inequality


experiments<-2^seq(-2.5,1,length=15)
exp_out<-data.frame(val=double(),result=double(),resultA=double(),resultB=double())

cl <- makeCluster(8) 
registerDoParallel(cl)  

for (val in experiments){
  
  ## set variable under investigation here
  VACCINE_EQUALITY<-val
  
  ## and recalculate anything that needs recalculating
  VACCINATE_A_RATE<-OVERALL_VACCINE_RATE/(PROPORTION_B/VACCINE_EQUALITY+1-PROPORTION_B)*MODEL_N*(1-PROPORTION_B)
  VACCINATE_B_RATE<-OVERALL_VACCINE_RATE/(PROPORTION_B+VACCINE_EQUALITY-PROPORTION_B*VACCINE_EQUALITY)*MODEL_N*PROPORTION_B
  
  
  

  
  Overall<-foreach(i=1:MC_LOOPS,.combine='rbind') %dopar% {SingleRun(i)}
  Overall[is.na(Overall)]<-0
  

  
  #fill extra data as different runs will have stopped at different points
  Overall<-Overall %>% 
    complete(day,iter) %>% 
    group_by(iter) %>% 
    fill(everything())
  
  PandemicGraph<-Overall %>% 
    group_by(day) %>% 
    summarise(IA=sum(IA)/MC_LOOPS,IB=sum(IB)/MC_LOOPS,I=sum(I)/MC_LOOPS,Reff=sum(mean)/MC_LOOPS,R=mean(R),RA=mean(RA),RB=mean(RB))
  #use sum/MC_LOOPS because otherwise it will ignore loops that finished early and have a falsely high mean
  
  
  print(paste0("Mean total infections:",max(PandemicGraph$R)," A:",max(PandemicGraph$RA)," B:",max(PandemicGraph$RB)))
  
  exp_out<-rbind(exp_out,data.frame(val=val,result=max(PandemicGraph$R),resultA=max(PandemicGraph$RA),resultB=max(PandemicGraph$RB)))
  
}

stopCluster(cl)



print(
  
  ggplot(exp_out)+
    geom_smooth(aes(x=val,y=result))
  
  
)

