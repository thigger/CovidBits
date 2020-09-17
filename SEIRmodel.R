MODEL_N<-10000


people<-data.frame(
  id=seq.int(1,MODEL_N),
  connections=pmax(as.integer(rlnorm(MODEL_N,log(20),log(1.3)))+5,1),  ##logsd from 1 = no variation, to 1.5 = plenty (up to 100ish), 2 = loads (up to 300ish). NB adjusting this will affect mean
  state="S",
  sday=0
)

sspread<-1 # add superspreaders

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


connections<-as.data.frame(lapply(people,rep,people$connections))
connections$id2<-sample(people$id,nrow(connections),replace=TRUE)


people$state[people$id<10]<-"E" #initial exposure of 10 people

day<-0
result<-data.frame(
  day=0,
  S=MODEL_N,
  E=0,
  I=0,
  R=0
)

cnum<-nrow(connections)

prob<-0.004 #initial probability of transmission for each connection per day

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
  
  tab<-table(people$state)
  result<-rbind(result,data.frame(
    day=day,
    S=tab["S"],
    E=tab["E"],
    I=tab["I"],
    R=tab["R"]
  ))
  
  if(day==200) {break}
  
  
  ## Adjust transmission probabilities here for distancing etc
  if(is.na(tab["I"])) {tab["I"]<-0}
  if(tab["I"]>250) {prob<-0.002} ## social distancing/mitigations on
  if(tab["I"]<100) {prob<-0.004} ## back to normal!
  
}

library(ggplot2)
print(ggplot(result)+
        geom_line(aes(x=day,y=R),colour="red",size=2) +
        geom_line(aes(x=day,y=I),size=2) +
        ylab("Cases")+
        xlab("Day")+
        theme_classic(base_size=18)
)