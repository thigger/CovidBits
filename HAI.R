library(readxl)
library(tidyverse)
library(runner)
library(plotly)

##Data from: https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-hospital-activity/

filename<-"Weekly-covid-admissions-and-beds-publication-210204-1.xlsx"

cnames<-paste0("cases",read_excel(filename,sheet="New hosp cases",n_max=0,skip=14) %>% names())
new_hosp_cases_wide<-na.omit(read_excel(filename,sheet="New hosp cases",skip=24,col_names=cnames))

cnames<-paste0("ads",read_excel(filename,sheet="Hosp ads from comm",n_max=0,skip=14) %>% names())
hosp_comm_ads_wide<-na.omit(read_excel(filename,sheet="Hosp ads from comm",skip=24,col_names=cnames))



#t1<-new_hosp_cases_wide[new_hosp_cases_wide$casesCode=="RYR",]

#t2<-hosp_comm_ads_wide[hosp_comm_ads_wide$adsCode=="RYR",]



overall_wide<-na.omit(merge(new_hosp_cases_wide,hosp_comm_ads_wide,by.x="casesCode",by.y="adsCode"))


#t3<-overall_wide[overall_wide$casesCode=="RYR",]

cnames<-names(overall_wide)
#overall_long<-reshape(overall_wide,direction="long",varying=c(grep("cases4",cnames),grep("ads4",cnames)),sep="",v.names=c("cases","ads"))
#misbehaves and gets data on wrong date


overall_long<-gather(overall_wide,variable,value,matches("\\d\\d",perl=TRUE)) %>% 
  separate("variable",into=c("var","date"),sep=-5) %>% 
  spread(var,value)

overall_long$hai<-overall_long$cases-overall_long$ads

overall_long$date<-strtoi(overall_long$date)


#t4<-overall_long[overall_long$casesCode=="RYR",]


sumDays<-30

test<-overall_long %>% 
  group_by(`casesCode`) %>% 
  arrange(`date`) %>% 
  mutate(
    sumNads = sum_run(`ads`,sumDays,idx=date),
    sumNcases = sum_run(`cases`,sumDays,idx=date),
    sumNhai = sum_run(`hai`,sumDays,idx=date),
  )
test$sumNads[is.na(test$sumNads)]<-0
test$sumNcases[is.na(test$sumNcases)]<-0
test$sumNhai[is.na(test$sumNhai)]<-0


#t5<-test[test$casesCode=="RYR",]


test$propNhai<-test$sumNhai/test$sumNcases
test$propNhai[is.nan(test$propNhai)]<-0
acutes<-test[test$`casesType 1 Acute?`=="Yes",]
acutes$date<-as.Date(acutes$date,origin="1899-12-30")



ggplot(acutes,aes(x=date,y=fct_reorder(casesName,propNhai),fill=propNhai))+
  geom_raster(show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="") +
  scale_x_date(name="Date", expand=c(0,0))

##this is messy!



acutes_grp<-acutes %>% 
  group_by(casesCode,casesName) %>% 
  summarize(totHAI=sum(hai),totCases=sum(cases))

acutes_grp$percHAI<-acutes_grp$totHAI/acutes_grp$totCases

#remove smaller hospitals
acutes_grp_lim<-acutes_grp[acutes_grp$totCases>300,]

lastdate<-max(acutes$date)

ggp<-ggplot(acutes_grp_lim,aes(y=fct_reorder(casesName,percHAI),x=percHAI,fill=percHAI))+
  geom_col()+
  scale_fill_distiller(palette="Spectral",guide=FALSE)+
  xlab("Percentage hospital acquired since August 2020")+
  scale_x_continuous(labels=scales::percent,expand=expansion(mult=c(0.00,0.02))) +
  ylab("Trust (and total cases)") +
  geom_text(aes(label=totCases),x=0.002,size=3,show.legend=FALSE,hjust=0) +
  ggtitle(paste0("Hospital Acquired COVID-19 to ",lastdate)) +
  theme(plot.title.position='plot',plot.title=element_text(hjust=0.5))


ggp %>% ggplotly(tooltip="y")

ggplot(acutes_grp_lim,aes(x=totCases,y=percHAI,colour=percHAI))+
  geom_point()+
  scale_color_gradient(low="blue",high="red",guide=FALSE)+
  xlab("Cases since August 2020 (log)")+
  scale_x_continuous(trans="log10")+
  scale_y_continuous(labels=scales::percent) +
  ylab("Percentage HAI") +
  #geom_text(aes(label=totCases,x=0)) +
  ggtitle("Hospital Acquired COVID-19")

cor.test(acutes_grp_lim$totCases,acutes_grp_lim$percHAI)


## Graph against 4-hr performance after a request by @DrLindaDykes
## 4hr performance from:
## using December only initially
## https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/ae-attendances-and-emergency-admissions-2020-21/

cnames<-paste0("aeperf",read_excel("December-2020-AE-by-provider-8c90a.xls",sheet="Provider Level Data",n_max=0,skip=15) %>% names())
cnames<-c(cnames,"aeperfSTP") #hidden column
hosp_aeperf_wide<-na.omit(read_excel("December-2020-AE-by-provider-8c90a.xls",sheet="Provider Level Data",skip=18,col_names=cnames))

test<-merge(acutes_grp_lim,hosp_aeperf_wide,by.x="casesCode",by.y="aeperfCode")

test$`aeperfPercentage in 4 hours or less (all)`

test$aeperfPerc4hr<-as.numeric(test$`aeperfPercentage in 4 hours or less (all)`)


model1<-lm(percHAI ~ aeperfPerc4hr,data=test)
summary(model1)

pred_vars <- predict(model1, interval="prediction")

test2<-test
test<-cbind(na.omit(test2),pred_vars)

ggplot(test,aes(x=`aeperfPerc4hr`,y=percHAI,colour=percHAI))+
  geom_point()+
  scale_color_gradient(low="blue",high="red",guide=FALSE)+
  xlab("December 2020 4-hour performance")+
  scale_y_continuous(labels=scales::percent) +
  ylab("Percentage HAI") +
  #geom_text(aes(label=totCases,x=0)) +
  ggtitle("Hospital Acquired COVID-19") +
  geom_smooth(method=lm,color="red",fill="grey",se=TRUE) +
  geom_line(aes(y=lwr), color = "red", linetype = "dashed") +
  geom_line(aes(y=upr), color = "red", linetype = "dashed")

cor.test(test$percHAI,test$aeperfPerc4hr,use="complete.obs")

