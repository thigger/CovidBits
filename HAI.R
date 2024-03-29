library(readxl)
library(tidyverse)
library(runner)
library(plotly)



airb_trusts<-read.csv("20220505MB Airborne trusts.csv")

##Data from: https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-hospital-activity/

filename<-"Weekly-covid-admissions-and-beds-publication-210429-up-to-210406.xlsx"

cnames<-paste0("cases",read_excel(filename,sheet="New hosp cases",n_max=0,skip=14) %>% names())
new_hosp_cases_wide<-read_excel(filename,sheet="New hosp cases",skip=24,col_names=cnames)
new_hosp_cases_wide<-new_hosp_cases_wide[!is.na(new_hosp_cases_wide$casesCode),]


cnames<-paste0("ads",read_excel(filename,sheet="Hosp ads from comm",n_max=0,skip=14) %>% names())
hosp_comm_ads_wide<-read_excel(filename,sheet="Hosp ads from comm",skip=24,col_names=cnames)
hosp_comm_ads_wide<-hosp_comm_ads_wide[!is.na(hosp_comm_ads_wide$adsCode),]


cnames<-paste0("noncovid",read_excel(filename,sheet="Adult G&A Bed Occupied NonCOVID",n_max=0,skip=14) %>% names())
hosp_noncovid_wide<-read_excel(filename,sheet="Adult G&A Bed Occupied NonCOVID",skip=24,col_names=cnames)
hosp_noncovid_wide<-hosp_noncovid_wide[!is.na(hosp_noncovid_wide$noncovidCode),]




filename<-"Weekly-covid-admissions-and-beds-publication-211209-210407-210930.xlsx"
cnames<-paste0("cases",read_excel(filename,sheet="New hosp cases",n_max=0,skip=14) %>% names())
new_hosp_cases_wide2<-read_excel(filename,sheet="New hosp cases",skip=24,col_names=cnames)
new_hosp_cases_wide2<-new_hosp_cases_wide2[!is.na(new_hosp_cases_wide2$casesCode),]


cnames<-paste0("noncovid",read_excel(filename,sheet="Adult G&A Bed Occupied NonCOVID",n_max=0,skip=14) %>% names())
hosp_noncovid_wide2<-read_excel(filename,sheet="Adult G&A Bed Occupied NonCOVID",skip=24,col_names=cnames)
hosp_noncovid_wide2<-hosp_noncovid_wide2[!is.na(hosp_noncovid_wide2$noncovidCode),]



cnames<-paste0("ads",read_excel(filename,sheet="Hosp ads from comm",n_max=0,skip=14) %>% names())
hosp_comm_ads_wide2<-read_excel(filename,sheet="Hosp ads from comm",skip=24,col_names=cnames)
hosp_comm_ads_wide2<-hosp_comm_ads_wide2[!is.na(hosp_comm_ads_wide2$adsCode),]

filename<-"Weekly-covid-admissions-and-beds-publication-220512_211001to220331-1.xlsx"


cnames<-paste0("cases",read_excel(filename,sheet="New hosp cases",n_max=0,skip=14) %>% names())
new_hosp_cases_wide3<-read_excel(filename,sheet="New hosp cases",skip=24,col_names=cnames)
new_hosp_cases_wide3<-new_hosp_cases_wide3[!is.na(new_hosp_cases_wide3$casesCode),]

cnames<-paste0("ads",read_excel(filename,sheet="Hosp ads from comm",n_max=0,skip=14) %>% names())
hosp_comm_ads_wide3<-read_excel(filename,sheet="Hosp ads from comm",skip=24,col_names=cnames)
hosp_comm_ads_wide3<-hosp_comm_ads_wide3[!is.na(hosp_comm_ads_wide3$adsCode),]


cnames<-paste0("noncovid",read_excel(filename,sheet="Adult G&A Bed Occupied NonCOVID",n_max=0,skip=14) %>% names())
hosp_noncovid_wide3<-read_excel(filename,sheet="Adult G&A Bed Occupied NonCOVID",skip=24,col_names=cnames)
hosp_noncovid_wide3<-hosp_noncovid_wide3[!is.na(hosp_noncovid_wide3$noncovidCode),]


filename<-"Weekly-covid-admissions-and-beds-publication-221110.xlsx"

##nb methodology change in this file (for 221006 release)
## NHSE definition states that the change is for the whole of the Hosp ads from comm table, but I have the distinct feeling it's from 27/9/22

cnames<-paste0("cases",read_excel(filename,sheet="Hosp ads & diag",n_max=0,skip=14) %>% names())
#new_hosp_cases_wide4<-na.omit(read_excel(filename,sheet="Hosp ads & diag",skip=24,col_names=cnames))
## Removed na.omit everywhere
new_hosp_cases_neww4<-read_excel(filename,sheet="Hosp ads & diag",skip=24,col_names=cnames)
new_hosp_cases_neww4<-new_hosp_cases_neww4[!is.na(new_hosp_cases_neww4$casesCode),]

cnames<-paste0("cases",read_excel(filename,sheet="New hosp cases",n_max=0,skip=14) %>% names())
new_hosp_cases_oldw4<-read_excel(filename,sheet="New hosp cases",skip=24,col_names=cnames)
new_hosp_cases_oldw4<-new_hosp_cases_oldw4[!is.na(new_hosp_cases_oldw4$casesCode),]



date_methodology_change<-44831 # first date of new methodology

newcasesold4_long<-gather(new_hosp_cases_oldw4,variable,value,matches("\\d\\d",perl=TRUE)) %>% 
  separate("variable",into=c("var","date"),sep=-5) %>% 
  spread(var,value) %>% 
  filter(date<date_methodology_change)

newcases4_long<-gather(new_hosp_cases_neww4,variable,value,matches("\\d\\d",perl=TRUE)) %>% 
  separate("variable",into=c("var","date"),sep=-5) %>% 
  spread(var,value) %>% 
  filter(date>=date_methodology_change)

combined<-rbind(newcasesold4_long,newcases4_long) %>% 
  mutate(date=paste0("cases",date))

new_hosp_cases_wide4<-spread(combined,date,cases)



cnames<-paste0("ads",read_excel(filename,sheet="Hosp ads from comm",n_max=0,skip=14) %>% names())
hosp_comm_ads_wide4<-read_excel(filename,sheet="Hosp ads from comm",skip=24,col_names=cnames)
hosp_comm_ads_wide4<-hosp_comm_ads_wide4[!is.na(hosp_comm_ads_wide4$adsCode),]


cnames<-paste0("noncovid",read_excel(filename,sheet="Adult G&A Bed Occupied NonCOVID",n_max=0,skip=14) %>% names())
hosp_noncovid_wide4<-read_excel(filename,sheet="Adult G&A Bed Occupied NonCOVID",skip=24,col_names=cnames)
hosp_noncovid_wide4<-hosp_noncovid_wide4[!is.na(hosp_noncovid_wide4$noncovidCode),]



new_hosp_cases_wide<-merge(new_hosp_cases_wide,new_hosp_cases_wide2,by="casesCode",all.y=TRUE)
new_hosp_cases_wide<-merge(new_hosp_cases_wide,new_hosp_cases_wide3,by="casesCode",all.y=TRUE)
new_hosp_cases_wide<-merge(new_hosp_cases_wide,new_hosp_cases_wide4,by="casesCode",all.y=TRUE)

hosp_comm_ads_wide<-merge(hosp_comm_ads_wide,hosp_comm_ads_wide2,by="adsCode",all=TRUE)
hosp_comm_ads_wide<-merge(hosp_comm_ads_wide,hosp_comm_ads_wide3,by="adsCode",all=TRUE)
hosp_comm_ads_wide<-merge(hosp_comm_ads_wide,hosp_comm_ads_wide4,by="adsCode",all=TRUE)

hosp_noncovid_wide<-merge(hosp_noncovid_wide,hosp_noncovid_wide2,by="noncovidCode",all=TRUE)
hosp_noncovid_wide<-merge(hosp_noncovid_wide,hosp_noncovid_wide3,by="noncovidCode",all=TRUE)
hosp_noncovid_wide<-merge(hosp_noncovid_wide,hosp_noncovid_wide4,by="noncovidCode",all=TRUE)

#t1<-new_hosp_cases_wide[new_hosp_cases_wide$casesCode=="RYR",]

#t2<-hosp_comm_ads_wide[hosp_comm_ads_wide$adsCode=="RYR",]


#overall_wide<-na.omit(merge(new_hosp_cases_wide,hosp_comm_ads_wide,by.x="casesCode",by.y="adsCode",all=TRUE))

overall_wide<-merge(new_hosp_cases_wide,hosp_comm_ads_wide,by.x="casesCode",by.y="adsCode",all=TRUE)

overall_wide<-overall_wide[!duplicated(colnames(overall_wide))] %>% 
  rename(casesName=casesName.x)


overall_incnoncovid_wide<-merge(overall_wide,hosp_noncovid_wide,by.x="casesCode",by.y="noncovidCode",all=TRUE)

#t3<-overall_wide[overall_wide$casesCode=="RYR",]

cnames<-names(overall_wide)
#overall_long<-reshape(overall_wide,direction="long",varying=c(grep("cases4",cnames),grep("ads4",cnames)),sep="",v.names=c("cases","ads"))
#misbehaves and gets data on wrong date


overall_long<-gather(overall_wide,variable,value,matches("\\d\\d",perl=TRUE)) %>% 
  separate("variable",into=c("var","date"),sep=-5) %>% 
  spread(var,value)

overall_long$hai<-overall_long$cases-overall_long$ads
overall_long<-overall_long %>% replace_na(list(cases=0,ads=0,hai=0,`casesType 1 Acute?.x`="No"))
#overall_long<-overall_long %>% replace_na(list(`casesType 1 Acute?.x`="No"))



overall_long$date<-strtoi(overall_long$date)



overall_incnoncovid_long<-gather(overall_incnoncovid_wide,variable,value,matches("\\d\\d",perl=TRUE)) %>% 
  separate("variable",into=c("var","date"),sep=-5) %>% 
  spread(var,value)

overall_incnoncovid_long$hai<-overall_incnoncovid_long$cases-overall_incnoncovid_long$ads
overall_incnoncovid_long<-overall_incnoncovid_long %>% replace_na(list(cases=0,ads=0,hai=0,`casesType 1 Acute?.x`="No")) ##debatable what to do about noncovid. Zero is not likely!



overall_incnoncovid_long$date<-strtoi(overall_incnoncovid_long$date)


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
acutes<-test[test$`casesType 1 Acute?.x`=="Yes",]
acutes$date<-as.Date(acutes$date,origin="1899-12-30")



## calculate 7-day risk for noncovid patients


tt<-overall_incnoncovid_long[overall_incnoncovid_long$casesCode=="RNS",]
tt$date<-as.Date(tt$date,origin="1899-12-30")


sumDays<-28

test<-overall_incnoncovid_long %>% 
  group_by(`casesCode`) %>% 
  arrange(`date`) %>% 
  mutate(
    sumNads = sum_run(`ads`,sumDays,idx=date),
    sumNcases = sum_run(`cases`,sumDays,idx=date),
    sumNhai = sum_run(`hai`,sumDays,idx=date),
    avNnoncovid = sum_run(`noncovid`,sumDays,idx=date)/sumDays
  )
test$sumNads[is.na(test$sumNads)]<-0
test$sumNcases[is.na(test$sumNcases)]<-0
test$sumNhai[is.na(test$sumNhai)]<-0
test$risk7days<-test$sumNhai/test$avNnoncovid/(sumDays/7)
test<-test[!is.na(test$risk7days),]
test<-test[test$date<=max(test[!is.na(test$hai),'date']),]
test<-test[test$`casesType 1 Acute?.x`=="Yes",]
test<-test[test$avNnoncovid>20,]

acutesRiskLast28days<-test[test$date==max(test$date,na.rm=TRUE),]

acutesRiskPrev28days<-test[test$date==max(test$date,na.rm=TRUE)-28,]

lastdate<-as.Date(max(test$date),origin="1899-12-30")

ggplot(acutesRiskLast28days,aes(y=fct_reorder(casesName,risk7days),x=risk7days,fill=risk7days))+
  geom_col()+
  scale_fill_distiller(palette="Spectral",guide=FALSE)+
  xlab(paste0("7-day percentage developing COVID-19"))+
  scale_x_continuous(labels=scales::percent,expand=expansion(mult=c(0.00,0.02))) +
  ylab("Trust") +
  #geom_text(aes(label=totCases),x=0.002,size=3,show.legend=FALSE,hjust=0) +
  ggtitle(paste0("Estimated 7-day risk of developing Hospital Acquired COVID-19 over last ",sumDays," days (to ",lastdate,")")) +
  theme(plot.title.position='plot',plot.title=element_text(hjust=0.5))










ggplot(acutesRiskPrev28days,aes(y=fct_reorder(casesName,risk7days),x=risk7days,fill=risk7days))+
  geom_col()+
  scale_fill_distiller(palette="Spectral",guide=FALSE)+
  xlab(paste0("7-day percentage catching COVID-19"))+
  scale_x_continuous(labels=scales::percent,expand=expansion(mult=c(0.00,0.02))) +
  ylab("Trust") +
  #geom_text(aes(label=totCases),x=0.002,size=3,show.legend=FALSE,hjust=0) +
  ggtitle(paste0("7-day risk of Hospital Acquired COVID-19 previous ",sumDays," days (to ",lastdate-28,")")) +
  theme(plot.title.position='plot',plot.title=element_text(hjust=0.5))


acutesRisk28daysRatio<-merge(acutesRiskLast28days,acutesRiskPrev28days,by="casesCode")
acutesRisk28daysRatio$ratio<-acutesRisk28daysRatio$risk7days.x/acutesRisk28daysRatio$risk7days.y


ggplot(acutesRisk28daysRatio,aes(y=fct_reorder(casesName.x,ratio),x=ratio,fill=ratio))+
  geom_col()+
  scale_fill_distiller(palette="Spectral",guide=FALSE)+
  xlab(paste0("HAI risk ratio to previous 28 days"))+
  scale_x_continuous(labels=scales::percent,expand=expansion(mult=c(0.00,0.02))) +
  ylab("Trust") +
  #geom_text(aes(label=totCases),x=0.002,size=3,show.legend=FALSE,hjust=0) +
  ggtitle(paste0("Ratio of last ",sumDays," days to previous ",sumDays," days (to ",lastdate,")")) +
  theme(plot.title.position='plot',plot.title=element_text(hjust=0.5))+
  scale_x_continuous(trans='log2')




#### AirDDS version of above ####


sumDays<-28

test<-overall_incnoncovid_long %>% 
  group_by(`casesCode`) %>% 
  arrange(`date`) %>% 
  mutate(
    sumNads = sum_run(`ads`,sumDays,idx=date),
    sumNcases = sum_run(`cases`,sumDays,idx=date),
    sumNhai = sum_run(`hai`,sumDays,idx=date),
    avNnoncovid = sum_run(`noncovid`,sumDays,idx=date)/sumDays
  )
test$sumNads[is.na(test$sumNads)]<-0
test$sumNcases[is.na(test$sumNcases)]<-0
test$sumNhai[is.na(test$sumNhai)]<-0
test$risk7days<-test$sumNhai/test$avNnoncovid/(sumDays/7)
test<-test[!is.na(test$risk7days),]
test<-test[test$date<=max(test[!is.na(test$hai),'date']),]
test<-test[test$`casesType 1 Acute?.x`=="Yes",]
test<-test[test$avNnoncovid>20,]

acutesRiskLastNdays<-test[test$date==max(test$date,na.rm=TRUE),]



acutesRiskLastNdays$arrows<-""
acutesRiskLastNdays$arrows[acutesRiskLastNdays$casesCode %in% airb_trusts$Code]<-"\u2190"

ggplot(acutesRiskLastNdays,aes(y=fct_reorder(casesName,risk7days),x=risk7days,fill=risk7days))+
  geom_col()+
  scale_fill_distiller(palette="Spectral",guide=FALSE)+
  xlab(paste0("7-day percentage developing COVID-19"))+
  scale_x_continuous(labels=scales::percent,expand=expansion(mult=c(0.00,0.02))) +
  ylab("Trust") +
  geom_text(aes(label=arrows),x=0.0002,size=10,show.legend=FALSE,hjust=0,nudge_y=0.45)+
  ggtitle(paste0("7-day percentage developing Hospital Acquired COVID-19 over last ",sumDays," days (to ",lastdate,")")) +
  theme(plot.title.position='plot',plot.title=element_text(hjust=0.5))









## acutes plot

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
acutes_grp_lim<-acutes_grp[acutes_grp$totCases>200,]

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


## Graph against bed occupancy after a request by @elinlowri
## Beds from:
## (September 2020 figures)
## https://www.england.nhs.uk/statistics/statistical-work-areas/bed-availability-and-occupancy/bed-data-overnight/
cnames<-paste0("beds",read_excel("Beds-Open-Overnight-Web_File-Final-DE5WC.xlsx",sheet="NHS Trust by Sector",n_max=0,skip=14) %>% names())
#cnames<-c(cnames,"aeperfSTP") #hidden column
hosp_beds_wide<-read_excel("Beds-Open-Overnight-Web_File-Final-DE5WC.xlsx",sheet="NHS Trust by Sector",skip=17,col_names=cnames)

test<-merge(acutes_grp_lim,hosp_beds_wide,by.x="casesCode",by.y="bedsOrg Code")


test$bedsAcGeneral<-as.numeric(test$`bedsGeneral & Acute...7`)
test$totCasesPerBed<-test$totCases/test$bedsAcGeneral


model1<-lm(percHAI ~ totCasesPerBed,data=test)
summary(model1)

pred_vars <- predict(model1, interval="prediction")

test2<-test
test<-cbind(test2,pred_vars)


ggplot(test,aes(x=`totCasesPerBed`,y=percHAI,colour=percHAI))+
  geom_point()+
  scale_color_gradient(low="blue",high="red",guide=FALSE)+
  xlab("Total Cases Per Acute Bed")+
  scale_y_continuous(labels=scales::percent) +
  ylab("Percentage HAI") +
  #geom_text(aes(label=totCases,x=0)) +
  ggtitle("Hospital Acquired COVID-19") +
  geom_smooth(method=lm,color="red",fill="grey",se=TRUE) +
  geom_line(aes(y=lwr), color = "red", linetype = "dashed") +
  geom_line(aes(y=upr), color = "red", linetype = "dashed")


## Compare with Scottish data - 28 days to Jan 17 960 new HAI cases out of 1918?
overall_long$date<-as.Date(overall_long$date,origin="1899-12-30")
tcases<-sum(overall_long$cases)
thai<-sum(overall_long$hai)
print(paste0("Total HAI: ",thai,"/",tcases," (",thai/tcases,"%)"))


pc<-data.frame(group=c("Hospital Acquired","Community Acquired"),value=c(thai,tcases-thai))

ggplot(pc,aes(x="",y=value,fill=group))+geom_bar(width=1,stat="identity")+coord_polar("y",start=0)+
  #scale_fill_manual(values=c("darkblue","darkred"))+
  scale_fill_manual(values=c("skyblue4","tomato4"))+
  theme_minimal()+
  xlab("")+
  ylab("")


#Dates are inclusive
#start_date<-as.Date("2020-12-20")
start_date<-as.Date("2020-08-01")
fin_date<-as.Date("2021-11-01")

tlim_overall_long<-overall_long[overall_long$date<=fin_date & overall_long$date>=start_date,]
tcases<-sum(tlim_overall_long$cases)
thai<-sum(tlim_overall_long$hai)
print(paste0("Specified dates - total HAI: ",thai,"/",tcases," (",thai/tcases,"%)"))

maxdate<-max(acutes$date)

n_days<-28

tlim_overall_long<-overall_long[overall_long$date<=maxdate & overall_long$date>=(maxdate-n_days),]
tcases<-sum(tlim_overall_long$cases)
thai<-sum(tlim_overall_long$hai)
print(paste0("Most recent ",n_days," days - total HAI: ",thai,"/",tcases," (",thai/tcases,"%)"))



#### Main charts here ####
## Charts for 28 or n days

n_days<-28

maxdate<-max(acutes$date)

#maxdate<-as.Date("2020-09-30")

acutes_28d<-acutes[acutes$date>=maxdate-n_days & acutes$date<=maxdate,]

#hull<-acutes_28d[acutes_28d$casesCode=="RWA",]
#write.csv(hull,file="20220109 Hull.csv")

acutes_28d_grp<-acutes_28d %>% 
  group_by(casesCode,casesName) %>% 
  summarize(totHAI=sum(hai),totCases=sum(cases))

acutes_28d_grp$percHAI<-acutes_28d_grp$totHAI/acutes_28d_grp$totCases

#remove smaller hospitals plus childrens hospitals
acutes_28d_grp_lim<-acutes_28d_grp[acutes_28d_grp$casesCode!="RCU"&acutes_28d_grp$casesCode!="RQ3"&acutes_28d_grp$casesCode!="RBS",]
acutes_28d_grp_lim<-acutes_28d_grp_lim[acutes_28d_grp_lim$totCases>0,]

lastdate<-max(acutes_28d$date)
firstdate<-min(acutes_28d$date)

ggplot(acutes_28d_grp_lim,aes(y=fct_reorder(casesName,percHAI),x=percHAI,fill=percHAI))+
  geom_col()+
  scale_fill_distiller(palette="Spectral",guide=FALSE)+
  xlab(paste0("Percentage hospital acquired since ",firstdate))+
  scale_x_continuous(labels=scales::percent,expand=expansion(mult=c(0.00,0.02))) +
  ylab("Trust (and total cases)") +
  geom_text(aes(label=paste0(totHAI," / ",totCases)),x=0.002,size=3,show.legend=FALSE,hjust=0) +
  ggtitle(paste0("Hospital Acquired COVID-19 last ",n_days," days (to ",lastdate,")")) +
  theme(plot.title.position='plot',plot.title=element_text(hjust=0.5))


#### AirDDS charts ####


#arrows - https://unicode-table.com/en/sets/arrow-symbols/#left-arrows
n_days<-28

maxdate<-max(acutes$date)

#maxdate<-as.Date("2020-09-30")

acutes_28d<-acutes[acutes$date>=maxdate-n_days & acutes$date<=maxdate,]
acutes_28d_grp<-acutes_28d %>% 
  group_by(casesCode,casesName) %>% 
  summarize(totHAI=sum(hai),totCases=sum(cases))

acutes_28d_grp$percHAI<-acutes_28d_grp$totHAI/acutes_28d_grp$totCases

#remove smaller hospitals plus childrens hospitals
acutes_28d_grp_lim<-acutes_28d_grp[acutes_28d_grp$casesCode!="RCU"&acutes_28d_grp$casesCode!="RQ3"&acutes_28d_grp$casesCode!="RBS",]
acutes_28d_grp_lim<-acutes_28d_grp_lim[acutes_28d_grp_lim$totCases>0,]


acutes_28d_grp_lim$arrows<-""
acutes_28d_grp_lim$arrows[acutes_28d_grp_lim$casesCode %in% airb_trusts$Code]<-"\u2190"

acutes_28d_grp_lim$shortName<-str_replace_all(acutes_28d_grp_lim$casesName,"(NHS|HEALTHCARE|TRUST|FOUNDATION|HOSPITALS|HOSPITAL|TEACHING|[:space:]OF[:space:]|THE[:space:]|^[:space:]|[:space]$)","")



lastdate<-max(acutes_28d$date)
firstdate<-min(acutes_28d$date)

ggplot(acutes_28d_grp_lim,aes(y=fct_reorder(casesName,percHAI),x=percHAI,fill=percHAI))+
    geom_col()+
    scale_fill_distiller(palette="Spectral",guide=FALSE)+
    xlab(paste0("Percentage hospital acquired since ",firstdate))+
    scale_x_continuous(labels=scales::percent,expand=expansion(mult=c(0.00,0.02))) +
    ylab("Trust (and total cases)") +
    #geom_text(aes(label=totCases),x=0.002,size=3,show.legend=FALSE,hjust=0) +
    geom_text(aes(label=arrows),x=0.002,size=10,show.legend=FALSE,hjust=0,nudge_y=0.45)+
    ggtitle(paste0("Hospital Acquired COVID-19 last ",n_days," days (to ",lastdate,")")) +
    theme(plot.title.position='plot',plot.title=element_text(hjust=0.5))


#### By Wave ####

acutes_alphawave<-acutes[acutes$date<as.Date("2021-05-01"),]
acutes_deltawave<-acutes[acutes$date>=as.Date("2021-05-01"),]

acutes_alpha_grp<-acutes_alphawave %>% 
  group_by(casesCode,casesName) %>% 
  summarize(totHAI=sum(hai),totCases=sum(cases))

print(paste0("Alpha Wave: ",sum(acutes_alpha_grp$totHAI)," / ",sum(acutes_alpha_grp$totCases)," = ",sum(acutes_alpha_grp$totHAI)/sum(acutes_alpha_grp$totCases)*100,"%"))


acutes_delta_grp<-acutes_deltawave %>% 
  group_by(casesCode,casesName) %>% 
  summarize(totHAI=sum(hai),totCases=sum(cases))

print(paste0("Delta Wave: ",sum(acutes_delta_grp$totHAI)," / ",sum(acutes_delta_grp$totCases)," = ",sum(acutes_delta_grp$totHAI)/sum(acutes_delta_grp$totCases)*100,"%"))



acutes_alpha_grp$percHAI<-acutes_alpha_grp$totHAI/acutes_alpha_grp$totCases
acutes_delta_grp$percHAI<-acutes_delta_grp$totHAI/acutes_delta_grp$totCases

acutes_alpha_grp<-acutes_alpha_grp[acutes_alpha_grp$totCases>200,]
acutes_delta_grp<-acutes_delta_grp[acutes_delta_grp$totCases>140,]

colour_lim_max<-max(acutes_alpha_grp$percHAI,acutes_delta_grp$percHAI)

ggplot(acutes_alpha_grp,aes(y=fct_reorder(casesName,percHAI),x=percHAI,fill=percHAI))+
  geom_col()+
  scale_fill_distiller(palette="Spectral",guide=FALSE,limits=range(0,colour_lim_max))+
  xlab("Percentage hospital acquired")+
  scale_x_continuous(labels=scales::percent,expand=expansion(mult=c(0.00,0.02)),limits=c(0,colour_lim_max)) +
  #scale_x_continuous(labels=scales::percent,limits=c(0,colour_lim_max)) +
  ylab("Trust (and total cases)") +
  geom_text(aes(label=totCases),x=0.002,size=3,show.legend=FALSE,hjust=0) +
  ggtitle("Hospital Acquired COVID-19 Alpha Wave (Aug 2020 - May 2021)") +
  theme(plot.title.position='plot',plot.title=element_text(hjust=0.5))

ggplot(acutes_delta_grp,aes(y=fct_reorder(casesName,percHAI),x=percHAI,fill=percHAI))+
  geom_col()+
  scale_fill_distiller(palette="Spectral",guide=FALSE,limits=range(0,colour_lim_max))+
  xlab("Percentage hospital acquired")+
  scale_x_continuous(labels=scales::percent,expand=expansion(mult=c(0.00,0.02)),limits=c(0,colour_lim_max)) +
  ylab("Trust (and total cases)") +
  geom_text(aes(label=totCases),x=0.002,size=3,show.legend=FALSE,hjust=0) +
  ggtitle(paste0("Hospital Acquired COVID-19 Delta Wave (May 2021 to ",max(acutes_deltawave$date),")")) +
  theme(plot.title.position='plot',plot.title=element_text(hjust=0.5))




## Slopegraphs
library("CGPfunctions")
source("newggrankslopegraph.R")

#acutes_panel<-acutes %>% group_by(casesCode,casesName,dr=cut(date,2,ordered_result=TRUE)) %>% summarize(totHAI=sum(hai),totCases=sum(cases))
acutes_panel<-acutes %>% group_by(casesCode,casesName,drB=(date<as.Date("2021-01-01"))) %>% summarize(totHAI=sum(hai),totCases=sum(cases))

acutes_panel$dr<-factor(c('2021','2020')[acutes_panel$drB+1],ordered=TRUE)

acutes_panel$percHAI<-acutes_panel$totHAI/acutes_panel$totCases

# remove trusts where cases in either period below 200
acutes_panel<-acutes_panel %>% 
  group_by(casesCode) %>% 
    filter(min(totCases)>=122) # picked to keep Birmingham W&C out!

acutes_panel<-group_by(acutes_panel,dr) %>% 
  mutate(rank=order(order(percHAI,decreasing=TRUE)))

acutes_panel$blank<-""
acutes_panel$shortName<-str_replace_all(acutes_panel$casesName,"(NHS|HEALTHCARE|TRUST|FOUNDATION|HOSPITALS|HOSPITAL|TEACHING|[:space:]OF[:space:]|THE[:space:]|^[:space:]|[:space]$)","")



acutes_panel<-ungroup(acutes_panel)


pal<-colorRamp(c("darkred","gray","darkgreen"))

custom_colors <- tidyr::pivot_wider(acutes_panel, 
                                   id_cols = shortName, 
                                   names_from = dr, 
                                   values_from = rank) %>% 
  mutate(difference = `2021` - `2020`) %>%
#  mutate(trend = case_when(
#    difference >= 20 ~ "green",
#    difference <= -20 ~ "red",
#    TRUE ~ "gray"
# )
#  ) %>%
  mutate(trend = rgb(pal(  (difference-min(difference))  / (max(difference)-min(difference)) ),maxColorValue=255)) %>% 
  #mutate(trend="gray") %>% 
  dplyr::select(shortName, trend) %>%
  tibble::deframe()


test<-acutes_panel %>% dplyr::filter(dr == min(dr))

#acutes_panel$drchar<-as.character(acutes_panel$dr)

#ranks
bigchart<-newggrankslopegraph(
  acutes_panel,
  dr,
  rank,
  shortName,
  Data.label=blank,
  YTextSize = 3,
  LineColor=custom_colors,
  LineThickness = 1.5,
  Title = "Ranked performance for Hospital-Acquired COVID-19 since August 2020",
  SubTitle = paste0("2020 figures vs 2021 - data to ",maxdate),
  Caption = "Top of chart = lowest %HAI",
  CaptionTextSize = 12
)

print(bigchart)

#%HAI
bigchart<-newggrankslopegraph(
  acutes_panel,
  dr,
  percHAI,
  shortName,
  Data.label=blank,
  YTextSize = 3,
  LineColor=custom_colors,
  LineThickness = 1.5,
  Title = "Performance for Hospital-Acquired COVID-19 since August 2020",
  SubTitle = "2020 figures vs 2021",
  Caption = "Top of chart = highest %HAI",
  CaptionTextSize = 12
)
print(bigchart)



#logit %HAI - looks terrible!
acutes_panel$logitpercHAI<-log(acutes_panel$percHAI/(1-acutes_panel$percHAI))
bigchart<-newggrankslopegraph(
  acutes_panel,
  dr,
  logitpercHAI,
  shortName,
  Data.label=blank,
  YTextSize = 3,
  LineColor=custom_colors,
  LineThickness = 1.5,
  Title = "Logit Transformed performance for Hospital-Acquired COVID-19 since August 2020",
  SubTitle = "2020 figures vs 2021",
  Caption = "Top of chart = highest %HAI",
  CaptionTextSize = 12
)
print(bigchart)



## Medians for DT
median(acutes_panel[acutes_panel$dr=="2020",]$percHAI)
median(acutes_panel[acutes_panel$dr=="2021",]$percHAI)
mean(acutes_panel[acutes_panel$dr=="2020",]$percHAI)
mean(acutes_panel[acutes_panel$dr=="2021",]$percHAI)

#ggsave("bigplot.png",plot=bigchart,device=png(),width=6,height=12,units="in",dpi=300)


# library("slopegraph")
# 
# ggslopegraph2(
#   acutes_panel,
#   dr,
#   rank,
#   casesName
# )


## Funnel plot as suggested by @I_M_Stratton

library("funnelR")

fframe<-data.frame(n=acutes_grp_lim$totHAI,d=acutes_grp_lim$totCases)
fpdata<-fundata(fframe)

fplot<-funplot(fframe,fpdata)
print(fplot)



## Above totally overdispersed, so trying with @ChrisMainey's FunnelPlotR which allows a couple of correction methods

library("FunnelPlotR")

model<-glm(totHAI ~ totCases,data=acutes_28d_grp_lim)
summary(model)
acutes_28d_grp_lim$prds<-predict(model,type="response")

meanPerc<-sum(acutes_28d_grp_lim$totHAI)/sum(acutes_28d_grp_lim$totCases)*100
meanPerc<-1

#acutes_grp_lim$prds<-acutes_grp_lim$totCases*sum(acutes_grp_lim$totHAI)/sum(acutes_grp_lim$totCases)

fplot<-funnel_plot(numerator=acutes_28d_grp_lim$totHAI,denominator=acutes_28d_grp_lim$prds,group=acutes_28d_grp_lim$casesName,title="Funnel plot of Hospital Acquired COVID-19", data_type="SR",limit=99,Poisson_limits = FALSE,sr_method="SHMI",OD_adjust = TRUE,label_outliers=TRUE,multiplier=meanPerc,y_label="Ratio to average HAI percentage",x_label="Expected HAI")
print(fplot)









## 28 day slopegraphs

#acutes_28d_panel<-acutes %>% group_by(casesCode,casesName,dr=cut(date,2,ordered_result=TRUE)) %>% summarize(totHAI=sum(hai),totCases=sum(cases))

maxdate<-max(acutes$date)
mindate<-maxdate-56

acutes_28d_panel<-acutes %>% filter(date>=maxdate-56) %>% group_by(casesCode,casesName,drB=(date>=(maxdate-28))) %>% summarize(totHAI=sum(hai),totCases=sum(cases))

acutes_28d_panel$dr<-factor(c('Previous 28d','Most recent 28d')[acutes_28d_panel$drB+1],levels=c('Previous 28d','Most recent 28d'),ordered=TRUE)

acutes_28d_panel$percHAI<-acutes_28d_panel$totHAI/acutes_28d_panel$totCases

# remove trusts where cases in either period below 20
acutes_28d_panel<-acutes_28d_panel %>% 
  group_by(casesCode) %>% 
  filter(min(totCases)>=30)

acutes_28d_panel<-group_by(acutes_28d_panel,dr) %>% 
  mutate(rank=order(order(percHAI,decreasing=TRUE)))

acutes_28d_panel$blank<-""
acutes_28d_panel$shortName<-str_replace_all(acutes_28d_panel$casesName,"(NHS|HEALTHCARE|TRUST|FOUNDATION|HOSPITALS|HOSPITAL|TEACHING|[:space:]OF[:space:]|THE[:space:]|^[:space:]|[:space]$)","")



acutes_28d_panel<-ungroup(acutes_28d_panel)


pal<-colorRamp(c("darkred","gray","darkgreen"))

custom_colors <- tidyr::pivot_wider(acutes_28d_panel, 
                                    id_cols = shortName, 
                                    names_from = dr, 
                                    values_from = rank) %>% 
  mutate(difference = `Most recent 28d`-`Previous 28d`) %>%
  #  mutate(trend = case_when(
  #    difference >= 20 ~ "green",
  #    difference <= -20 ~ "red",
  #    TRUE ~ "gray"
  # )
  #  ) %>%
  mutate(trend = rgb(pal(  (difference-min(difference))  / (max(difference)-min(difference)) ),maxColorValue=255)) %>% 
  #mutate(trend="gray") %>% 
  dplyr::select(shortName, trend) %>%
  tibble::deframe()


test<-acutes_28d_panel %>% dplyr::filter(dr == min(dr))

#acutes_28d_panel$drchar<-as.character(acutes_28d_panel$dr)

#ranks
bigchart<-newggrankslopegraph(
  acutes_28d_panel,
  dr,
  rank,
  shortName,
  Data.label=blank,
  YTextSize = 3,
  LineColor=custom_colors,
  LineThickness = 1.5,
  Title = "Ranked performance for Hospital-Acquired COVID-19",
  SubTitle = paste0("Most recent 28d figures vs previous 28d - ",mindate," to ",maxdate),
  Caption = "Top of chart = lowest %HAI",
  CaptionTextSize = 12
)

print(bigchart)



##
## longest run

trusts<-unique(acutes$casesCode)
d2<-data.frame(matrix(NA,ncol=2,nrow=length(trusts)))
names(d2)<-c("trustcode","longest_zero")
d2$trustcode<-trusts
for (i in trusts) {
  d2$trustName[d2$trustcode==i]<-acutes$casesName[acutes$casesCode==i][1]
  if (0 %in% acutes$hai[acutes$casesCode==i]) {
    run<-rle(acutes$hai[acutes$casesCode==i])
    d2$longest_zero[d2$trustcode==i]<-max(run$length[run$values==0])
  } else{
    d2$longest_zero[d2$trustcode==i]<-0
  }
}


## chart by month
library(lubridate)
acutes$month<-month(acutes$date)
acutes$year<-year(acutes$date)

acutes_TS<-acutes[acutes$casesCode!="RCU"&acutes$casesCode!="RQ3"&acutes$casesCode!="RBS",]
#acutes_28d_grp_lim<-acutes_28d_grp_lim[acutes_28d_grp_lim$totCases>0,]



acutes_TS_grp<-acutes_TS %>% 
  group_by(casesCode,casesName,year,month) %>% 
  summarize(totHAI=sum(hai),totCases=sum(cases))

acutes_TS_grp$percHAI<-acutes_TS_grp$totHAI/acutes_TS_grp$totCases


## by day?
acutes_TSD_grp<-acutes_TS %>%
  group_by(date) %>%
  summarize(totHAI=sum(hai),totCases=sum(cases)) %>% 
  mutate(hai28d=sum_run(totHAI,28,idx=date),
         cases28d=sum_run(totCases,28,idx=date),
         percHAI28d=hai28d/cases28d)
  
acutes_TSD_grp$percHAI<-acutes_TSD_grp$totHAI/acutes_TSD_grp$totCases


ggplot(acutes_TSD_grp,aes(x=date,y=percHAI))+
  geom_smooth(span=0.15,size=2)+
  geom_point(alpha=0.5)+
  xlab("Date")+
  ylab("Percentage HAI")+
  ggtitle("Overall - Percentage HAI by day for English Acute Trusts combined")+
  scale_y_continuous(labels=scales::percent)+
  theme_minimal(base_size=18)

ggplot(acutes_TSD_grp[acutes_TSD_grp$date>as.Date("2021-05-01"),],aes(x=date,y=percHAI))+
  geom_smooth(span=0.5,size=2)+
  geom_point(alpha=0.5)+
  xlab("Date (2021-2)")+
  ylab("Percentage HAI")+
  ggtitle("Percentage HAI by day for English Acute Trusts combined")+
  scale_y_continuous(labels=scales::percent)+
  theme_minimal(base_size=18)


ggplot(acutes_TSD_grp[acutes_TSD_grp$date>as.Date("2021-05-01"),],aes(x=date,y=hai28d))+
  #geom_smooth(span=0.5,size=2)+
  #geom_point(alpha=0.5)+
  geom_line(size=2)+
  xlab("Date (2021-2)")+
  ylab("28 day total cases")+
  ggtitle("Rolling 28 day HAI cases for English Acute Trusts combined")+
  theme_minimal(base_size=18)

ggplot(acutes_TSD_grp[acutes_TSD_grp$date>as.Date("2021-05-01"),],aes(x=date,y=hai28d))+
  #geom_smooth(span=0.5,size=2)+
  #geom_point(alpha=0.5)+
  geom_line(size=2)+
  xlab("Date (2021-2)")+
  ylab("28 day percentage HAI")+
  ggtitle("Rolling 28 day HAI percentage for English Acute Trusts combined")+
  theme_minimal(base_size=18)


ggplot(acutes_TSD_grp,aes(x=date,y=percHAI28d))+
  #geom_smooth(span=0.5,size=2)+
  geom_point(alpha=0.5)+
  geom_smooth(size=1.5,span=0.1)+
  xlab("Date")+
  ylab("28 day HAI percentage")+
  ggtitle("Rolling 28 day HAI percentage for English Acute Trusts combined")+
  theme_minimal(base_size=18)+
  expand_limits(y=0)


## are cases related to community cases?

library(lubridate)

overall_long$rwdate<-round_date(overall_long$date,unit="month")
overall_roundweek<-overall_long %>%
  filter(`casesType 1 Acute?.x`=="Yes") %>%
  group_by(casesCode,casesName,rwdate) %>%
  summarise(ads=sum(ads),hai=sum(hai),cases=sum(cases)) %>%
  mutate(perHAI=hai/cases)


ggplot(overall_roundweek,aes(x=cases,y=hai))+
  geom_point(alpha=0.3)+
  xlab("Cases")+
  ylab("Hospital acquired cases")+
  ggtitle("Hospital acquired vs total cases (per month)")+
  theme_minimal(base_size=18)

ggplot(overall_roundweek,aes(x=cases,y=perHAI))+
         geom_point(alpha=0.3)+
xlab("Cases")+
  ylab("Hospital acquired cases (percentage)")+
  ggtitle("Percentage hospital acquired vs total cases (per month)")+
  scale_y_continuous(labels=scales::percent)+
theme_minimal(base_size=18)





library("FunnelPlotR")

model<-glm(hai ~ cases + factor(rwdate),data=overall_roundweek)
summary(model)
overall_roundweek$prds<-predict(model,type="response")

meanPerc<-sum(overall_roundweek$hai)/sum(overall_roundweek$cases)*100
meanPerc<-1

#acutes_grp_lim$prds<-acutes_grp_lim$totCases*sum(acutes_grp_lim$totHAI)/sum(acutes_grp_lim$totCases)

fplot<-funnel_plot(numerator=overall_roundweek$hai,denominator=overall_roundweek$prds,group=overall_roundweek$casesName,title="Funnel plot of Hospital Acquired COVID-19", data_type="SR",limit=99,Poisson_limits = FALSE,sr_method="SHMI",OD_adjust = TRUE,label_outliers=TRUE,multiplier=meanPerc,y_label="Ratio to average HAI percentage",x_label="Expected HAI")
print(fplot)







## Overall time series

overall_TSD_grp<-overall_long %>%
  group_by(date) %>%
  summarize(totHAI=sum(hai),totCases=sum(cases)) %>% 
  mutate(hai28d=sum_run(totHAI,28,idx=date),
         cases28d=sum_run(totCases,28,idx=date),
         percHAI28d=hai28d/cases28d)

overall_TSD_grp$percHAI<-acutes_TSD_grp$totHAI/acutes_TSD_grp$totCases


ggplot(overall_TSD_grp,aes(x=date,y=percHAI28d))+
  #geom_smooth(span=0.5,size=2)+
  #geom_point(alpha=0.5)+
  geom_smooth(size=2,span=0.05)+
  xlab("Date")+
  ylab("28 day HAI percentage")+
  ggtitle("Rolling 28 day HAI percentage for English Trusts combined")+
  theme_minimal(base_size=18)+
  expand_limits(y=0)

