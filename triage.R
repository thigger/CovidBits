library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
triage_nos <- read_csv("C:/Documents/Work/202007 covid visualisation/NHS Pathways Covid-19 data 2020-09-09.csv")
#from: https://digital.nhs.uk/data-and-information/publications/statistical/mi-potential-covid-19-symptoms-reported-through-nhs-pathways-and-111-online/latest

triage_nos$date<-as.Date(triage_nos$`Call Date`,"%d/%m/%Y")
triage_nos$AgeBand[triage_nos$AgeBand=="70-120 years"]<-"70+ years"

test<-aggregate(triage_nos$TriageCount,by=list(date=triage_nos$date,ageband=triage_nos$AgeBand),FUN=sum)
test$ageband<-factor(test$ageband,levels=c("70+ years","19-69 years","0-18 years"))
maxdate<-max(test$date)
test<-filter(test,date>maxdate-28)
ggplot(test,aes(x=date,y=x,fill=ageband)) + geom_area(size=1,colour="black") + ggtitle("National 111 Triages for COVID-19") + theme_classic(base_size=18) + theme(axis.title.y=element_blank()) + labs(fill="Age Band",x="Date")



triage_nosB<-filter(triage_nos,CCGName=="NHS Bradford District and Craven CCG")
test<-aggregate(triage_nosB$TriageCount,by=list(date=triage_nosB$date,ageband=triage_nosB$AgeBand),FUN=sum)
test<-complete(test,`date`,`ageband`)
test$x[is.na(test$x)]<-0
test$ageband<-factor(test$ageband,levels=c("70+ years","19-69 years","0-18 years"))
maxdate<-max(test$date)
test<-filter(test,date>maxdate-28)
ggplot(test,aes(x=date,y=x,fill=ageband)) + geom_area(size=1,colour="black") + ggtitle("Bradford 111 Triages for COVID-19") + theme_classic(base_size=18) + theme(axis.title.y=element_blank()) + labs(fill="Age Band",x="Date")
