library(readxl)
library(tidyverse)
library(runner)
library(plotly)

##Data from: https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-hospital-activity/

filename<-"COVID-19-daily-admissions-and-beds-20220308.xlsx"

cnames<-paste0("cases",read_excel(filename,sheet="Daily publication",n_max=0,skip=27) %>% names())
new_hosp_cases_wide<-na.omit(read_excel(filename,sheet="Daily publication",skip=28,n_max=8,col_names=cnames))

cnames<-paste0("admis",read_excel(filename,sheet="Daily publication",n_max=0,skip=42) %>% names())
new_hosp_admis_wide<-na.omit(read_excel(filename,sheet="Daily publication",skip=43,n_max=8,col_names=cnames))


overall_wide<-na.omit(merge(new_hosp_cases_wide,new_hosp_admis_wide,by.x="casesName",by.y="admisName"))

overall_long<-gather(overall_wide,variable,value,matches("\\d\\d",perl=TRUE)) %>% 
  separate("variable",into=c("var","date"),sep=-5) %>% 
  spread(var,value)

overall_long$hai<-overall_long$cases-overall_long$admis

overall_long$date<-strtoi(overall_long$date)



sumDays<-7

test<-overall_long %>% 
  group_by(`casesName`) %>% 
  arrange(`date`) %>% 
  mutate(
    sumNads = sum_run(`admis`,sumDays,idx=date),
    sumNcases = sum_run(`cases`,sumDays,idx=date),
    sumNhai = sum_run(`hai`,sumDays,idx=date),
  )
test$sumNads[is.na(test$sumNads)]<-0
test$sumNcases[is.na(test$sumNcases)]<-0
test$sumNhai[is.na(test$sumNhai)]<-0


test$propNhai<-test$sumNhai/test$sumNcases
test$propNhai[is.nan(test$propNhai)]<-0
test$Ddate<-as.Date(test$date,origin="1899-12-30")


ggplot(test[test$casesName!="ENGLAND",],aes(x=Ddate,y=propNhai,colour=casesName))+
         geom_smooth(se=FALSE,span=0.25,size=1.5)+
  theme_minimal(base_size=18)+
  #geom_point()+
  xlab("Date")+
  ylab("Proportion HAI")+
  ggtitle("Hospital-acquired Covid proportion by region")+
  scale_y_continuous(labels = scales::percent_format(accuracy=1))+
  scale_colour_brewer(name="Region",palette="Dark2")+
  theme(legend.position="bottom",legend.text = element_text(size=10))



ggplot(test[test$casesName!="ENGLAND",],aes(x=Ddate,y=cases,colour=casesName))+
 # geom_smooth(se=FALSE,span=0.3,size=1.5)+
  geom_line(size=1.5)+
  theme_minimal(base_size=18)+
  xlab("Date")+
  ylab("New cases")+
  ggtitle("New daily hospital cases of Covid-19 by region")+
  scale_colour_brewer(name="Region",palette="Dark2")+
  theme(legend.position="bottom",legend.text = element_text(size=10))


ggplot(test[test$casesName!="ENGLAND",],aes(x=Ddate,y=admis,colour=casesName))+
  geom_smooth(se=FALSE,span=0.3,size=1.5)+
  theme_minimal(base_size=18)+
  xlab("Date")+
  ylab("New admissions")+
  ggtitle("New daily admissions from the community of Covid-19 by region")+
  scale_colour_brewer(name="Region",palette="Dark2")+
  theme(legend.position="bottom",legend.text = element_text(size=10))

