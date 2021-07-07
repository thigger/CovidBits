library(readxl)
library(tidyr)

##https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-hospital-activity/
filename<-"Covid-Publication-10-06-2021-Supplementary-Data.xlsx"

cnames<-paste0("",read_excel(filename,sheet="Admissions and Diagnoses",n_max=0,skip=12) %>% names())
#cnames<-c("MSOA code","MSOA Name ONS","MSOA Name HCL","blank",cnames)
#cnames<-janitor::make_clean_names(cnames)
hosp_data_raw<- read_excel(filename,sheet = "Admissions and Diagnoses", skip = 13,col_names=cnames,n_max=10)



comm_cases_raw<-read_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=newCasesBySpecimenDateAgeDemographics&format=csv")



t<-gather(hosp_data_raw,date,value,-Measure)
hosp_data<-spread(t,Measure,value)
hosp_data$Date<-as.Date(as.numeric(hosp_data$date),origin="1899-12-30")




comm_cases<-
  select(comm_cases_raw,c(date,age,cases)) %>% 
  spread(age,cases)

comm_cases$Total<-rowSums(comm_cases[,-1])

comm_cases$`Age0-5`<-comm_cases$`00_04`+comm_cases$`05_09`/5
comm_cases$`Age6-17`<-comm_cases$`05_09`*4/5+comm_cases$`10_14`+comm_cases$`15_19`*3/5
comm_cases$`Age18-54`<-comm_cases$`15_19`*2/5+comm_cases$`20_24`+comm_cases$`25_29`+comm_cases$`30_34`+comm_cases$`35_39`+comm_cases$`40_44`+comm_cases$`45_49`+comm_cases$`50_54`
comm_cases$`Age55-64`<-comm_cases$`55_59`+comm_cases$`60_64`
comm_cases$`Age65-74`<-comm_cases$`65_69`+comm_cases$`70_74`
comm_cases$`Age75-84`<-comm_cases$`75_79`+comm_cases$`80_84`
comm_cases$`Age85+`<-comm_cases$`85_89`+comm_cases$`90+`


offset<-0 #days
hosp_data$DateOff<-hosp_data$Date+offset


combined<-merge(comm_cases,hosp_data,by.x="date",by.y="DateOff")


library(runner)

rsum<-30


combined<-mutate(combined,
                 TotalProportion=sum_run(x=combined$`Total reported admissions and diagnoses`,k=rsum)/sum_run(x=combined$Total,k=rsum),
                 `Prop0-5`=sum_run(x=combined$`Total reported admissions and diagnoses  0-5`,k=rsum)/sum_run(x=combined$`Age0-5`,k=rsum),
                 `Prop6-17`=sum_run(x=combined$`Total reported admissions and diagnoses  6-17`,k=rsum)/sum_run(x=combined$`Age6-17`,k=rsum),
                 `Prop18-54`=sum_run(x=combined$`Total reported admissions and diagnoses  18-54`,k=rsum)/sum_run(x=combined$`Age18-54`,k=rsum),
                 `Prop55-64`=sum_run(x=combined$`Total reported admissions and diagnoses  55-64`,k=rsum)/sum_run(x=combined$`Age55-64`,k=rsum),
                 `Prop65-74`=sum_run(x=combined$`Total reported admissions and diagnoses  65-74`,k=rsum)/sum_run(x=combined$`Age65-74`,k=rsum),
                 `Prop75-84`=sum_run(x=combined$`Total reported admissions and diagnoses  75-84`,k=rsum)/sum_run(x=combined$`Age75-84`,k=rsum),
                 `Prop85+`=sum_run(x=combined$`Total reported admissions and diagnoses  85+`,k=rsum)/sum_run(x=combined$`Age85+`,k=rsum)
                 )


plotdf<-select(combined,Date,starts_with("Prop")) %>% 
  gather(key="var",value="value",-Date)

plotdf$varF<-factor(str_sub(plotdf$var,5),levels=c("85+","75-84","65-74","55-64","18-54","6-17","0-5"))


ggplot(plotdf,aes(x=Date,y=value))+
  geom_line(aes(color=varF),size=2)+
  scale_y_continuous(trans="log10")+
  geom_line(data=combined,aes(x=Date,y=TotalProportion),linetype="dotted",size=3) +
  guides(color=guide_legend(title="Age Group"))+
  theme_minimal(base_size=20)+
  ylab("Hospitalised per case")