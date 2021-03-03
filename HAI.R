library(readxl)
library(tidyverse)
library(runner)
library(plotly)

##Data from: https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-hospital-activity/

filename<-"Weekly-covid-admissions-and-beds-publication-210225.xlsx"

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

#Dates are inclusive
#start_date<-as.Date("2020-12-20")
start_date<-as.Date("2021-01-11")
fin_date<-as.Date("2021-01-17")

tlim_overall_long<-overall_long[overall_long$date<=fin_date & overall_long$date>=start_date,]
tcases<-sum(tlim_overall_long$cases)
thai<-sum(tlim_overall_long$hai)
print(paste0("Specified dates - total HAI: ",thai,"/",tcases," (",thai/tcases,"%)"))

maxdate<-max(acutes$date)

tlim_overall_long<-overall_long[overall_long$date<=maxdate & overall_long$date>=(maxdate-28),]
tcases<-sum(tlim_overall_long$cases)
thai<-sum(tlim_overall_long$hai)
print(paste0("Most recent 28 days - total HAI: ",thai,"/",tcases," (",thai/tcases,"%)"))




## Charts for 28 days

maxdate<-max(acutes$date)

#maxdate<-as.Date("2020-09-30")

acutes_28d<-acutes[acutes$date>=maxdate-28 & acutes$date<=maxdate,]

acutes_28d_grp<-acutes_28d %>% 
  group_by(casesCode,casesName) %>% 
  summarize(totHAI=sum(hai),totCases=sum(cases))

acutes_28d_grp$percHAI<-acutes_28d_grp$totHAI/acutes_28d_grp$totCases

#remove smaller hospitals
acutes_28d_grp_lim<-acutes_28d_grp[acutes_28d_grp$totCases>40,]

lastdate<-max(acutes_28d$date)
firstdate<-min(acutes_28d$date)

ggplot(acutes_28d_grp_lim,aes(y=fct_reorder(casesName,percHAI),x=percHAI,fill=percHAI))+
  geom_col()+
  scale_fill_distiller(palette="Spectral",guide=FALSE)+
  xlab(paste0("Percentage hospital acquired since ",firstdate))+
  scale_x_continuous(labels=scales::percent,expand=expansion(mult=c(0.00,0.02))) +
  ylab("Trust (and total cases)") +
  geom_text(aes(label=totCases),x=0.002,size=3,show.legend=FALSE,hjust=0) +
  ggtitle(paste0("Hospital Acquired COVID-19 last 28 days (to ",lastdate,")")) +
  theme(plot.title.position='plot',plot.title=element_text(hjust=0.5))



## Slopegraphs
library("CGPfunctions")
source("newggrankslopegraph.R")

#acutes_28d_panel<-acutes %>% group_by(casesCode,casesName,dr=cut(date,2,ordered_result=TRUE)) %>% summarize(totHAI=sum(hai),totCases=sum(cases))
acutes_28d_panel<-acutes %>% group_by(casesCode,casesName,drB=(date<as.Date("2021-01-01"))) %>% summarize(totHAI=sum(hai),totCases=sum(cases))

acutes_28d_panel$dr<-factor(c('2021','2020')[acutes_28d_panel$drB+1],ordered=TRUE)

acutes_28d_panel$percHAI<-acutes_28d_panel$totHAI/acutes_28d_panel$totCases

# remove trusts where cases in either period below 200
acutes_28d_panel<-acutes_28d_panel %>% 
  group_by(casesCode) %>% 
  filter(min(totCases)>=200)

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
  mutate(difference = `2021` - `2020`) %>%
#  mutate(trend = case_when(
#    difference >= 20 ~ "green",
#    difference <= -20 ~ "red",
#    TRUE ~ "gray"
# )
#  ) %>%
  mutate(trend = rgb(pal(  (difference-min(difference))  / (max(difference)-min(difference)) ),maxColorValue=255)) %>% 
  #mutate(trend="gray") %>% 
  select(shortName, trend) %>%
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
  Title = "Ranked performance for Hospital-Acquired COVID-19 since August 2020",
  SubTitle = "2020 figures vs 2021",
  Caption = "Top of chart = lowest %HAI",
  CaptionTextSize = 12
)

print(bigchart)

#%HAI
bigchart<-newggrankslopegraph(
  acutes_28d_panel,
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
acutes_28d_panel$logitpercHAI<-log(acutes_28d_panel$percHAI/(1-acutes_28d_panel$percHAI))
bigchart<-newggrankslopegraph(
  acutes_28d_panel,
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
median(acutes_28d_panel[acutes_28d_panel$dr=="2020",]$percHAI)
median(acutes_28d_panel[acutes_28d_panel$dr=="2021",]$percHAI)
mean(acutes_28d_panel[acutes_28d_panel$dr=="2020",]$percHAI)
mean(acutes_28d_panel[acutes_28d_panel$dr=="2021",]$percHAI)

#ggsave("bigplot.png",plot=bigchart,device=png(),width=6,height=12,units="in",dpi=300)


# library("slopegraph")
# 
# ggslopegraph2(
#   acutes_28d_panel,
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

model<-glm(totHAI ~ totCases,data=acutes_grp_lim)
summary(model)
acutes_grp_lim$prds<-predict(model,type="response")

meanPerc<-sum(acutes_grp_lim$totHAI)/sum(acutes_grp_lim$totCases)*100
meanPerc<-1

#acutes_grp_lim$prds<-acutes_grp_lim$totCases*sum(acutes_grp_lim$totHAI)/sum(acutes_grp_lim$totCases)

fplot<-funnel_plot(numerator=acutes_grp_lim$totHAI,denominator=acutes_grp_lim$prds,group=acutes_grp_lim$casesName,title="Funnel plot of Hospital Acquired COVID-19", data_type="SR",limit=99,Poisson_limits = FALSE,sr_method="SHMI",OD_adjust = TRUE,label_outliers=TRUE,multiplier=meanPerc,y_label="Ratio to average HAI percentage",x_label="Expected HAI")
print(fplot)

