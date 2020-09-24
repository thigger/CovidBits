library(readr)
library(dplyr)
library(tidyr)
library(readxl)
library(ISOweek)
ecdc <- read_excel("C:/Documents/Work/202007 covid visualisation/ECDC/hosp_icu_all_data_2020-09-23.xlsx", 
                   col_types = c("text", "text", "text", 
                                 "text", "numeric", "text", "text"))
## source: https://www.ecdc.europa.eu/en/publications-data/download-data-hospital-and-icu-admission-rates-and-current-occupancy-covid-19

ecdc$date_wk<-ISOweek2date(paste0(ecdc$year_week,"-1"))
ecdc$date_day<-as.Date(ecdc$date)

spain_hosp<-ecdc[ecdc$country=="Spain" & ecdc$indicator=="Weekly new hospital admissions per 100k",]
france_hosp<-ecdc[ecdc$country=="France" & ecdc$indicator=="Weekly new hospital admissions per 100k",]
UK_hosp<-ecdc[ecdc$country=="United Kingdom" & ecdc$indicator=="Weekly new hospital admissions per 100k",]

spain_icu<-ecdc[ecdc$country=="Spain" & ecdc$indicator=="Weekly new ICU admissions per 100k",]
france_icu<-ecdc[ecdc$country=="France" & ecdc$indicator=="Weekly new ICU admissions per 100k",]
UK_icu<-ecdc[ecdc$country=="United Kingdom" & ecdc$indicator=="Weekly new ICU admissions per 100k",] ##missing? could get from coronavirus.data.gov.uk



tmp<-rbind(spain_hosp,france_hosp,UK_hosp)

maxdate<-max(tmp$date_wk)
tmp$alpha<-1.0
tmp$alpha[tmp$date_wk==maxdate & tmp$country=="Spain"]<-0.1
tmp$alpha[tmp$date_wk==maxdate-7 & tmp$country=="Spain"]<-0.2
tmp$alpha[tmp$date_wk==maxdate-14 & tmp$country=="Spain"]<-0.5
tmp1<-filter(tmp,`date_wk`<=maxdate-14)
tmp2<-filter(tmp,`date_wk`>=maxdate-14)


tmp1<-filter(tmp1,`date_wk`>=as.Date("2020-03-01"))

library(ggplot2)
print(ggplot(data=tmp1,aes(x=date_wk,y=value,group=country)) +
        geom_line(aes(color=country),size=3)+
        geom_line(data=tmp2,aes(color=country,alpha=alpha),size=3) +
        xlab("Date") +
        ylab("Weekly hospital admissions per 100k") +
        scale_alpha(guide=FALSE) +
        scale_color_discrete(name="Country") +
        theme_classic(base_size=18) +
        scale_y_log10())
