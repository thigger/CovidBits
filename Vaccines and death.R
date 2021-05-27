## Death data
#https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/causesofdeath/articles/deathsduetocovid19interactivemap/2021-02-25
#

## Standardised deaths
#https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/deathsduetocovid19bylocalareaanddeprivation
#sadly not standardised at MSOA level
library(readxl)
library(janitor)
library(tidyverse)
library(dplyr)

filename<-"covidlocalareadeprivationfebruary2021.xlsx"

cnames<-paste0("msoadeaths",read_excel(filename,sheet="Table 6",n_max=0,skip=5) %>% names())
cnames<-c("MSOA code","MSOA Name ONS","MSOA Name HCL","blank",cnames)
cnames<-janitor::make_clean_names(cnames)
msoadeaths<- read_excel(filename,sheet = "Table 6", skip = 6,col_names=cnames)
##All causes, COVID-19, other causes
msoadeaths$AllCauseTotalDeaths<-msoadeaths$msoadeaths12_month_total_march_2020_to_february_2021_13
msoadeaths$CovidTotalDeaths<-msoadeaths$msoadeaths12_month_total_march_2020_to_february_2021_27
msoadeaths$OtherCauseTotalDeaths<-msoadeaths$msoadeaths12_month_total_march_2020_to_february_2021_41

#SMRscale<-sum(na.omit(msoadeaths$AllCauseTotalDeaths))/sum(na.omit(msoadeaths$CovidTotalDeaths))

## Reference tables for mortality
# https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/deathsregisteredinenglandandwalesseriesdrreferencetables
# finalreftables2019.xlsx
# copied relevant bits into UK-2019-age-mortality.xlsx
UK_2019_age_mortality <- read_csv("UK-2019-age-mortality.csv")
# remove the 0 and 1-4 rows
UK_2019_age_mortality <- UK_2019_age_mortality %>% 
  subset(AgeGroup!="0" & AgeGroup!="1-4")

UK_12month_COVID_deaths <- read_csv("UK12monthCovidDeaths.csv")
UK_12month_COVID_deaths <- UK_12month_COVID_deaths %>% 
  subset(AgeGroup!="<1" & AgeGroup!="1-4")


## MSOA population (mid 2019)
#5 year bands
#https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/middlesuperoutputareamidyearpopulationestimatesnationalstatistics
#1 year
#https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/middlesuperoutputareamidyearpopulationestimates

MSOAMales <- read_excel("SAPE22DT15-mid-2019-msoa-quinary-estimates-unformatted.xlsx", 
                                                                     sheet = "Mid-2019 Males", skip = 3)
MSOAFemales <- read_excel("SAPE22DT15-mid-2019-msoa-quinary-estimates-unformatted.xlsx", 
                        sheet = "Mid-2019 Females", skip = 3)

MSOAMales<-MSOAMales %>% 
  mutate(TotalOver60s = select(.,`60-64`:`90+`) %>% rowSums()) %>% 
  mutate(TotalOver50s = select(.,`50-54`:`90+`) %>% rowSums()) %>% 
  mutate(TotalOver40s = select(.,`40-44`:`90+`) %>% rowSums())

MSOAFemales<-MSOAFemales %>% 
  mutate(TotalOver60s = select(.,`60-64`:`90+`) %>% rowSums()) %>% 
  mutate(TotalOver50s = select(.,`50-54`:`90+`) %>% rowSums()) %>% 
  mutate(TotalOver40s = select(.,`40-44`:`90+`) %>% rowSums())

## Background death data
# https://www.nomisweb.co.uk/query/construct/submit.asp?forward=yes&menuopt=201&subcomp=
#nomisdeaths<-read_excel("nomis deaths by MSOA by year to 2019.xlsx",skip=9)

## Vaccine data
#  https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-vaccinations/
## nb may need changing if new columns added

COVID_19_weekly_announced_vaccinations <- read_excel("COVID-19-weekly-announced-vaccinations-20-May-2021.xlsx", 
                                                                   sheet = "MSOA", skip = 12)
COVID_19_weekly_announced_vaccinations_old <- read_excel("COVID-19-weekly-announced-vaccinations-25-March-2021.xlsx", 
                                                     sheet = "MSOA", skip = 12)
#1st doses only
COVID_19_weekly_announced_vaccinations<-COVID_19_weekly_announced_vaccinations %>% 
  mutate(TotalVaccines=select(.,`Under 40...7`:`80+...16`) %>% rowSums()) %>% 
  mutate(Over60Vaccines=select(.,`60-64...12`:`80+...16`) %>% rowSums()) %>% 
  mutate(Over50Vaccines=select(.,`50-54...10`:`80+...16`) %>% rowSums()) %>% 
  mutate(Over40Vaccines=select(.,`40-44...8`:`80+...16`) %>% rowSums())



## Calculate expected deaths by MSOA for indirect standardisation
5
MSOAmortMales<-MSOAMales
MSOAmortFemales<-MSOAFemales

## Standardise to UK 2019 all-cause mortality (leads to low SMRs)

# for (i in 1:nrow(UK_2019_age_mortality)) {
# ag<-UK_2019_age_mortality[i,]$AgeGroup
# MSOAmortMales[[ag]]<-MSOAMales[[ag]]*UK_2019_age_mortality$MaleMortalityPer1000[UK_2019_age_mortality$AgeGroup==ag]/1000
# MSOAmortFemales[[ag]]<-MSOAFemales[[ag]]*UK_2019_age_mortality$FemaleMortalityPer1000[UK_2019_age_mortality$AgeGroup==ag]/1000
# }

## Standardise to 12-month COVID-19 mortality

for (i in 1:nrow(UK_12month_COVID_deaths)) {
  ag<-UK_12month_COVID_deaths[i,]$AgeGroup
  agRateM<-UK_12month_COVID_deaths[i,]$MaleCovidDeaths/sum(MSOAMales[[ag]])
  agRateF<-UK_12month_COVID_deaths[i,]$FemaleCovidDeaths/sum(MSOAFemales[[ag]])
  MSOAmortMales[[ag]]<-MSOAMales[[ag]]*agRateM
  MSOAmortFemales[[ag]]<-MSOAFemales[[ag]]*agRateF
}


MSOAmortMales<-MSOAmortMales %>% 
  mutate(TotalDeaths = select(.,`0-4`:`90+`) %>% rowSums())

MSOAmortFemales<-MSOAmortFemales %>% 
  mutate(TotalDeaths = select(.,`0-4`:`90+`) %>% rowSums())

MSOAmortTotal<-left_join(MSOAmortFemales,MSOAmortMales,by="MSOA Code")
MSOAmortTotal$CombinedExpectedDeaths<-MSOAmortTotal$TotalDeaths.x+MSOAmortTotal$TotalDeaths.y
MSOAmortTotal$CombinedPopulation<-MSOAmortTotal$`All Ages.x`+MSOAmortTotal$`All Ages.y`
MSOAmortTotal$CombinedOver60s<-MSOAmortTotal$TotalOver60s.x+MSOAmortTotal$TotalOver60s.y
MSOAmortTotal$CombinedOver50s<-MSOAmortTotal$TotalOver50s.x+MSOAmortTotal$TotalOver50s.y
MSOAmortTotal$CombinedOver40s<-MSOAmortTotal$TotalOver40s.x+MSOAmortTotal$TotalOver40s.y

## indirect standardisation


MSOACombinedDeathsCovid<-left_join(MSOAmortTotal,msoadeaths,by=c("MSOA Code"="msoa_code"))

MSOAOEDataSet<-MSOACombinedDeathsCovid %>% select("MSOA Code","LA Code (2020 boundaries).x","LA name (2020 boundaries).x","msoa_name_hcl","CombinedExpectedDeaths","CovidTotalDeaths","CombinedPopulation","CombinedOver60s","CombinedOver50s","CombinedOver40s")

# SMR mortality - scale expected down to national proportion of COVID-19 deaths
MSOAOEDataSet$SMRCovid<-MSOAOEDataSet$CovidTotalDeaths/MSOAOEDataSet$CombinedExpectedDeaths*100
# Raw mortality per 100,000
MSOAOEDataSet$RawMortCovid<-MSOAOEDataSet$CovidTotalDeaths/MSOAOEDataSet$CombinedPopulation*100000

## Add in vaccines



MSOAVaccinesTotal<-select(COVID_19_weekly_announced_vaccinations,"...5","TotalVaccines","Over60Vaccines","Over50Vaccines","Over40Vaccines")

MSOAOverall<-inner_join(MSOAOEDataSet,MSOAVaccinesTotal,by=c("MSOA Code"="...5"))
MSOAOverall$VaccinatedPercent<-MSOAOverall$TotalVaccines/MSOAOverall$CombinedPopulation
MSOAOverall$VaccinatedOver60Percent<-MSOAOverall$Over60Vaccines/MSOAOverall$CombinedOver60s
MSOAOverall$VaccinatedOver50Percent<-MSOAOverall$Over50Vaccines/MSOAOverall$CombinedOver50s
MSOAOverall$VaccinatedOver40Percent<-MSOAOverall$Over40Vaccines/MSOAOverall$CombinedOver40s

## Load shapefile
library(sf)
msoa_poly<-st_read("ShapeFiles/Middle_Layer_Super_Output_Areas_(December_2011)_Boundaries.shp")

library(rmapshaper)
#msoa_poly<-ms_simplify(msoa_poly)


msoa_poly <- st_transform(msoa_poly, 4326)

data_poly<-merge(msoa_poly,MSOAOverall,by.x="msoa11cd",by.y="MSOA Code",all.x=FALSE)


library(leaflet)

library(scales)
library(RColorBrewer)
library(htmlwidgets)

#obs_map <- leaflet(data_poly) %>% addTiles() 


Vacc_n<-10
VaccPal<-colorQuantile(colorRampPalette(brewer.pal(9,"YlOrRd"))(Vacc_n), n=Vacc_n, domain=data_poly$VaccinatedPercent)
VaccPal_cols<-unique(VaccPal(sort(data_poly$VaccinatedPercent)))
VaccPal_labs<-percent(quantile(data_poly$VaccinatedPercent,seq(0,1,1/Vacc_n)),accuracy=1)
first<-VaccPal_labs[[2]]
VaccPal_labs<-paste(lag(VaccPal_labs),VaccPal_labs,sep=" - ")[-1]
VaccPal_labs[[1]]<-paste0("<",first)

Vacc60_n<-10
Vacc60Pal<-colorQuantile(colorRampPalette(brewer.pal(9,"YlOrRd"))(Vacc60_n), n=Vacc60_n, domain=data_poly$VaccinatedOver60Percent)
Vacc60Pal_cols<-unique(Vacc60Pal(sort(data_poly$VaccinatedOver60Percent)))
Vacc60Pal_labs<-percent(quantile(data_poly$VaccinatedOver60Percent,seq(0,1,1/Vacc60_n)),accuracy=1)
first<-Vacc60Pal_labs[[2]]
last<-Vacc60Pal_labs[[Vacc60_n]]
Vacc60Pal_labs<-paste(lag(Vacc60Pal_labs),Vacc60Pal_labs,sep=" - ")[-1]
Vacc60Pal_labs[[1]]<-paste0("<",first)
Vacc60Pal_labs[[Vacc60_n]]<-paste0(">",last)

Vacc50_n<-10
Vacc50Pal<-colorQuantile(colorRampPalette(brewer.pal(9,"YlOrRd"))(Vacc50_n), n=Vacc50_n, domain=data_poly$VaccinatedOver50Percent)
Vacc50Pal_cols<-unique(Vacc50Pal(sort(data_poly$VaccinatedOver50Percent)))
Vacc50Pal_labs<-percent(quantile(data_poly$VaccinatedOver50Percent,seq(0,1,1/Vacc50_n)),accuracy=1)
first<-Vacc50Pal_labs[[2]]
last<-Vacc50Pal_labs[[Vacc50_n]]
Vacc50Pal_labs<-paste(lag(Vacc50Pal_labs),Vacc50Pal_labs,sep=" - ")[-1]
Vacc50Pal_labs[[1]]<-paste0("<",first)
Vacc50Pal_labs[[Vacc50_n]]<-paste0(">",last)

Vacc40_n<-10
Vacc40Pal<-colorQuantile(colorRampPalette(brewer.pal(9,"YlOrRd"))(Vacc40_n), n=Vacc40_n, domain=data_poly$VaccinatedOver40Percent)
Vacc40Pal_cols<-unique(Vacc40Pal(sort(data_poly$VaccinatedOver40Percent)))
Vacc40Pal_labs<-percent(quantile(data_poly$VaccinatedOver40Percent,seq(0,1,1/Vacc40_n)),accuracy=1)
first<-Vacc40Pal_labs[[2]]
last<-Vacc40Pal_labs[[Vacc40_n]]
Vacc40Pal_labs<-paste(lag(Vacc40Pal_labs),Vacc40Pal_labs,sep=" - ")[-1]
Vacc40Pal_labs[[1]]<-paste0("<",first)
Vacc40Pal_labs[[Vacc40_n]]<-paste0(">",last)



SMR_n<-10
SMRPal<-colorQuantile(colorRampPalette(brewer.pal(9,"YlOrRd"))(SMR_n), n=SMR_n, domain=data_poly$SMRCovid)
SMRPal_cols<-unique(SMRPal(sort(data_poly$SMRCovid)))
SMRPal_labs<-signif(quantile(data_poly$SMRCovid,seq(0,1,1/SMR_n)),digits=3)
first<-SMRPal_labs[[2]]
SMRPal_labs<-paste(lag(SMRPal_labs),SMRPal_labs,sep=" - ")[-1]
SMRPal_labs[[1]]<-paste0("<",first)

Mort_n<-10
MortPal<-colorQuantile(colorRampPalette(brewer.pal(9,"YlOrRd"))(Mort_n), n=Mort_n, domain=data_poly$RawMortCovid)
MortPal_cols<-unique(MortPal(sort(data_poly$RawMortCovid)))
MortPal_labs<-signif(quantile(data_poly$RawMortCovid,seq(0,1,1/SMR_n)),digits=3)
first<-MortPal_labs[[2]]
MortPal_labs<-paste(lag(MortPal_labs),MortPal_labs,sep=" - ")[-1]
MortPal_labs[[1]]<-paste0("<",first)

library(htmltools)

data_poly <- data_poly %>% 
  mutate(Label=paste0("<b>",msoa_name_hcl,"</b><br/>",
                           percent(VaccinatedPercent,accuracy=1)," Vaccinated Total<br/>",
           #                percent(VaccinatedOver60Percent,accuracy=1)," Vaccinated of Over 60s<br/>",
                          percent(VaccinatedOver50Percent,accuracy=1)," Vaccinated of Over 50s<br/>",
                     percent(VaccinatedOver40Percent,accuracy=1)," Vaccinated of Over 40s<br/>",
                           signif(SMRCovid,digits=3)," COVID-19 Standardised Mortality Rate<br/>",
                           signif(RawMortCovid,digits=3)," COVID-19 mortality per 100,000"
    
                           ))
##Ugly but HTML just takes the whole vector if you let it
data_poly$HTMLLabel<-lapply(data_poly$Label,htmltools::HTML)


obs_map<-leaflet(data_poly) %>% addProviderTiles(providers$Stamen.Toner)
obs_map %>% fitBounds(-3.195992, 53.338868, -2.762375, 53.462912)

obs_map<-obs_map %>% addPolygons(color = "black", 
                        weight = 1, 
                        smoothFactor = 0.5,
                        opacity = 0.1, 
                        fillOpacity = 0.5, 
                        fillColor = ~VaccPal(VaccinatedPercent),
                        group="Proportion Vaccinated",label=~HTMLLabel
                        ) %>%
  addPolygons(color = "black", 
              weight = 1, 
              smoothFactor = 0.5,
              opacity = 0.1, 
              fillOpacity = 0.5, 
              fillColor = ~SMRPal(SMRCovid),
              group="Mortality (SMR)",label=~HTMLLabel
  ) %>% 
  addPolygons(color = "black", 
              weight = 1, 
              smoothFactor = 0.5,
              opacity = 0.1, 
              fillOpacity = 0.5, 
              fillColor = ~MortPal(RawMortCovid),
              group="Mortality (Raw)",label=~HTMLLabel
  ) %>%
  # addPolygons(color = "black",
  #             weight = 1,
  #             smoothFactor = 0.5,
  #             opacity = 0.1,
  #             fillOpacity = 0.5,
  #             fillColor = ~Vacc60Pal(VaccinatedOver60Percent),
  #             group="Proportion >60 Vaccinated",label=~HTMLLabel
  # ) %>%
  addPolygons(color = "black",
              weight = 1,
              smoothFactor = 0.5,
              opacity = 0.1,
              fillOpacity = 0.5,
              fillColor = ~Vacc50Pal(VaccinatedOver50Percent),
              group="Proportion >50 Vaccinated",label=~HTMLLabel
  ) %>%
  addPolygons(color = "black",
              weight = 1,
              smoothFactor = 0.5,
              opacity = 0.1,
              fillOpacity = 0.5,
              fillColor = ~Vacc40Pal(VaccinatedOver40Percent),
              group="Proportion >40 Vaccinated",label=~HTMLLabel
  ) %>%
  addLayersControl(
    baseGroups=c("Proportion Vaccinated","Proportion >50 Vaccinated","Proportion >40 Vaccinated","Mortality (SMR)","Mortality (Raw)"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  addLegend(
    "bottomright",colors=VaccPal_cols,labels=VaccPal_labs,
    title="Percent Vaccinated",
    #labFormat=labelFormat(""),
    group="Proportion Vaccinated",
    className="info legend ProportionVaccinated"
  ) %>%
  addLegend(
    "bottomright",colors=SMRPal_cols,labels=SMRPal_labs,
    title="Mortality (COVID-19 SMR)",
    #labFormat=labelFormat(""),
    group="Mortality (SMR)",
    className="info legend Mortality(SMR)"
  ) %>%
  addLegend(
    "bottomright",colors=MortPal_cols,labels=MortPal_labs,
    title="COVID-19 Mortality (per 100,000)",
    #labFormat=labelFormat(""),
    group="Mortality (Raw)",
    className="info legend Mortality(Raw)"
  ) %>%
  # addLegend(
  #   "bottomright",colors=Vacc60Pal_cols,labels=Vacc60Pal_labs,
  #   title="Percent >60 Vaccinated",
  #   #labFormat=labelFormat(""),
  #   group="Proportion >60 Vaccinated",
  #   className="info legend Proportion60Vaccinated"
  # ) %>%
  addLegend(
    "bottomright",colors=Vacc50Pal_cols,labels=Vacc50Pal_labs,
    title="Percent >50 Vaccinated",
    #labFormat=labelFormat(""),
    group="Proportion >50 Vaccinated",
    className="info legend Proportion50Vaccinated"
  ) %>%
  addLegend(
    "bottomright",colors=Vacc40Pal_cols,labels=Vacc40Pal_labs,
    title="Percent >40 Vaccinated",
    #labFormat=labelFormat(""),
    group="Proportion >40 Vaccinated",
    className="info legend Proportion40Vaccinated"
  ) %>%
htmlwidgets::onRender("
      function(el, x) {
         var updateLegend = function () {
            var selectedGroup = document.querySelectorAll('input:checked')[0].nextSibling.innerText.substr(1);
            var selectedClass = selectedGroup.replace(/ /g, '');
            selectedClass = selectedClass.replace('>', '');
            document.querySelectorAll('.legend').forEach(a => a.hidden=true);
            document.querySelectorAll('.legend').forEach(l => {
               if (l.classList.contains(selectedClass)) l.hidden=false;
            });
         };
         updateLegend();
         this.on('baselayerchange', el => updateLegend());
      }"
)

#saveWidget(obs_map,file="C:/Users/cliche/Desktop/tmp/VaccMapSimp20May.html",selfcontained = TRUE)

#saveWidget(obs_map,file="C:/Users/cliche/Desktop/tmp/VaccMapFull.html",selfcontained = TRUE)






## regression



model1<-lm(VaccinatedPercent ~ SMRCovid,data=data_poly)
summary(model1)

pred_vars <- predict(model1, interval="prediction")

test<-cbind(na.omit(data_poly),pred_vars)

ggplot(test,aes(x=SMRCovid,y=VaccinatedPercent,colour=VaccinatedOver60Percent))+
  geom_point()+
  scale_color_gradient(low="blue",high="red",guide=FALSE)+
  xlab("COVID-19 SMR")+
  scale_y_continuous(labels=scales::percent) +
  ylab("Percentage of population vaccinated") +
  #geom_text(aes(label=totCases,x=0)) +
  ggtitle("Vaccine rollout vs Standardised mortality rate from COVID-19") +
  geom_smooth(method=lm,color="red",fill="grey",se=TRUE) +
  geom_line(aes(y=lwr), color = "red", linetype = "dashed") +
  geom_line(aes(y=upr), color = "red", linetype = "dashed")
 # + scale_x_continuous(trans="log2")





model1<-lm(VaccinatedPercent ~ RawMortCovid,data=data_poly)
summary(model1)

pred_vars <- predict(model1, interval="prediction")

test<-cbind(na.omit(data_poly),pred_vars)

ggplot(test,aes(x=RawMortCovid,y=VaccinatedPercent,colour=VaccinatedOver60Percent))+
  geom_point()+
  scale_color_gradient(low="blue",high="red",guide=FALSE)+
  xlab("COVID-19 Mortality per 100,000")+
  scale_y_continuous(labels=scales::percent) +
  ylab("Percentage of population vaccinated") +
  #geom_text(aes(label=totCases,x=0)) +
  ggtitle("Vaccine rollout vs Raw mortality from COVID-19") +
  geom_smooth(method=lm,color="red",fill="grey",se=TRUE) +
  geom_line(aes(y=lwr), color = "red", linetype = "dashed") +
  geom_line(aes(y=upr), color = "red", linetype = "dashed")
# + scale_x_continuous(trans="log2")


