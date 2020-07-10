library(readr)
library(dplyr)
library(runner)
library(tidyr)
LA_locations <- read_csv("C:/Documents/Work/202007 covid visualisation/Local_Authority_Districts__May_2020__Boundaries_UK_BGC.csv")
#covid_nos <- read_csv("C:/Documents/Work/202007 covid visualisation/20200705 coronavirus-cases_latest.csv")
covid_nos <- read_csv("https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv")

covid_nos <- filter(covid_nos,`Area type`=="Lower tier local authority")
covid_nos$date<-as.Date(covid_nos$`Specimen date`,"%d/%m/%Y")
covid_nos<-complete(covid_nos,`Area code`,`date`)
full<-merge(covid_nos,LA_locations,by.x="Area code",by.y="lad20cd")

test<-full %>% 
  group_by(`Area code`) %>% 
  arrange(`date`) %>% 
  mutate(
    sum7 = sum_run(`Daily lab-confirmed cases`,7,idx=date)
  )
test$sum7[is.na(test$sum7)]<-0

dates<-unique(test$date)

tmp<-filter(test,`lad20nm`=="Bradford" | `lad20nm`=="Leicester" | `lad20nm`=="York")

maxdate<-max(tmp$date)
tmp$alpha<-1.0
tmp$alpha[tmp$date==maxdate]<-0.1
tmp$alpha[tmp$date==maxdate-1]<-0.1
tmp$alpha[tmp$date==maxdate-2]<-0.3
tmp$alpha[tmp$date==maxdate-3]<-0.5
tmp1<-filter(tmp,date<maxdate-3)
tmp2<-filter(tmp,date>=maxdate-4)

library(ggplot2)
ggplot(data=tmp1,aes(x=date,y=sum7,group=lad20nm)) + geom_line(aes(color=lad20nm),size=3)+ geom_line(data=tmp2,aes(color=lad20nm,alpha=alpha),size=3) + xlab("Date") + ylab("7-Day sum of cases") + scale_alpha(guide=FALSE) + scale_color_discrete(name="Location") + theme_classic(base_size=18)

library(ggmap)
map<-get_stamenmap(zoom=7,maptype="terrain",bbox=c(left=min(full$long),bottom=min(full$lat),right=max(full$long),top=max(full$lat)),crop=FALSE)

library(ggtern)
library(animation)
ani.options(interval = .25)
#ani.options(ani.width = 800)
#ani.options(ani.height = 1200)

saveGIF({
  for (intdate in dates) {
    eachdate<-as.Date(intdate,origin = "1970-01-01")
    print(eachdate)
    #eachdate<-as.Date("2020-07-01") 
    mainset<-filter(test,date==eachdate)
    total<-sum(mainset$sum7)
    density<-kde2d.weighted(mainset$long,mainset$lat,h=0.5,n=200,w=mainset$sum7,lims=c(min(mainset$long)-1,max(mainset$long)+1,min(mainset$lat)-1,max(mainset$lat)+1))
    breaks<-c(0,seq(10,10000,length=128))
    densdf<-data.frame(expand.grid(x=density$x,y=density$y),z=as.vector(density$z))
    densdf$z<-densdf$z*total
    print(ggmap(map)+stat_contour(aes(x=x,y=y,z=z,fill=..level..,alpha=1),data=densdf,geom="polygon",breaks=breaks) +
            scale_fill_gradient(low = "blue", high = "red", guide=FALSE) + scale_alpha(range = c(0.0, 0.5), guide = FALSE) + 
            ggtitle(eachdate))
  }
})
