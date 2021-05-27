library(readr)
library(dplyr)
library(runner)
library(tidyr)
library(bbplot)
LA_locations <- read_csv("C:/Documents/Work/202007 covid visualisation/Local_Authority_Districts__May_2020__Boundaries_UK_BGC.csv")
#from: http://geoportal.statistics.gov.uk/datasets/
#covid_nos <- read_csv("C:/Documents/Work/202007 covid visualisation/20200705 coronavirus-cases_latest.csv")
#covid_nos <- read_csv("https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv")
covid_nos <- read_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=newCasesBySpecimenDate&format=csv")

#nb look at: https://coronavirus.data.gov.uk/about-data#cases-by-age for age breakdown

#covid_nos <- filter(covid_nos,`Area type`=="ltla")
covid_nos$date<-as.Date(covid_nos$`date`,"%d/%m/%Y")
covid_nos<-complete(covid_nos,`areaCode`,`date`)
full<-merge(covid_nos,LA_locations,by.x="areaCode",by.y="lad20cd")

test<-full %>% 
  group_by(`areaCode`) %>% 
  arrange(`date`) %>% 
  mutate(
    sum7 = sum_run(`newCasesBySpecimenDate`,7,idx=date)
  )
test$sum7[is.na(test$sum7)]<-0


#tmp<-filter(test,`lad20nm`=="Manchester" | `lad20nm`=="Liverpool" | `lad20nm`=="Leeds"| `lad20nm`=="Bradford" | `lad20nm`=="Nottingham")

tmp<-filter(test,`lad20nm`=="Liverpool" | `lad20nm`=="Leeds"| `lad20nm`=="Bradford")

maxdate<-max(tmp$date)
tmp<-filter(tmp,date>maxdate-7*10)
tmp$alpha<-1.0
tmp$alpha[tmp$date==maxdate]<-0.1
tmp$alpha[tmp$date==maxdate-1]<-0.1
tmp$alpha[tmp$date==maxdate-2]<-0.3
tmp$alpha[tmp$date==maxdate-3]<-0.5
tmp$alpha[tmp$date==maxdate-4]<-0.7
tmp1<-filter(tmp,date<maxdate-4)
tmp2<-filter(tmp,date>=maxdate-5)


colors <- c("#1F77B4", "#AEC7E8", "#FF7F0E", "#FFBB78", "#2CA02C",
            "#98DF8A", "#D62728", "#FF9896", "#9467BD", "#C5B0D5",
            "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF")

library(ggplot2)
print(ggplot(data=tmp1,aes(x=date,y=sum7,group=lad20nm)) + geom_line(aes(color=lad20nm),size=3)+ geom_line(data=tmp2,aes(color=lad20nm,alpha=alpha),size=3) + xlab("Date") + ylab("7-Day sum of cases") + scale_alpha(guide=FALSE) + scale_color_discrete(name="Location") +
        labs(title="COVID-19 Growth",
             subtitle = "7-day sum of cases in each city") +
        scale_color_manual(values=colors) +
        bbc_style())





bbc_plot<-ggplot(data=tmp1,aes(x=date,y=sum7,group=lad20nm)) + geom_line(aes(color=lad20nm),size=3)+ geom_line(data=tmp2,aes(color=lad20nm,alpha=alpha),size=3) + xlab("Date") + ylab("7-Day sum of cases") + scale_alpha(guide=FALSE) + scale_color_discrete(name="Location") +
  labs(title="COVID-19 Growth",
       subtitle = "7-day sum of cases in each city") +
  scale_color_manual(values=colors) +
  bbc_style() + scale_y_continuous(trans='log10')


finalise_plot(plot_name=bbc_plot,source="Source: Public Health England",save_filepath="bbcplot.png",width_pixels=1200,height_pixels=1000)

      #theme_classic(base_size=18))

library(ggmap)
map<-get_stamenmap(zoom=7,maptype="terrain",bbox=c(left=min(full$long),bottom=min(full$lat),right=max(full$long),top=max(full$lat)),crop=FALSE)

library(ggtern)
library(animation)
ani.options(interval = .25)
#ani.options(ani.width = 800)
#ani.options(ani.height = 1200)


dates<-unique(test$date)

dates<-seq.Date(max(test$date)-35,max(test$date)-2,1)

saveGIF({
  for (intdate in dates) {
    eachdate<-as.Date(intdate,origin = "1970-01-01")
    print(eachdate)
    #eachdate<-as.Date("2020-07-01") 
    mainset<-filter(test,date==eachdate)
    total<-sum(mainset$sum7)
    density<-kde2d.weighted(mainset$long,mainset$lat,h=0.5,n=200,w=mainset$sum7,lims=c(min(mainset$long)-1,max(mainset$long)+1,min(mainset$lat)-1,max(mainset$lat)+1))
    breaks<-c(0,seq(10,20000,length=128))
    densdf<-data.frame(expand.grid(x=density$x,y=density$y),z=as.vector(density$z))
    densdf$z<-densdf$z*total
    print(ggmap(map)+stat_contour(aes(x=x,y=y,z=z,fill=..level..,alpha=1),data=densdf,geom="polygon",breaks=breaks) +
            scale_fill_gradient(low = "blue", high = "red", guide=FALSE) + scale_alpha(range = c(0.0, 0.5), guide = FALSE) + 
            ggtitle(eachdate))
  }
})
