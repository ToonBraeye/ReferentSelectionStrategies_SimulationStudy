
library(RcppBDT)
library(timeDate)
library(lubridate)
library(dplyr)
library(survival)
library(gnm)
library(data.table)

############
#### Input Data
############

#1.RSS (risk- and referent-dates)

RSS<-read.csv("c:/users/ToBr708/dropbox/6.RainfallLegionella/results/2020/RSS.csv")
RSS$date<-as.Date(RSS$date)
RSS$referent.dates<-as.Date(RSS$referent.dates)

#2.Legionella-data (date of diagnosis and province)

legionella.cases<-read.csv("C:/users/ToBr708/dropbox/6.RainfallLegionella/data/legionella22082018.csv", header=TRUE, fill=TRUE, sep=";", quote="")
LD<-legionella.cases[c('DateUsedForStatistics', 'Province')]
colnames(LD)<-c('date', 'province')
LD$date<-as.Date(LD$date, format='%d/%m/%Y')
LD$province<-ifelse(LD$province=="Brussels-Capital Region", 'Brussels-Capital-Region', as.character(LD$province))


#3.Meteo-variables
# -Standardize the variables
#  -Aggregate on the province level
#  -Aggregate on the national level

meteoProv <- read.csv("c:/users/ToBr708/dropbox/6.RainfallLegionella/results/2020/meteoProv.csv")
meteoProv$date<-as.Date(meteoProv$date)
meteoProv<-na.omit(meteoProv)
meteoProv<-subset(meteoProv, select=-c(pressure, precipitation))
meteoBE <- read.csv("c:/users/ToBr708/dropbox/6.RainfallLegionella/results/2020/meteoBE2.csv")
meteoBE$date<-as.Date(meteoBE$date)


#############
#### Create the lists
#############

#General
number.of.repetitions = 100
number.of.cases=nrow(LD)
all.dates<-seq(as.Date('2004/01/01'), as.Date('2017/12/31'), by="day")

for (iter in c(1:50)){
  print(iter)
## 1. Scenario 01 (random dates, fixed exposures)

#create a list with 5000 elements, each element is random dates + provinces
i <- 1
scenario.01<-list()
while(i<number.of.repetitions) {
  scenario.01[[i]] <- data.table(date=sample(all.dates, number.of.cases, replace=T), province=sample(levels(meteoProv$province), number.of.cases, replace=T), ID=c(1:number.of.cases))
  i <- i + 1
}

#merge the elements of the list with the referents and subsequently with the meteo
scenario.01.RSS<-lapply(scenario.01, function (run){
  date.RSS <- merge(run, RSS, by='date')
  date.RSS.meteo.prov<-merge(date.RSS, meteoProv, by.x=c('province', 'referent.dates'), by.y=c('province', 'date'))
  date.RSS.meteo.BE<-merge(date.RSS[,province:=NULL], meteoBE, by.x=c('referent.dates'), by.y='date')
  rbind(date.RSS.meteo.prov, date.RSS.meteo.BE)
})

# saveRDS(scenario.01.RSS, "c:/users/ToBr708/dropbox/6.RainfallLegionella/results/2020/scenario01.Rdata")


library(pbapply)

#Scenario 01: no conditional poisson


##
# Conditional Logit
##

#Scenario 01
# scenario.01.RSS <-readRDS("c:/users/ToBr708/dropbox/6.RainfallLegionella/results/2020/scenario01.Rdata")

scenario.01.outputCL <- pblapply(scenario.01.RSS, function (run){
  run$case <- ifelse(grepl('risk', run$referents), 1, 0)
  run$national <- ifelse(grepl('Belgie', run$province), 1, 0)
  
  
  coef_run = run[, as.list(summary(clogit(case ~  temperature  + rel.humidity + wind.speed + strata(ID), method='efron'))$coef), by=c('RSS', 'national')]
  names(coef_run) = c('RSS', 'national', 'temp', 'rel.hum', 'wind.speed', 'exp.temp', 'exp.rel.hum', 'exp.wind.speed', 'se.temp', 'se.rel.hum', 'se.wind.speed', 'z.temp', 'z.rel.hum', 'z.wind.speed', 'p.temp', 'p.rel.hum', 'p.wind.speed')
  return(coef_run)
})

saveRDS(scenario.01.outputCL, paste("c:/users/ToBr708/dropbox/6.RainfallLegionella/results/2020/scenario01CL", iter, ".Rdata", sep=""))
}