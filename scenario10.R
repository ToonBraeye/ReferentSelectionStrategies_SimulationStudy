
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
meteoProv<-subset(meteoProv, select=-c(pressure, precipitation))
meteoProv<-na.omit(meteoProv)
meteoBE <- read.csv("c:/users/ToBr708/dropbox/6.RainfallLegionella/results/2020/meteoBE2.csv")
meteoBE$date<-as.Date(meteoBE$date)
meteoBE<-subset(meteoBE, select=-c(pressure, precipitation))


#############
#### Create the lists
#############

#General
number.of.repetitions = 100
number.of.cases=nrow(LD)
all.dates<-seq(as.Date('2004/01/01'), as.Date('2017/12/31'), by="day")


## 2. Scenario 10 (fixed dates, permuted exposures)

for (iter in c(1:50)){
print(iter)
#create a list with 5000 elements, each element is random dates + provinces
i <- 1
scenario.10<-list()
while(i<number.of.repetitions) {
  scenario.10[[i]] <- data.table(date=LD$date, province=LD$province, ID=c(1:number.of.cases))
  i <- i + 1
}

#merge the elements of the list with the referents and subsequently with the meteo
scenario.10.RSS<-lapply(scenario.10, function (run){
  date.RSS <- merge(run, RSS, by='date')
  
  meteoProvR<-meteoProv
  meteoProvR[c('date', 'province')]<-meteoProvR[sample(c(1:nrow(meteoProvR))), c('date', 'province')]
  meteoBER<-meteoBE
  meteoBER$date<-sample(meteoBER$date, nrow(meteoBER), replace=F)
  
  date.RSS.meteo.prov<-merge(date.RSS, meteoProvR, by.x=c('province', 'referent.dates'), by.y=c('province', 'date'))
  date.RSS.meteo.BE<-merge(date.RSS[,province:=NULL], meteoBER, by.x=c('referent.dates'), by.y='date')
  rbind(date.RSS.meteo.prov, date.RSS.meteo.BE)
})

# saveRDS(scenario.10.RSS, "c:/users/ToBr708/dropbox/6.RainfallLegionella/results/2020/scenario10.Rdata")

##
# Data analysiss
##

### Conditional Logit Analysis

#Scenario 10
# scenario.10.RSS <-readRDS("c:/users/ToBr708/dropbox/6.RainfallLegionella/results/2020/scenario10.Rdata")


scenario.10.outputCL <- pblapply(scenario.10.RSS, function (run){
  run$case <- ifelse(grepl('risk', run$referents), 1, 0)
  run$national <- ifelse(grepl('Belgie', run$province), 1, 0)

  coef_run = run[, as.list(summary(clogit(case ~  temperature  + rel.humidity + wind.speed + strata(ID), method='efron'))$coef), by=c('RSS', 'national')]
  names(coef_run) = c('RSS', 'national', 'temp', 'rel.hum', 'wind.speed', 'exp.temp', 'exp.rel.hum', 'exp.wind.speed', 'se.temp', 'se.rel.hum', 'se.wind.speed', 'z.temp', 'z.rel.hum', 'z.wind.speed', 'p.temp', 'p.rel.hum', 'p.wind.speed')
  return(coef_run)
})

saveRDS(scenario.10.outputCL, paste("c:/users/ToBr708/dropbox/6.RainfallLegionella/results/2020/scenario10CL", iter, ".Rdata", sep=""))

#Scenario 10 + Outliers Removed

scenario.10.outputCLNoOutliers <- pblapply(scenario.10.RSS, function (run){
  run$case <- ifelse(grepl('risk', run$referents), 1, 0)
  run$national <- ifelse(grepl('Belgie', run$province), 1, 0)

  # Outlier Removal
  run<-run[!(run$date %in% c(as.Date('2004/10/29'), as.Date('2011/08/19'), as.Date('2016/09/13'),as.Date('2006/12/25'))),]

  coef_run = run[, as.list(summary(clogit(case ~  temperature  + rel.humidity + wind.speed + strata(ID), method='efron'))$coef), by=c('RSS', 'national')]
  names(coef_run) = c('RSS', 'national', 'temp', 'rel.hum', 'wind.speed', 'exp.temp', 'exp.rel.hum', 'exp.wind.speed', 'se.temp', 'se.rel.hum', 'se.wind.speed', 'z.temp', 'z.rel.hum', 'z.wind.speed', 'p.temp', 'p.rel.hum', 'p.wind.speed')
  return(coef_run)
})

saveRDS(scenario.10.outputCLNoOutliers, paste("c:/users/ToBr708/dropbox/6.RainfallLegionella/results/2020/scenario10CLNoOutliers", iter, ".Rdata", sep=""))

#### Conditional Poisson Analysis

scenario.10.outputCP <- pblapply(scenario.10.RSS, function (run){
  run<-run[!(run$RSS %in% c('AD', 'AY')),]
  
  run$case <- ifelse(grepl('risk', run$referents), 1, 0)
  run$national <- ifelse(grepl('Belgie', run$province), 1, 0)
  
  run.agg = run[,list(count = sum(case)), by = c('referent.dates', 'RSS', 'province', 'national', 'temperature', 'rel.humidity', 'wind.speed')]
  
  ##Define ID.strata
  
  run.agg$ID.strata <- ifelse (run.agg$RSS == 'SY', paste('strata', format(run.agg$referent.dates, '%j'), run.agg$province), ifelse(run.agg$RSS == 'SM', 
                                                                                                                                    paste('strata', format(run.agg$referent.dates, '%A'), format(run.agg$referent.dates, '%m'), format(run.agg$referent.dates, '%Y'), run.agg$province),
                                                                                                                                    paste('strata', floor((as.numeric(run.agg$referent.dates-as.Date('2004-01-01'))/7)/3), run.agg$province)))
  
  
  run.agg$year <- format(run.agg$date, "%Y")
  
  coef_run = run.agg[, as.list(summary(gnm(count ~  temperature + rel.humidity + wind.speed, family=quasipoisson,
                                           eliminate=factor(ID.strata)))$coef), by=c('RSS', 'national')]
  names(coef_run) = c('RSS', 'national', 'temp', 'rel.hum', 'wind.speed', 
                      'se.temp', 'se.rel.hum', 'se.wind.speed', 
                      't.temp', 't.rel.hum', 't.wind.speed', 
                      'p.temp', 'p.rel.hum', 'p.wind.speed')
  
  return(coef_run)
})

saveRDS(scenario.10.outputCP, paste("c:/users/ToBr708/dropbox/6.RainfallLegionella/results/2020/scenario10CP", iter, ".Rdata", sep=""))

#NO Outliers
scenario.10.outputCPNoOutliers <- pblapply(scenario.10.RSS, function (run){
  run<-run[!(run$RSS %in% c('AD', 'AY')),]
  
  run$case <- ifelse(grepl('risk', run$referents), 1, 0)
  run$national <- ifelse(grepl('Belgie', run$province), 1, 0)
  
  # Outlier Removal
  run<-run[!(run$date %in% c(as.Date('2004/10/29'), as.Date('2011/08/19'), as.Date('2016/09/13'),as.Date('2006/12/25'))),]
  
  run.agg = run[,list(count = sum(case)), by = c('referent.dates', 'RSS', 'province', 'national', 'temperature', 'rel.humidity', 'wind.speed')]
  
  ##Define ID.strata
  
  run.agg$ID.strata <- ifelse (run.agg$RSS == 'SY', paste('strata', format(run.agg$referent.dates, '%j'), run.agg$province), ifelse(run.agg$RSS == 'SM', 
                                                                                                                                    paste('strata', format(run.agg$referent.dates, '%A'), format(run.agg$referent.dates, '%m'), format(run.agg$referent.dates, '%Y'), run.agg$province),
                                                                                                                                    paste('strata', floor((as.numeric(run.agg$referent.dates-as.Date('2004-01-01'))/7)/3), run.agg$province)))
  
  run.agg$year <- format(run.agg$date, "%Y")
  
  coef_run = run.agg[, as.list(summary(gnm(count ~  temperature + rel.humidity + wind.speed, family=quasipoisson,
                                           eliminate=factor(ID.strata)))$coef), by=c('RSS', 'national')]
  names(coef_run) = c('RSS', 'national', 'temp', 'rel.hum', 'wind.speed', 
                      'se.temp', 'se.rel.hum', 'se.wind.speed', 
                      't.temp', 't.rel.hum', 't.wind.speed', 
                      'p.temp', 'p.rel.hum', 'p.wind.speed')
  
  return(coef_run)
})

saveRDS(scenario.10.outputCPNoOutliers, paste("c:/users/ToBr708/dropbox/6.RainfallLegionella/results/2020/scenario10CPNoOutliers", iter, ".Rdata", sep=""))
}

