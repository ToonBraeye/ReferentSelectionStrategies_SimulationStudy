
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
meteoBE <- read.csv("c:/users/ToBr708/dropbox/6.RainfallLegionella/results/2020/meteoBE2.csv")
meteoBE$date<-as.Date(meteoBE$date)


#############
#### Create the lists
#############

#General
number.of.repetitions = 50
number.of.cases=nrow(LD)
all.dates<-seq(as.Date('2004/01/01'), as.Date('2017/12/31'), by="day")


## 3. Scenario M1 (Modelled event probabilities)

#create a list with 5000 elements, each element is random dates + provinces
all.dates.meteoProv<-merge(meteoProv, data.table('date'=all.dates), by='date')
all.dates.meteoBE<-merge(meteoBE, data.table('date'=all.dates), by='date')
meteo.all<-rbind(meteoProv, meteoBE)

all.dates.meteoProv$trend <- as.numeric(all.dates.meteoProv$date-as.Date('2003/12/25'))
all.dates.meteoProv$doy<-as.numeric(format(all.dates.meteoProv$date, "%j"))
all.dates.meteoProv$weight.id.pos <- -0.1207*sin(2*3.1415/365.7*all.dates.meteoProv$doy)-0.09609*cos(2*3.1415/365.7*all.dates.meteoProv$doy)+1
all.dates.meteoProv$prob <- exp(all.dates.meteoProv$temperature*0.1+all.dates.meteoProv$rel.humidity*0.02 + all.dates.meteoProv$weight.id.pos + all.dates.meteoProv$trend*0.0001)# + as.numeric(format(all.dates.all.meteo$all.dates, "%u"))*0.1),
all.dates.meteoProv<-na.omit(all.dates.meteoProv)

all.dates.meteoBE$trend <- as.numeric(all.dates.meteoBE$date-as.Date('2003/12/25'))
all.dates.meteoBE$doy<-as.numeric(format(all.dates.meteoBE$date, "%j"))
all.dates.meteoBE$weight.id.pos <- -0.1207*sin(2*3.1415/365.7*all.dates.meteoBE$doy)-0.09609*cos(2*3.1415/365.7*all.dates.meteoBE$doy)+1
all.dates.meteoBE$prob <- exp(all.dates.meteoBE$temperature*0.1+all.dates.meteoBE$rel.humidity*0.02 + all.dates.meteoBE$weight.id.pos + all.dates.meteoBE$trend*0.0001)# + as.numeric(format(all.dates.all.meteo$all.dates, "%u"))*0.1),
all.dates.meteoBE<-na.omit(all.dates.meteoBE)

for (iter in c(1:100)){
print(iter)
i <- 1
scenario.M1<-list()
while(i<number.of.repetitions) {
  all.dates.meteoProv.S<-data.table(all.dates.meteoProv[sample(c(1:nrow(all.dates.meteoProv)), size=number.of.cases, prob=all.dates.meteoProv$prob, replace=T), c('date', 'province')])
  all.dates.meteoProv.S$date0<-all.dates.meteoProv.S$date+6
  all.dates.meteoProv.S <- all.dates.meteoProv.S[, date:=NULL]
  all.dates.meteoProv.S$ID<-paste('Prov', c(1:number.of.cases))
  all.dates.meteoProv.S$date2 <- all.dates.meteoProv.S$date0-2
  all.dates.meteoProv.S$date3 <- all.dates.meteoProv.S$date0-3
  all.dates.meteoProv.S$date4 <- all.dates.meteoProv.S$date0-4
  all.dates.meteoProv.S$date5 <- all.dates.meteoProv.S$date0-5
  all.dates.meteoProv.S$date6 <- all.dates.meteoProv.S$date0-6
  all.dates.meteoProv.S$date7 <- all.dates.meteoProv.S$date0-7
  all.dates.meteoProv.S$date8 <- all.dates.meteoProv.S$date0-8
  all.dates.meteoProv.S$date9 <- all.dates.meteoProv.S$date0-9
  all.dates.meteoProv.S$date10 <- all.dates.meteoProv.S$date0-10
  all.dates.meteoProv.S <- melt(all.dates.meteoProv.S, measure.vars = c("date2", "date3", "date4","date5", "date6", "date7","date8", "date9", "date10"),
                                variable.name = "delay", value.name = "date")
  
  all.dates.meteoBE.S<-data.table(all.dates.meteoProv[sample(c(1:nrow(all.dates.meteoProv)), size=number.of.cases, prob=all.dates.meteoProv$prob, replace=T), c('date', 'province')])
  all.dates.meteoBE.S$province<-'Belgie'
  all.dates.meteoBE.S$date0<-all.dates.meteoBE.S$date+6
  all.dates.meteoBE.S <- all.dates.meteoBE.S[, date:=NULL]
  all.dates.meteoBE.S$ID<-paste('BE', c(1:number.of.cases))
  all.dates.meteoBE.S$date2 <- all.dates.meteoBE.S$date0-2
  all.dates.meteoBE.S$date3 <- all.dates.meteoBE.S$date0-3
  all.dates.meteoBE.S$date4 <- all.dates.meteoBE.S$date0-4
  all.dates.meteoBE.S$date5 <- all.dates.meteoBE.S$date0-5
  all.dates.meteoBE.S$date6 <- all.dates.meteoBE.S$date0-6
  all.dates.meteoBE.S$date7 <- all.dates.meteoBE.S$date0-7
  all.dates.meteoBE.S$date8 <- all.dates.meteoBE.S$date0-8
  all.dates.meteoBE.S$date9 <- all.dates.meteoBE.S$date0-9
  all.dates.meteoBE.S$date10 <- all.dates.meteoBE.S$date0-10
  all.dates.meteoBE.S <- melt(all.dates.meteoBE.S, measure.vars = c("date2", "date3", "date4","date5", "date6", "date7","date8", "date9", "date10"),
                              variable.name = "delay", value.name = "date")
  
  scenario.M1[[i]] <- rbind(all.dates.meteoProv.S[, c('date', 'delay', 'province', 'ID'), with=FALSE], all.dates.meteoBE.S[, c('date', 'delay', 'province', 'ID'), with=FALSE])
  i<-i+1
}

#merge the elements of the list with the referents and subsequently with the meteo
scenario.M1.RSS<-lapply(scenario.M1, function (run){
  date.RSS <- merge(run, RSS, by='date', allow.cartesian=TRUE)
  merge(date.RSS, meteo.all, by.x=c('province', 'referent.dates'), by.y=c('province', 'date'))
})


##
# Data analysiss
##

### Conditional Logit Analysis

# scenario.M1.RSS <-readRDS("c:/users/ToBr708/dropbox/6.RainfallLegionella/results/2020/scenarioM1.Rdata")


scenario.M1.outputCL <- pblapply(scenario.M1.RSS, function (run){
  run$case <- ifelse(grepl('risk', run$referents), 1, 0)
  run$national <- ifelse(grepl('Belgie', run$province), 1, 0)

  #subset to only AD & AY
  run<-run[run$RSS =='AD' | run$RSS == 'AY',]
  run$ID.CL<-paste(run$ID, run$delay)

  coef_run = run[, as.list(summary(clogit(case ~  temperature  + rel.humidity + wind.speed + strata(ID.CL), method='efron'))$coef), by=c('RSS', 'national', 'delay')]
  names(coef_run) = c('RSS', 'national', 'delay', 'temp', 'rel.hum', 'wind.speed', 'exp.temp', 'exp.rel.hum', 'exp.wind.speed', 'se.temp', 'se.rel.hum', 'se.wind.speed', 'z.temp', 'z.rel.hum', 'z.wind.speed', 'p.temp', 'p.rel.hum', 'p.wind.speed')
  return(coef_run)
})

saveRDS(scenario.M1.outputCL, paste("c:/users/ToBr708/dropbox/6.RainfallLegionella/results/2020/scenarioM1CL", iter, ".Rdata", sep=""))



#### Conditional Poisson Analysis (Unmodelled & Modelled)

scenario.M1.outputCP <- pblapply(scenario.M1.RSS, function (run){
  run<-run[(run$RSS %in% c('SM', 'SY', 'S3W')),]
  
  run$case <- ifelse(grepl('risk', run$referents), 1, 0)
  run$national <- ifelse(grepl('Belgie', run$province), 1, 0)
  
  run.agg = run[,list(count = sum(case)), by = c('referent.dates', 'delay', 'RSS', 'province', 'national', 'temperature', 'rel.humidity', 'wind.speed')]
  
  ##Define ID.strata
  
  run.agg$ID.strata <- ifelse (run.agg$RSS == 'SY', paste('strata', format(run.agg$referent.dates, '%j'), run.agg$province), ifelse(run.agg$RSS == 'SM', 
                                                                                                                                    paste('strata', format(run.agg$referent.dates, '%A'), format(run.agg$referent.dates, '%m'), format(run.agg$referent.dates, '%Y'), run.agg$province),
                                                                                                                                    paste('strata', floor((as.numeric(run.agg$referent.dates-as.Date('2004-01-01'))/7)/3), run.agg$province)))
  
  ##Unmodelled
  
  coef_run = run.agg[, as.list(summary(gnm(count ~  temperature + rel.humidity + wind.speed, family=quasipoisson,
                                           eliminate=factor(ID.strata)))$coef), by=c('RSS', 'national', 'delay')]
  names(coef_run) = c('RSS', 'national', 'delay', 'temp', 'rel.hum', 'wind.speed', 
                      'se.temp', 'se.rel.hum', 'se.wind.speed', 
                      't.temp', 't.rel.hum', 't.wind.speed', 
                      'p.temp', 'p.rel.hum', 'p.wind.speed')
  
  ##SY modelled
  
  run.aggSY<-run.agg[run.agg$RSS=='SY',]
  run.aggSY$year <- as.numeric(format(run.aggSY$referent.dates, "%Y"))-2003
  
  coef_runSY = run.aggSY[, as.list(summary(gnm(count ~  temperature + rel.humidity + wind.speed + year, family=quasipoisson,
                                           eliminate=factor(ID.strata)))$coef), by=c('RSS', 'national', 'delay')]
   names(coef_runSY) = c('RSS', 'national', 'delay', 'temp', 'rel.hum', 'wind.speed', 'year',
                        'se.temp', 'se.rel.hum', 'se.wind.speed', 'se.year',
                        't.temp', 't.rel.hum', 't.wind.speed', 't.year',
                        'p.temp', 'p.rel.hum', 'p.wind.speed', 'p.year')
  coef_runSY = coef_runSY[, (grep('year',  colnames(coef_runSY))):=NULL]
  coef_runSY$RSS<-paste(coef_runSY$RSS, '.m', sep="")
  
  
  ##SM modelled
  run.aggSM<-run.agg[run.agg$RSS=='SM',]
  run.aggSM$doy <- as.numeric(format(run.aggSM$referent.dates, "%j"))
  
  coef_runSM = run.aggSM[, as.list(summary(gnm(count ~  temperature + rel.humidity + wind.speed + sin(2*pi*(doy/365))+cos(2*pi*(doy/365)), family=quasipoisson,
                                               eliminate=factor(ID.strata)))$coef), by=c('RSS', 'national', 'delay')]
  names(coef_runSM) = c('RSS', 'national', 'delay', 'temp', 'rel.hum', 'wind.speed', 'sin', 'cosin',
                        'se.temp', 'se.rel.hum', 'se.wind.speed', 'sesin', 'secosin',
                        't.temp', 't.rel.hum', 't.wind.speed', 'tsin', 'tcosin',
                        'p.temp', 'p.rel.hum', 'p.wind.speed', 'psin', 'pcosin')
  coef_runSM = coef_runSM[, (grep('sin',  colnames(coef_runSM))):=NULL]
  coef_runSM$RSS<-paste(coef_runSM$RSS, '.m', sep="")
  
  coef_run<-rbind(coef_run, coef_runSY, coef_runSM)
  
  return(coef_run)
})

saveRDS(scenario.M1.outputCP, paste("c:/users/ToBr708/dropbox/6.RainfallLegionella/results/2020/scenarioM1CP", iter, ".Rdata", sep=""))

}

