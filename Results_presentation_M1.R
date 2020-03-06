library(ggplot2)
library(cowplot)
library(data.table)

# #alpha scenario01
# source("C:/users/tbraeye/dropbox/6.RainfallLegionella/R/2020/CC_scenario01_sim.R")

colors = c("blue", "green", "red2", "red1", "gold2", "gold3")
names(colors) = c('AY', 'AD', 'SM.cp', 'SM.m.cp', 'SY.cp', 'SY.m.cp')

ltys = c("longdash", "longdash", "solid", "dotted", "solid", "dotted")
names(ltys) = c('AY', 'AD', 'SM.cp', 'SM.m.cp', 'SY.cp', 'SY.m.cp')


###
#Load in the data
###

## First the analysis with all the outliers included.

#scenario 01: bias and type I error
RD.CL<-list()
RD.CP<-list()

for (iter in c(0:96)){
  scenario.01.outputCL <- readRDS(paste("c:/users/ToBr708/dropbox/6.RainfallLegionella/results/2020/simresults_M1/scenarioM1CL", iter, ".Rdata", sep=""))
  RD.CL <- c(RD.CL, scenario.01.outputCL)}
  RD.CL<-rbindlist(RD.CL)
  
  RD.CL<-subset(RD.CL, select=-c(exp.temp, exp.rel.hum, exp.wind.speed))

for (iter in c(0:96)){
  scenario.01.outputCP <- readRDS(paste("c:/users/ToBr708/dropbox/6.RainfallLegionella/results/2020/simresults_M1/scenarioM1CP", iter, ".Rdata", sep=""))
  RD.CP <- c(RD.CP, scenario.01.outputCP)}
  RD.CP<-rbindlist(RD.CP)
  
  colnames(RD.CP) <- names(RD.CL)
  RD.CP$RSS<-paste(RD.CP$RSS, '.cp', sep="")

RD<-rbind(RD.CL, RD.CP)

RD<-RD[RD$RSS!='S3W.cp' & RD$RSS!='S3W',]

p005 <- RD[, list(temp005=sum(p.temp<=0.05)/length(p.temp), rel.hum005=sum(p.rel.hum<=0.05)/length(p.rel.hum), wind.speed005=sum(p.wind.speed<=0.05)/length(p.wind.speed)), by=list(RSS, national, delay)]
p005$day <- as.numeric(p005$delay)+1
RD$day <- as.numeric(RD$delay)+1
RD$RSS <- factor(RD$RSS, levels = c("AD", "AY", "SM.cp", "SM.m.cp", "SY.cp", "SY.m.cp"))

##Province-values for all 3 variables
#temp
AlphaProvTemp<-ggplot(data=p005[p005$national==0,], aes(day, temp005)) + geom_line(aes(color=RSS, lty=RSS)) + labs(x='Lag (days)', y='Proportion of sign. coef.') +
   theme_bw() + scale_colour_manual(values = colors) + geom_vline(xintercept = 6, lty='dotted') + scale_linetype_manual(values=ltys) +
  coord_cartesian(ylim = c(-0, 0.7))
AlphaProvTemp

#rel.hum
AlphaProvRelHum<-ggplot(data=p005[p005$national==0,], aes(day, rel.hum005)) + geom_line(aes(color=RSS, lty=RSS)) + labs(x='Lag (days)', y='Proportion of sign. coef.') +
  theme_bw() + scale_colour_manual(values = colors) + geom_vline(xintercept = 6, lty='dotted') + scale_linetype_manual(values=ltys) +
  coord_cartesian(ylim = c(-0, 0.25))
AlphaProvRelHum

#wind.speed
AlphaProvWindSpeed<-ggplot(data=p005[p005$national==0,], aes(day, wind.speed005)) + geom_line(aes(color=RSS, lty=RSS)) + labs(x='Lag (days)', y='Proportion of sign. coef.') +
   theme_bw() + scale_colour_manual(values = colors) + geom_vline(xintercept = 6, lty='dotted') + scale_linetype_manual(values=ltys) +
  coord_cartesian(ylim = c(-0, 0.15))
  AlphaProvWindSpeed

##National-values for all 3 variables
#temp
AlphaNatTemp<-ggplot(data=p005[p005$national==1,], aes(day, temp005)) + geom_line(aes(color=RSS, lty=RSS)) + labs(x='Lag (days)', y='Proportion of sign. coef.') +
   theme_bw() + scale_colour_manual(values = colors) + geom_vline(xintercept = 6, lty='dotted') + scale_linetype_manual(values=ltys) +
  coord_cartesian(ylim = c(-0, 0.7))
AlphaNatTemp

#rel.hum
AlphaNatRelHum<-ggplot(data=p005[p005$national==1,], aes(day, rel.hum005)) + geom_line(aes(color=RSS, lty=RSS)) + labs(x='Lag (days)', y='Proportion of sign. coef.') +
  theme_bw() + scale_colour_manual(values = colors) + geom_vline(xintercept = 6, lty='dotted') + scale_linetype_manual(values=ltys) +
  coord_cartesian(ylim = c(-0, 0.25))
AlphaNatRelHum

#wind.speed
AlphaNatWindSpeed<-ggplot(data=p005[p005$national==1,], aes(day, wind.speed005)) + geom_line(aes(color=RSS, lty=RSS)) + labs(x='Lag (days)', y='Proportion of sign. coef.') +
   theme_bw() + scale_colour_manual(values = colors) + geom_vline(xintercept = 6, lty='dotted') + scale_linetype_manual(values=ltys) +
  coord_cartesian(ylim = c(-0, 0.15))
  AlphaNatWindSpeed


###
# BIAS
###

#Province
BiasProvTemp<-ggplot(data=RD[RD$national==0 & RD$day==6 ,], aes(RSS, temp)) + geom_boxplot(aes(color=RSS)) + theme_bw() +
  labs(y='Coefficient', x="")  + theme(legend.position='bottom') + guides(colour = guide_legend(nrow = 1))  + scale_colour_manual(values = colors) + scale_linetype_manual(values=ltys) +
  geom_hline(yintercept=0.1, lty='dotted') + scale_x_discrete(labels= c('AD', 'AY', 'SM.cp', 'SM.m.cp', 'SY.cp', 'SY.m.cp'))  + coord_cartesian(ylim = c(-0.1, 0.3))
BiasProvTemp

BiasProvRelHum<-ggplot(data=RD[RD$national==0 & RD$day==6 ,], aes(RSS, rel.hum)) + geom_boxplot(aes(color=RSS)) + geom_hline(yintercept=0.02, lty='dotted') + theme_bw() +
  labs(y='Coefficient', x="")  + theme(legend.position='bottom') + guides(colour = guide_legend(nrow = 1))  + scale_colour_manual(values = colors) + scale_linetype_manual(values=ltys) +
   scale_x_discrete(labels= c('AD', 'AY', 'SM.cp', 'SM.m.cp', 'SY.cp', 'SY.m.cp'))  + coord_cartesian(ylim = c(-0.1, 0.15))
BiasProvRelHum

BiasProvWindSpeed<-ggplot(data=RD[RD$national==0 & RD$day==6 ,], aes(RSS, wind.speed)) + geom_boxplot(aes(color=RSS)) + theme_bw() +
  labs(y='Coefficient', x="")  + theme(legend.position='bottom') + guides(colour = guide_legend(nrow = 1))  + scale_colour_manual(values = colors) + scale_linetype_manual(values=ltys) +
  geom_hline(yintercept=0, lty='dotted') + scale_x_discrete(labels= c('AD', 'AY', 'SM.cp', 'SM.m.cp', 'SY.cp', 'SY.m.cp'))  + coord_cartesian(ylim = c(-0.12, 0.12))
BiasProvWindSpeed


#Create a plot alpha 2.1 
BiasNatTemp<-ggplot(data=RD[RD$national==1 & RD$day==6 ,], aes(RSS, temp)) + geom_boxplot(aes(color=RSS)) + theme_bw() +
  labs(y='Coefficient', x="")  + theme(legend.position='bottom') + guides(colour = guide_legend(nrow = 1))  + scale_colour_manual(values = colors) + scale_linetype_manual(values=ltys) +
  geom_hline(yintercept=0.1, lty='dotted') + scale_x_discrete(labels= c('AD', 'AY', 'SM.cp', 'SM.m.cp', 'SY.cp', 'SY.m.cp'))  + coord_cartesian(ylim = c(-0.1, 0.3))
BiasNatTemp

BiasNatRelHum<-ggplot(data=RD[RD$national==1 & RD$day==6 ,], aes(RSS, rel.hum)) + geom_boxplot(aes(color=RSS)) + theme_bw() +
  labs(y='Coefficient', x="")  + theme(legend.position='bottom') + guides(colour = guide_legend(nrow = 1))  + scale_colour_manual(values = colors) + scale_linetype_manual(values=ltys) +
  geom_hline(yintercept=0.02, lty='dotted') + scale_x_discrete(labels= c('AD', 'AY', 'SM.cp', 'SM.m.cp', 'SY.cp', 'SY.m.cp'))  + coord_cartesian(ylim = c(-0.1, 0.15))
BiasNatRelHum

BiasNatWindSpeed<-ggplot(data=RD[RD$national==1 & RD$day==6 ,], aes(RSS, wind.speed)) + geom_boxplot(aes(color=RSS)) + theme_bw() +
  labs(y='Coefficient', x="")  + theme(legend.position='bottom') + guides(colour = guide_legend(nrow = 1))  + scale_colour_manual(values = colors) + scale_linetype_manual(values=ltys) +
  geom_hline(yintercept=0, lty='dotted') + scale_x_discrete(labels= c('AD', 'AY', 'SM.cp', 'SM.m.cp', 'SY.cp', 'SY.m.cp'))  + coord_cartesian(ylim = c(-0.12, 0.12))
BiasNatWindSpeed


#combine the alfa and bias plots into three figures (one for each variable)

extractLegend <- function(gg) {
  grobs <- ggplot_gtable(ggplot_build(gg))
  foo <- which(sapply(grobs$grobs, function(x) x$name) == "guide-box")
  grobs$grobs[[foo]]
}

legend<-extractLegend(AlphaNatTemp + theme(legend.position='bottom') + guides(colour = guide_legend(nrow = 1)))

title.prov <- ggdraw() + draw_label("Province")
title.be <- ggdraw() + draw_label("Belgium")

title.temp <- ggdraw() + draw_label("Temperature", fontface='bold')
title.rel.hum <- ggdraw() + draw_label("Rel. Humidity", fontface='bold')
title.wind.speed <- ggdraw() + draw_label("Wind Speed", fontface='bold')

temp.plot<-plot_grid(title.be, title.prov, BiasNatTemp + theme(legend.position = "none"), BiasProvTemp + theme(legend.position = "none"), AlphaNatTemp + theme(legend.position = "none"), AlphaProvTemp + theme(legend.position = "none"), ncol=2, nrow=3, labels=c('','','A', 'B', 'C', 'D'), rel_heights=c(0.1,1,1))
rel.hum.plot<-plot_grid(title.be, title.prov, BiasNatRelHum + theme(legend.position = "none"), BiasProvRelHum + theme(legend.position = "none"),AlphaNatRelHum + theme(legend.position = "none"), AlphaProvRelHum + theme(legend.position = "none"),  ncol=2, labels=c('','','A', 'B', 'C', 'D'), rel_heights=c(0.1,1,1))
wind.speed.plot<-plot_grid(title.be, title.prov, BiasNatWindSpeed + theme(legend.position = "none"), BiasProvWindSpeed + theme(legend.position = "none"), AlphaNatWindSpeed + theme(legend.position = "none"), AlphaProvWindSpeed + theme(legend.position = "none"),  ncol=2, labels=c('','','A', 'B', 'C', 'D'), rel_heights=c(0.1,1,1))

tempF<-plot_grid(title.temp, temp.plot, legend, ncol=1, rel_heights = c(0.1,2,0.1))
relhumF<-plot_grid(title.rel.hum, rel.hum.plot, legend, ncol=1, rel_heights = c(0.1,2,0.1))
windspeedF<-plot_grid(title.wind.speed, wind.speed.plot, legend, ncol=1, rel_heights = c(0.1,2,0.1))


##Save the plots
#Temp
save_plot("C:/Users/ToBr708/dropbox/6.RainfallLegionella/results/2020/fig7_1_temp.tiff", tempF, 
          ncol = 1.5, # we're saving a grid plot of 2 columns
          nrow = 2.3, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1.3)

save_plot("C:/Users/ToBr708/dropbox/6.RainfallLegionella/results/2020/fig7_1_temp.eps", tempF, 
          ncol = 1.5, # we're saving a grid plot of 2 columns
          nrow = 2.3, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1.3)

#Temp
save_plot("C:/Users/ToBr708/dropbox/6.RainfallLegionella/results/2020/fig7_2_relhum.tiff", relhumF, 
          ncol = 1.5, # we're saving a grid plot of 2 columns
          nrow = 2.3, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1.3)

save_plot("C:/Users/ToBr708/dropbox/6.RainfallLegionella/results/2020/fig7_2_relhum.eps", relhumF, 
          ncol = 1.5, # we're saving a grid plot of 2 columns
          nrow = 2.3, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1.3)

#WindSpeed
save_plot("C:/Users/ToBr708/dropbox/6.RainfallLegionella/results/2020/fig7_3_windspeed.tiff", windspeedF, 
          ncol = 1.5, # we're saving a grid plot of 2 columns
          nrow = 2.3, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1.3)

save_plot("C:/Users/ToBr708/dropbox/6.RainfallLegionella/results/2020/fig7_3_windspeed.eps", windspeedF, 
          ncol = 1.5, # we're saving a grid plot of 2 columns
          nrow = 2.3, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1.3)

