library(ggplot2)
library(cowplot)

# #alpha scenario01
# source("C:/users/tbraeye/dropbox/6.RainfallLegionella/R/2020/CC_scenario01_sim.R")

colors = c("blue", "green", "red2", "red1", "gold2", "gold3")
names(colors) = c('AY', 'AD', 'SM', 'SM.cp', 'SY', 'SY.cp')

ltys = c("longdash", "longdash", "solid", "dotted", "solid", "dotted")
names(ltys) = c('AY', 'AD', 'SM', 'SM.cp', 'SY', 'SY.cp')


###
#Load in the data
###

## First the analysis with all the outliers included.

#scenario 01: bias and type I error
RD.CL<-list()
RD.CP<-list()

for (iter in c(1:50)){
  scenario.01.outputCL <- readRDS(paste("c:/users/ToBr708/dropbox/6.RainfallLegionella/results/2020/simresults_10/scenario10CL", iter, ".Rdata", sep=""))
  RD.CL <- c(RD.CL, scenario.01.outputCL)}
  RD.CL<-rbindlist(RD.CL)
  
  RD.CL<-subset(RD.CL, select=-c(exp.temp, exp.rel.hum, exp.wind.speed))

for (iter in c(1:50)){
  scenario.01.outputCP <- readRDS(paste("c:/users/ToBr708/dropbox/6.RainfallLegionella/results/2020/simresults_10/scenario10CP", iter, ".Rdata", sep=""))
  RD.CP <- c(RD.CP, scenario.01.outputCP)}
  RD.CP<-rbindlist(RD.CP)
  
  colnames(RD.CP) <- names(RD.CL)
  RD.CP$RSS<-paste(RD.CP$RSS, '.cp', sep="")

RD<-rbind(RD.CL, RD.CP)

RD<-RD[RD$RSS!='S3W.cp' & RD$RSS!='S3W',]
RD$RSS <- factor(RD$RSS, levels = c("AD", "AY", "SM", "SY", "SM.cp", "SY.cp"))


p005 <- RD[, list(temp005=sum(p.temp<=0.05)/length(p.temp), rel.hum005=sum(p.rel.hum<=0.05)/length(p.rel.hum), wind.speed005=sum(p.wind.speed<=0.05)/length(p.wind.speed)), by=list(RSS, national)]
p004 <- RD[, list(temp004=sum(p.temp<=0.04)/length(p.temp), rel.hum004=sum(p.rel.hum<=0.04)/length(p.rel.hum), wind.speed004=sum(p.wind.speed<=0.04)/length(p.wind.speed)), by=list(RSS, national)]
p003 <- RD[, list(temp003=sum(p.temp<=0.03)/length(p.temp), rel.hum003=sum(p.rel.hum<=0.03)/length(p.rel.hum), wind.speed003=sum(p.wind.speed<=0.03)/length(p.wind.speed)), by=list(RSS, national)]
p002 <- RD[, list(temp002=sum(p.temp<=0.02)/length(p.temp), rel.hum002=sum(p.rel.hum<=0.02)/length(p.rel.hum), wind.speed002=sum(p.wind.speed<=0.02)/length(p.wind.speed)), by=list(RSS, national)]

p005.all <- p005[, list(y=mean(temp005, rel.hum005, wind.speed005), x=0.05), by=list(RSS, national)]
p004.all <- p004[, list(y=mean(temp004, rel.hum004, wind.speed004), x=0.04), by=list(RSS, national)]
p003.all <- p003[, list(y=mean(temp003, rel.hum003, wind.speed003), x=0.03), by=list(RSS, national)]
p002.all <- p002[, list(y=mean(temp002, rel.hum002, wind.speed002), x=0.02), by=list(RSS, national)]


RD_alpha<-rbind(p005.all, p004.all, p003.all, p002.all)

###
# Plotting alpha
###

#Create a plot alpha 2.1 
alphapowerProv<-ggplot(data=RD_alpha[RD_alpha$national==0,], aes(x, y)) + geom_line(aes(color=RSS, lty=RSS)) + labs(x='Nominal individual significance level', y='Proportion of sign. coef.') +
  ggtitle('Random events, fixed exposure') + theme_bw() + scale_colour_manual(values = colors) + geom_abline(intercept = 0, slope=1, lty='dotted') + scale_linetype_manual(values=ltys) + coord_cartesian(ylim=c(0.02,0.08))
alphapowerProv
# ggsave("c:/users/tbraeye/dropbox/6.RainfallLegionella/results/2020/alphascenario01.tiff", alphapower)

alphapowerBE<-ggplot(data=RD_alpha[RD_alpha$national==1,], aes(x, y)) + geom_abline(intercept = 0, slope=1, lty='dotted') + geom_line(aes(color=RSS, lty=RSS)) + labs(x='Nominal individual significance level', y='Proportion of sign. coef.') +
  ggtitle('Random events, fixed exposure') + theme_bw() + scale_colour_manual(values = colors)  + scale_linetype_manual(values=ltys) + coord_cartesian(ylim=c(0.02,0.2))
alphapowerBE


## Second the analysis with NO outliers.

#scenario 01: bias and type I error
RD.CLNoOutliers<-list()
RD.CPNoOutliers<-list()

for (iter in c(1:50)){
  scenario.01.outputCLNoOutliers <- readRDS(paste("c:/users/ToBr708/dropbox/6.RainfallLegionella/results/2020/simresults_10/scenario10CLNoOutliers", iter, ".Rdata", sep=""))
  RD.CLNoOutliers <- c(RD.CLNoOutliers, scenario.01.outputCLNoOutliers)}
RD.CLNoOutliers<-rbindlist(RD.CLNoOutliers)

RD.CLNoOutliers<-subset(RD.CLNoOutliers, select=-c(exp.temp, exp.rel.hum, exp.wind.speed))

for (iter in c(1:50)){
  scenario.01.outputCPNoOutliers <- readRDS(paste("c:/users/ToBr708/dropbox/6.RainfallLegionella/results/2020/simresults_10/scenario10CPNoOutliers", iter, ".Rdata", sep=""))
  RD.CPNoOutliers <- c(RD.CPNoOutliers, scenario.01.outputCPNoOutliers)}
RD.CPNoOutliers<-rbindlist(RD.CPNoOutliers)

colnames(RD.CPNoOutliers) <- names(RD.CLNoOutliers)
RD.CPNoOutliers$RSS<-paste(RD.CPNoOutliers$RSS, '.cp', sep="")

RDNoOutliers<-rbind(RD.CLNoOutliers, RD.CPNoOutliers)

RDNoOutliers<-RDNoOutliers[RDNoOutliers$RSS!='S3W.cp' & RDNoOutliers$RSS!='S3W',]
RDNoOutliers$RSS <- factor(RDNoOutliers$RSS, levels = c("AD", "AY", "SM", "SY", "SM.cp", "SY.cp"))


p005 <- RDNoOutliers[, list(temp005=sum(p.temp<=0.05)/length(p.temp), rel.hum005=sum(p.rel.hum<=0.05)/length(p.rel.hum), wind.speed005=sum(p.wind.speed<=0.05)/length(p.wind.speed)), by=list(RSS, national)]
p004 <- RDNoOutliers[, list(temp004=sum(p.temp<=0.04)/length(p.temp), rel.hum004=sum(p.rel.hum<=0.04)/length(p.rel.hum), wind.speed004=sum(p.wind.speed<=0.04)/length(p.wind.speed)), by=list(RSS, national)]
p003 <- RDNoOutliers[, list(temp003=sum(p.temp<=0.03)/length(p.temp), rel.hum003=sum(p.rel.hum<=0.03)/length(p.rel.hum), wind.speed003=sum(p.wind.speed<=0.03)/length(p.wind.speed)), by=list(RSS, national)]
p002 <- RDNoOutliers[, list(temp002=sum(p.temp<=0.02)/length(p.temp), rel.hum002=sum(p.rel.hum<=0.02)/length(p.rel.hum), wind.speed002=sum(p.wind.speed<=0.02)/length(p.wind.speed)), by=list(RSS, national)]

p005.all <- p005[, list(y=mean(temp005, rel.hum005, wind.speed005), x=0.05), by=list(RSS, national)]
p004.all <- p004[, list(y=mean(temp004, rel.hum004, wind.speed004), x=0.04), by=list(RSS, national)]
p003.all <- p003[, list(y=mean(temp003, rel.hum003, wind.speed003), x=0.03), by=list(RSS, national)]
p002.all <- p002[, list(y=mean(temp002, rel.hum002, wind.speed002), x=0.02), by=list(RSS, national)]


RD_alpha<-rbind(p005.all, p004.all, p003.all, p002.all)

###
# Plotting alpha
###

#Create a plot alpha 2.1 
alphapowerProvNoOutliers<-ggplot(data=RD_alpha[RD_alpha$national==0,], aes(x, y)) + geom_line(aes(color=RSS, lty=RSS)) + labs(x='Nominal individual significance level', y='Proportion of sign. coef.') +
  ggtitle('Random events, fixed exposure') + theme_bw() + scale_colour_manual(values = colors) + geom_abline(intercept = 0, slope=1, lty='dotted') + scale_linetype_manual(values=ltys)
alphapowerProvNoOutliers
# ggsave("c:/users/tbraeye/dropbox/6.RainfallLegionella/results/2020/alphascenario01.tiff", alphapower)

alphapowerBENoOutliers<-ggplot(data=RD_alpha[RD_alpha$national==1,], aes(x, y)) + geom_line(aes(color=RSS, lty=RSS)) + labs(x='Nominal individual significance level', y='Proportion of sign. coef.') +
  ggtitle('Random events, fixed exposure') + theme_bw() + scale_colour_manual(values = colors) + geom_abline(intercept = 0, slope=1, lty='dotted') + scale_linetype_manual(values=ltys)
alphapowerBENoOutliers

#Combine the NoOutliers with the complete dataset

extractLegend <- function(gg) {
  grobs <- ggplot_gtable(ggplot_build(gg))
  foo <- which(sapply(grobs$grobs, function(x) x$name) == "guide-box")
  grobs$grobs[[foo]]
}

legend<-extractLegend(alphapowerProv + theme(legend.position='bottom') + guides(colour = guide_legend(nrow = 1)))

alphapowerNational<-plot_grid(alphapowerBE + theme(legend.position = "none") + ggtitle('National'), labels='1A')
alphapowerNationalld<-plot_grid(alphapowerBENoOutliers + theme(legend.position = "none") + ggtitle('National: Outliers removed'), labels='1B')
alphapowerProvincial<-plot_grid(alphapowerProv + theme(legend.position = "none") + ggtitle('Provincial'), labels='1C')
alphapowerProvincialld<-plot_grid(alphapowerProvNoOutliers + theme(legend.position = "none") + ggtitle('Provincial: Outliers removed'), labels='1D')


figure.10_alphaProv<-plot_grid(alphapowerNational, alphapowerNationalld, alphapowerProvincial, alphapowerProvincialld, ncol=2, rel_heights = c(1,1))
title <- ggdraw() + draw_label("Unaltered events, random exposures", fontface='bold')
figure.10_alphaProv.all<-plot_grid(title, figure.10_alphaProv, legend, ncol=1, rel_heights = c(0.1,2,0.1))

save_plot("C:/Users/ToBr708/dropbox/6.RainfallLegionella/results/2020/fig6_scenario10_alpha.tiff", figure.10_alphaProv.all, 
          ncol = 1.5, # we're saving a grid plot of 2 columns
          nrow = 2.3, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1.3)

save_plot("C:/Users/ToBr708/dropbox/6.RainfallLegionella/results/2020/fig6_scenario10_alpha.eps", figure.10_alphaProv.all, 
          ncol = 1.5, # we're saving a grid plot of 2 columns
          nrow = 2.3, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1.3)



###
# BIAS
###

## BE

#Create a plot alpha 2.1 
bias.rel.humBE<-ggplot(data=RD[RD$national==1 ,], aes(RSS, rel.hum)) + geom_boxplot(aes(color=RSS)) + labs(x='Scenario', y='Coef') +
  ggtitle('Relative humidity') + labs(y='Coefficient', x='RSS') + theme_bw() +
  geom_hline(yintercept=0, lty='dotted') + scale_x_discrete(labels= c('AD', 'AY', 'SM', 'SY', 'SM.cp', 'SY.cp')) + scale_colour_manual(values = colors) #+ coord_cartesian(ylim = c(-0, 0.05))
bias.rel.humBE
# ggsave("c:/users/tbraeye/dropbox/6.RainfallLegionella/results/2020/scenario01_relhum_beta.tiff", bias.rel.hum)

bias.tempBE<-ggplot(data=RD[RD$national==1 ,], aes(RSS, temp)) + geom_boxplot(aes(color=RSS)) + labs(x='Scenario', y='Coef') +
  ggtitle('Temperature') + labs(y='Coefficient', x='RSS') + theme_bw() +
  geom_hline(yintercept=0, lty='dotted') + scale_x_discrete(labels= c('AD', 'AY', 'SM', 'SY', 'SM.cp', 'SY.cp')) + scale_colour_manual(values = colors)  #+ coord_cartesian(ylim = c(0.2, 0.4))
bias.tempBE
# ggsave("c:/users/tbraeye/dropbox/6.RainfallLegionella/results/2020/scenario01_temp_beta.tiff", bias.temp)

bias.windBE<-ggplot(data=RD[RD$national==1 ,], aes(RSS, wind.speed)) + geom_boxplot(aes(color=RSS)) + labs(x='Scenario', y='Coef') +
  ggtitle('Wind speed')  + labs(y='Coefficient', x='RSS') + theme_bw() + #+ coord_cartesian(ylim = c(-0.1, 0.1))
  geom_hline(yintercept=0, lty='dotted') + scale_x_discrete(labels= c('AD', 'AY', 'SM', 'SY', 'SM.cp', 'SY.cp')) + scale_colour_manual(values = colors)
bias.windBE
# ggsave("c:/users/tbraeye/dropbox/6.RainfallLegionella/results/2020/scenario01_wind_beta.tiff", bias.wind)


legend<-extractLegend(bias.rel.humBE + theme(legend.position='bottom') + guides(colour = guide_legend(nrow = 1)))

figure.10.biasBE<-plot_grid(bias.rel.humBE + theme(legend.position = "none"), bias.tempBE + theme(legend.position = "none"), bias.windBE + theme(legend.position = "none"), ncol=3, labels=c('1A', '1B', '1C'))

##BE NO outliers

#Create a plot alpha 2.1 
bias.rel.humBENO<-ggplot(data=RDNoOutliers[RDNoOutliers$national==1 ,], aes(RSS, rel.hum)) + geom_boxplot(aes(color=RSS)) + labs(x='Scenario', y='Coef') +
  ggtitle('Relative humidity') + labs(y='Coefficient', x='RSS') + theme_bw() +
  geom_hline(yintercept=0, lty='dotted') + scale_x_discrete(labels= c('AD', 'AY', 'SM', 'SY', 'SM.cp', 'SY.cp')) + scale_colour_manual(values = colors) #+ coord_cartesian(ylim = c(-0, 0.05))
bias.rel.humBENO
# ggsave("c:/users/tbraeye/dropbox/6.RainfallLegionella/results/2020/scenario01_relhum_beta.tiff", bias.rel.hum)

bias.tempBENO<-ggplot(data=RDNoOutliers[RDNoOutliers$national==1 ,], aes(RSS, temp)) + geom_boxplot(aes(color=RSS)) + labs(x='Scenario', y='Coef') +
  ggtitle('Temperature') + labs(y='Coefficient', x='RSS') + theme_bw() +
  geom_hline(yintercept=0, lty='dotted') + scale_x_discrete(labels= c('AD', 'AY', 'SM', 'SY', 'SM.cp', 'SY.cp')) + scale_colour_manual(values = colors)  #+ coord_cartesian(ylim = c(0.2, 0.4))
bias.tempBENO
# ggsave("c:/users/tbraeye/dropbox/6.RainfallLegionella/results/2020/scenario01_temp_beta.tiff", bias.temp)

bias.windBENO<-ggplot(data=RDNoOutliers[RDNoOutliers$national==1 ,], aes(RSS, wind.speed)) + geom_boxplot(aes(color=RSS)) + labs(x='Scenario', y='Coef') +
  ggtitle('Wind speed')  + labs(y='Coefficient', x='RSS') + theme_bw() + #+ coord_cartesian(ylim = c(-0.1, 0.1))
  geom_hline(yintercept=0, lty='dotted') + scale_x_discrete(labels= c('AD', 'AY', 'SM', 'SY', 'SM.cp', 'SY.cp')) + scale_colour_manual(values = colors)
bias.windBENO
# ggsave("c:/users/tbraeye/dropbox/6.RainfallLegionella/results/2020/scenario01_wind_beta.tiff", bias.wind)


legend<-extractLegend(bias.rel.humBE + theme(legend.position='bottom') + guides(colour = guide_legend(nrow = 1)))

figure.10.biasBENO<-plot_grid(bias.rel.humBENO + theme(legend.position = "none"), bias.tempBENO + theme(legend.position = "none"), bias.windBENO + theme(legend.position = "none"), ncol=3, labels=c('2A', '2B', '2C'))



## Prov

#Create a plot alpha 2.1 
bias.rel.humProv<-ggplot(data=RD[RD$national==0 ,], aes(RSS, rel.hum)) + geom_boxplot(aes(color=RSS)) + labs(x='Scenario', y='Coef') +
  ggtitle('Relative humidity') + labs(y='Coefficient', x='RSS') + theme_bw() +
  geom_hline(yintercept=0, lty='dotted') + scale_x_discrete(labels= c('AD', 'AY', 'SM', 'SY', 'SM.cp', 'SY.cp')) + scale_colour_manual(values = colors) #+ coord_cartesian(ylim = c(-0, 0.05))
bias.rel.humProv
# ggsave("c:/users/tbraeye/dropbox/6.RainfallLegionella/results/2020/scenario01_relhum_Provta.tiff", bias.rel.hum)

bias.tempProv<-ggplot(data=RD[RD$national==0 ,], aes(RSS, temp)) + geom_boxplot(aes(color=RSS)) + labs(x='Scenario', y='Coef') +
  ggtitle('Temperature') + labs(y='Coefficient', x='RSS') + theme_bw() +
  geom_hline(yintercept=0, lty='dotted') + scale_x_discrete(labels= c('AD', 'AY', 'SM', 'SY', 'SM.cp', 'SY.cp')) + scale_colour_manual(values = colors)  #+ coord_cartesian(ylim = c(0.2, 0.4))
bias.tempProv
# ggsave("c:/users/tbraeye/dropbox/6.RainfallLegionella/results/2020/scenario01_temp_Provta.tiff", bias.temp)

bias.windProv<-ggplot(data=RD[RD$national==0 ,], aes(RSS, wind.speed)) + geom_boxplot(aes(color=RSS)) + labs(x='Scenario', y='Coef') +
  ggtitle('Wind speed')  + labs(y='Coefficient', x='RSS') + theme_bw() + #+ coord_cartesian(ylim = c(-0.1, 0.1))
  geom_hline(yintercept=0, lty='dotted') + scale_x_discrete(labels= c('AD', 'AY', 'SM', 'SY', 'SM.cp', 'SY.cp')) + scale_colour_manual(values = colors)
bias.windProv
# ggsave("c:/users/tbraeye/dropbox/6.RainfallLegionella/results/2020/scenario01_wind_Provta.tiff", bias.wind)


legend<-extractLegend(bias.rel.humProv + theme(legend.position='bottom') + guides(colour = guide_legend(nrow = 1)))

figure.10.biasProv<-plot_grid(bias.rel.humProv + theme(legend.position = "none"), bias.tempProv + theme(legend.position = "none"), bias.windProv + theme(legend.position = "none"), ncol=3, labels=c('3A', '3B', '3C'))

##Prov NO outliers

#Create a plot alpha 2.1 
bias.rel.humProvNO<-ggplot(data=RDNoOutliers[RDNoOutliers$national==0 ,], aes(RSS, rel.hum)) + geom_boxplot(aes(color=RSS)) + labs(x='Scenario', y='Coef') +
  ggtitle('Relative humidity') + labs(y='Coefficient', x='RSS') + theme_bw() +
  geom_hline(yintercept=0, lty='dotted') + scale_x_discrete(labels= c('AD', 'AY', 'SM', 'SY', 'SM.cp', 'SY.cp')) + scale_colour_manual(values = colors) #+ coord_cartesian(ylim = c(-0, 0.05))
bias.rel.humProvNO
# ggsave("c:/users/tbraeye/dropbox/6.RainfallLegionella/results/2020/scenario01_relhum_Provta.tiff", bias.rel.hum)

bias.tempProvNO<-ggplot(data=RDNoOutliers[RDNoOutliers$national==0 ,], aes(RSS, temp)) + geom_boxplot(aes(color=RSS)) + labs(x='Scenario', y='Coef') +
  ggtitle('Temperature') + labs(y='Coefficient', x='RSS') + theme_bw() +
  geom_hline(yintercept=0, lty='dotted') + scale_x_discrete(labels= c('AD', 'AY', 'SM', 'SY', 'SM.cp', 'SY.cp')) + scale_colour_manual(values = colors)  #+ coord_cartesian(ylim = c(0.2, 0.4))
bias.tempProvNO
# ggsave("c:/users/tbraeye/dropbox/6.RainfallLegionella/results/2020/scenario01_temp_Provta.tiff", bias.temp)

bias.windProvNO<-ggplot(data=RDNoOutliers[RDNoOutliers$national==0 ,], aes(RSS, wind.speed)) + geom_boxplot(aes(color=RSS)) + labs(x='Scenario', y='Coef') +
  ggtitle('Wind speed')  + labs(y='Coefficient', x='RSS') + theme_bw() + #+ coord_cartesian(ylim = c(-0.1, 0.1))
  geom_hline(yintercept=0, lty='dotted') + scale_x_discrete(labels= c('AD', 'AY', 'SM', 'SY', 'SM.cp', 'SY.cp')) + scale_colour_manual(values = colors)
bias.windProvNO
# ggsave("c:/users/tbraeye/dropbox/6.RainfallLegionella/results/2020/scenario01_wind_Provta.tiff", bias.wind)


legend<-extractLegend(bias.rel.humProv + theme(legend.position='bottom') + guides(colour = guide_legend(nrow = 1)))

figure.10.biasProvNO<-plot_grid(bias.rel.humProvNO + theme(legend.position = "none"), bias.tempProvNO + theme(legend.position = "none"), bias.windProvNO + theme(legend.position = "none"), ncol=3, labels=c('4A', '4B', '4C'))

title <- ggdraw() + draw_label("Unaltered events, random exposures", fontface='bold')


#Combine all the bias-plots
figure.10.bias<-plot_grid(title, figure.10.biasBE, figure.10.biasBENO, figure.10.biasProv, figure.10.biasProvNO, ncol=1, rel_heights = c(0.3,2,2,2,2))


save_plot("C:/Users/ToBr708/dropbox/6.RainfallLegionella/results/2020/fig5_scenario10_bias.tiff", figure.10.bias, 
          ncol = 2, # we're saving a grid plot of 2 columns
          nrow = 2, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1.3)

save_plot("C:/Users/ToBr708/dropbox/6.RainfallLegionella/results/2020/fig5_scenario10_bias.eps", figure.10.bias, 
          ncol = 2, # we're saving a grid plot of 2 columns
          nrow = 2, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1.3)




