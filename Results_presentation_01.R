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

#scenario 01: bias and type I error


RD.CL<-list()
RD.CL2<-list()

# 
# for (iter in c(1:48)){
#   scenario.01.outputCL <- readRDS(paste("c:/users/ToBr708/dropbox/6.RainfallLegionella/results/2020/simresults2/scenario01CL", iter, ".Rdata", sep=""))
#   RD.CL <- c(RD.CL, scenario.01.outputCL)}
# RD.CL<-rbindlist(RD.CL)

for (iter in c(1:50)){
  scenario.01.outputCL <- readRDS(paste("c:/users/ToBr708/dropbox/6.RainfallLegionella/results/2020/simresults_01_2/scenario01CL", iter, ".Rdata", sep=""))
  RD.CL <- c(RD.CL, scenario.01.outputCL)}
RD.CL<-rbindlist(RD.CL)

for (iter in c(1:50)){
  scenario.01.outputCL <- readRDS(paste("c:/users/ToBr708/dropbox/6.RainfallLegionella/results/2020/simresults_01/scenario01CL", iter, ".Rdata", sep=""))
  RD.CL2 <- c(RD.CL2, scenario.01.outputCL)}
RD.CL2<-rbindlist(RD.CL2)

RD.CL<-rbind(RD.CL, RD.CL2)

RD <- subset(RD.CL, select=-c(exp.temp, exp.rel.hum, exp.wind.speed))


RD<-RD[RD$RSS!='S3W',]

p005 <- RD[, list(temp005=sum(p.temp<=0.05)/length(p.temp), rel.hum005=sum(p.rel.hum<0.05)/length(p.rel.hum), wind.speed005=sum(p.wind.speed<0.05)/length(p.wind.speed)), by=list(RSS, national)]
p004 <- RD[, list(temp004=sum(p.temp<=0.04)/length(p.temp), rel.hum004=sum(p.rel.hum<0.04)/length(p.rel.hum), wind.speed004=sum(p.wind.speed<0.04)/length(p.wind.speed)), by=list(RSS, national)]
p003 <- RD[, list(temp003=sum(p.temp<=0.03)/length(p.temp), rel.hum003=sum(p.rel.hum<0.03)/length(p.rel.hum), wind.speed003=sum(p.wind.speed<0.03)/length(p.wind.speed)), by=list(RSS, national)]
p002 <- RD[, list(temp002=sum(p.temp<=0.02)/length(p.temp), rel.hum002=sum(p.rel.hum<0.02)/length(p.rel.hum), wind.speed002=sum(p.wind.speed<0.02)/length(p.wind.speed)), by=list(RSS, national)]

p005.all <- p005[, list(y=((temp005 + rel.hum005 + wind.speed005)/3), x=0.05), by=list(RSS, national)]
p004.all <- p004[, list(y=((temp004 + rel.hum004 + wind.speed004)/3), x=0.04), by=list(RSS, national)]
p003.all <- p003[, list(y=((temp003 + rel.hum003 + wind.speed003)/3), x=0.03), by=list(RSS, national)]
p002.all <- p002[, list(y=((temp002 + rel.hum002 + wind.speed002)/3), x=0.02), by=list(RSS, national)]


RD_alpha<-rbind(p005.all, p004.all, p003.all, p002.all)

###
# Plotting alpha
###

#Create a plot alpha 2.1 
alphapowerProv<-ggplot(data=RD_alpha[RD_alpha$national==0,], aes(x, y)) + geom_line(aes(color=RSS, lty=RSS)) + labs(x='Nominal individual significance level', y='Proportion of sign. coef.') +
  ggtitle('Random events, unaltered exposure') + theme_bw() + scale_colour_manual(values = colors) + geom_abline(intercept = 0, slope=1, lty='dotted') + scale_linetype_manual(values=ltys)
alphapowerProv
# ggsave("c:/users/tbraeye/dropbox/6.RainfallLegionella/results/2020/alphascenario01.tiff", alphapower)

alphapowerBE<-ggplot(data=RD_alpha[RD_alpha$national==1,], aes(x, y)) + geom_line(aes(color=RSS, lty=RSS)) + labs(x='Nominal individual significance level', y='Proportion of sign. coef.') +
  ggtitle('Random events, unaltered exposure') + theme_bw() + scale_colour_manual(values = colors) + geom_abline(intercept = 0, slope=1, lty='dotted') + scale_linetype_manual(values=ltys)
alphapowerBE

##Combining National and Provincial

extractLegend <- function(gg) {
  grobs <- ggplot_gtable(ggplot_build(gg))
  foo <- which(sapply(grobs$grobs, function(x) x$name) == "guide-box")
  grobs$grobs[[foo]]
}

legend<-extractLegend(alphapowerProv + theme(legend.position='bottom') + guides(colour = guide_legend(nrow = 1)))
title.01 <- ggdraw() + draw_label("Random events, unaltered exposures", fontface='bold')

alphapowerNational<-plot_grid(alphapowerBE + theme(legend.position = "none") + ggtitle('National'), labels='1')
alphapowerProvincial<-plot_grid(alphapowerProv + theme(legend.position = "none") + ggtitle('Provincial'), labels='2')

figure.01_alphaProv<-plot_grid(title.01, alphapowerNational, alphapowerProvincial, legend, ncol=1, rel_heights = c(0.1,1,1,0.1))


save_plot("C:/Users/ToBr708/dropbox/6.RainfallLegionella/results/2020/fig4_scenario01_alpha_2.tiff", figure.01_alphaProv, 
          ncol = 2, # we're saving a grid plot of 2 columns
          nrow = 2, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1.3)
save_plot("C:/Users/ToBr708/dropbox/6.RainfallLegionella/results/2020/fig4_scenario01_alpha_2.eps", figure.01_alphaProv, 
          ncol = 2, # we're saving a grid plot of 2 columns
          nrow = 2, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1.3)





###
# BIAS
###

#Create a plot alpha 2.1 
bias.rel.humBE<-ggplot(data=RD[RD$national==1 ,], aes(RSS, rel.hum)) + geom_boxplot(aes(color=RSS)) + labs(x='Scenario', y='Coef') +
  ggtitle('Relative humidity') + labs(y='Coefficient', x='RSS') + theme_bw() +
  geom_hline(yintercept=0, lty='dotted') + scale_x_discrete(labels= c('AD', 'AY', 'SM', 'SY', 'SY.cp')) + scale_colour_manual(values = colors) #+ coord_cartesian(ylim = c(-0, 0.05))
bias.rel.humBE
# ggsave("c:/users/tbraeye/dropbox/6.RainfallLegionella/results/2020/scenario01_relhum_beta.tiff", bias.rel.hum)

bias.tempBE<-ggplot(data=RD[RD$national==1 ,], aes(RSS, temp)) + geom_boxplot(aes(color=RSS)) + labs(x='Scenario', y='Coef') +
  ggtitle('Temperature') + labs(y='Coefficient', x='RSS') + theme_bw() +
  geom_hline(yintercept=0, lty='dotted') + scale_x_discrete(labels= c('AD', 'AY', 'SM', 'SY', 'SY.cp')) + scale_colour_manual(values = colors)  #+ coord_cartesian(ylim = c(0.2, 0.4))
bias.tempBE
# ggsave("c:/users/tbraeye/dropbox/6.RainfallLegionella/results/2020/scenario01_temp_beta.tiff", bias.temp)

bias.windBE<-ggplot(data=RD[RD$national==1 ,], aes(RSS, wind.speed)) + geom_boxplot(aes(color=RSS)) + labs(x='Scenario', y='Coef') +
  ggtitle('Wind speed')  + labs(y='Coefficient', x='RSS') + theme_bw() + #+ coord_cartesian(ylim = c(-0.1, 0.1))
  geom_hline(yintercept=0, lty='dotted') + scale_x_discrete(labels= c('AD', 'AY', 'SM', 'SY', 'SY.cp')) + scale_colour_manual(values = colors)
bias.windBE
# ggsave("c:/users/tbraeye/dropbox/6.RainfallLegionella/results/2020/scenario01_wind_beta.tiff", bias.wind)


legend<-extractLegend(bias.rel.humBE + theme(legend.position='bottom') + guides(colour = guide_legend(nrow = 1)))

alphapowerf<-plot_grid(alphapowerBE, labels='1A')
figure.01.biasBE<-plot_grid(bias.rel.humBE + theme(legend.position = "none"), bias.tempBE + theme(legend.position = "none"), bias.windBE + theme(legend.position = "none"), ncol=3, labels=c('2A', '2B', '2C'))
figure.01.biasBE1<-plot_grid(bias.rel.humBE + theme(legend.position = "none"), bias.tempBE + theme(legend.position = "none"), bias.windBE + theme(legend.position = "none"), ncol=3, labels=c('1A', '1B', '1C'))


figure.01f<-plot_grid(alphapowerf, figure.01.biasBE, ncol=1, rel_heights = c(1,1))


# save_plot("C:/Users/ToBr708/dropbox/6.RainfallLegionella/results/2020/scenario01_nat.tiff", figure.01f, 
#           ncol = 2, # we're saving a grid plot of 2 columns
#           nrow = 2, # and 2 rows
#           # each individual subplot should have an aspect ratio of 1.3
#           base_aspect_ratio = 1.3)
#           

###
# Plotting Estimate
###

#Create a plot alpha 2.1 
bias.rel.humProv<-ggplot(data=RD[RD$national==0 ,], aes(RSS, rel.hum)) + geom_boxplot(aes(color=RSS)) + labs(x='Scenario', y='Coef') +
  ggtitle('Relative humidity') + labs(y='Coefficient', x='RSS') + theme_bw() +
  geom_hline(yintercept=0, lty='dotted') + scale_x_discrete(labels= c('AD', 'AY', 'SM', 'SY', 'SY.cp')) + scale_colour_manual(values = colors) #+ coord_cartesian(ylim = c(-0, 0.05))
bias.rel.humProv
# ggsave("c:/users/tbraeye/dropbox/6.RainfallLegionella/results/2020/scenario01_relhum_beta.tiff", bias.rel.hum)

bias.tempProv<-ggplot(data=RD[RD$national==0 ,], aes(RSS, temp)) + geom_boxplot(aes(color=RSS)) + labs(x='Scenario', y='Coef') +
  ggtitle('Temperature') + labs(y='Coefficient', x='RSS') + theme_bw() +
  geom_hline(yintercept=0, lty='dotted') + scale_x_discrete(labels= c('AD', 'AY', 'SM', 'SY', 'SY.cp')) + scale_colour_manual(values = colors)  #+ coord_cartesian(ylim = c(0.2, 0.4))
bias.tempProv
# ggsave("c:/users/tbraeye/dropbox/6.RainfallLegionella/results/2020/scenario01_temp_beta.tiff", bias.temp)

bias.windProv<-ggplot(data=RD[RD$national==0 ,], aes(RSS, wind.speed)) + geom_boxplot(aes(color=RSS)) + labs(x='Scenario', y='Coef') +
  ggtitle('Wind speed')  + labs(y='Coefficient', x='RSS') + theme_bw() + #+ coord_cartesian(ylim = c(-0.1, 0.1))
  geom_hline(yintercept=0, lty='dotted') + scale_x_discrete(labels= c('AD', 'AY', 'SM', 'SY', 'SY.cp')) + scale_colour_manual(values = colors)
bias.windProv
# ggsave("c:/users/tbraeye/dropbox/6.RainfallLegionella/results/2020/scenario01_wind_beta.tiff", bias.wind)


legend<-extractLegend(bias.rel.humProv + theme(legend.position='bottom') + guides(colour = guide_legend(nrow = 1)))

alphapowerf<-plot_grid(alphapowerProv, labels='1A')
figure.01.biasProv<-plot_grid(bias.rel.humProv + theme(legend.position = "none"), bias.tempProv + theme(legend.position = "none"), bias.windProv + theme(legend.position = "none"), ncol=3, labels=c('2A', '2B', '2C'))

figure.01f<-plot_grid(alphapowerf, figure.01.biasProv, ncol=1, rel_heights = c(1,1))


# save_plot("C:/Users/tbraeye/dropbox/6.RainfallLegionella/results/2020/scenario01_prov.png", figure.01f, 
#           ncol = 2, # we're saving a grid plot of 2 columns
#           nrow = 2, # and 2 rows
#           # each individual subplot should have an aspect ratio of 1.3
#           base_aspect_ratio = 1.3)


####
# Combine provincial and national
####

# bias


biasProv<-plot_grid(figure.01.biasProv)
biasBE<-plot_grid(figure.01.biasBE1)

figure.01_biasProv<-plot_grid(title.01, biasBE, biasProv, ncol=1, rel_heights = c(0.1,1,1))


save_plot("C:/Users/ToBr708/dropbox/6.RainfallLegionella/results/2020/fig3_scenario01_bias_2.tiff", figure.01_biasProv, 
          ncol = 2, # we're saving a grid plot of 2 columns
          nrow = 2, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1.3)

save_plot("C:/Users/ToBr708/dropbox/6.RainfallLegionella/results/2020/fig3_scenario01_bias_2.eps", figure.01_biasProv, 
          ncol = 2, # we're saving a grid plot of 2 columns
          nrow = 2, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1.3)
