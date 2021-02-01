PointEst<-read.csv('/Users/aliciakraay/Dropbox/NoroCovid/GAMEsts_big_quasipoisson.csv')
names(PointEst)[2]<-'Group'
PointEst$Group[4]<-'Tennessee'
require(ggplot2)
require(metafor)
require(ggpubr)

States<-subset(PointEst, PointEst$ModelType=='State')
States2<-subset(States, !(States$Group=='Tennessee'))
Pooled.states<-rma.mv(yi=log(IRR), V=(SE)^2, data=States, 
                             random=~1|Group); Pooled.states
Pooled.states2<-rma.mv(yi=log(IRR), V=(SE)^2, data=States2,
                      random=~1|Group); Pooled.states2

StatesAll<-rbind(States[,c(2, 10:12)], 
      data.frame(Group='All States', IRR=0.136, IRR_LCL=0.075, IRR_UCL=0.247))

States_noTN<-subset(StatesAll, !(StatesAll$Group=='Tennessee'))

StatesAll$IRR_LCL[which(StatesAll$Group=='Tennessee')]<-NA
StatesAll$IRR_UCL[which(StatesAll$Group=='Tennessee')]<-NA

StatePlot<-ggplot(data=StatesAll, aes(x=Group, y=IRR))+geom_point()+
  geom_errorbar(ymin=StatesAll$IRR_LCL, ymax=StatesAll$IRR_UCL)+geom_hline(yintercept=1, linetype=2)+
  theme_classic()+coord_flip()+lims(y=c(0, 1.3))+
  labs(x='State', y='IRR (95% CI) comparing \n Apr-July 2020 with prior years (ref)')+ggtitle('A')

Setting<-subset(PointEst, PointEst$ModelType=='Setting')

SettingPlot<-ggplot(data=Setting, aes(x=Group, y=IRR))+geom_point()+
  geom_errorbar(ymin=Setting$IRR_LCL, ymax=Setting$IRR_UCL)+geom_hline(yintercept=1, linetype=2)+
  theme_classic()+coord_flip()+lims(y=c(0, 2))+
  labs(x='Setting', y='IRR (95% CI) comparing \n Apr-July 2020 with prior years (ref)')+ggtitle('B')

ggsave(file='/Users/aliciakraay/Dropbox/NoroCovid/ResponseComments/Fig1_StateSetting.pdf',
       plot=ggarrange(StatePlot, SettingPlot, nrow=1, ncol=2), device='pdf',
       w=9, h=5)
