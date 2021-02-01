require(ggplot2)
#MobilePooled<-read.csv('/Users/jo/Dropbox/NoroCovid/GoogleMobility_Pooled.csv')
MobilePooled<-read.csv('/Users/aliciakraay/Dropbox/NoroCovid/GoogleMobility_Pooled.csv')

PooledUS<-subset(MobilePooled, MobilePooled$Entity=='United States')

ggplot(PooledUS, aes(x='Date', y='Workplaces....'))+geom_line()

#MobileCounty<-read.csv('/Users/jo/Dropbox/NoroCovid/mobility_report_US_counties.csv')
MobileCounty<-read.csv('/Users/aliciakraay/Dropbox/NoroCovid/mobility_report_US_counties.csv')

MobileNORS<-subset(MobileCounty, MobileCounty$state %in% c('Michigan', 'Massachusetts', 'Minnesota', 'Nebraska', 'New Mexico',
                                               'Ohio', 'Oregon', 'South Carolina', 'Tennessee', 'Virginia', 
                                               'Wisconsin', 'Wyoming'))

MobileNORS<-subset(MobileNORS, !(MobileNORS$state %in% c('New Mexico', 'Nebraska', 'Wyoming')))

MobileNORSState<-subset(MobileNORS, MobileNORS$county=='Total')

MobileNORSState$Date<-as.Date(MobileNORSState$date, format='%Y-%m-%d')

ggplot(MobileNORSState, aes(x=Date, y=workplaces))+geom_line()+facet_wrap(~state)

#This assumes 25 complete weeks of data (current number).  Reps need to be altered if more data is added. 
MobileWeekly<-data.frame(State=rep(unique(MobileNORSState$state), each=25),
                         Date=rep(seq(from=min(MobileNORSState$Date), to=max(MobileNORSState$Date), by=7), 9),
                         Work=colMeans(matrix(MobileNORSState$workplaces, nrow=7)),
                         Retail.Recreation=colMeans(matrix(MobileNORSState$retail.and.recreation, nrow=7)),
                         Grocery.Pharmacy=colMeans(matrix(MobileNORSState$grocery.and.pharmacy, nrow=7)),
                         Transit=colMeans(matrix(MobileNORSState$transit.stations, nrow=7)),
                         Residential=colMeans(matrix(MobileNORSState$residential, nrow=7)))

MobileWeekly1<-subset(MobileWeekly, Date<=as.Date("2020-07-31"))
ggplot(MobileWeekly1, aes(x=Date, y=Work))+geom_line()+facet_wrap(~State)+labs(y='Workplace Mobility (7 day average)')+
  scale_x_date(limits = as.Date(c(NA, "2020-06-30")))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                            panel.background = element_blank(), axis.line = element_line(colour = "black"))

p1<-ggplot(MobileWeekly1, aes(x=Date, y=Work, group=State))+geom_line(aes(color=State))+
  labs(y='Workplace Mobility (7 day average)')+theme(legend.position='none')+
  scale_x_date(limits = as.Date(c(NA, "2020-06-30")))+
  theme_bw()+theme(legend.position='none')

p2<-ggplot(MobileWeekly1, aes(x=Date, y=Retail.Recreation, group=State))+geom_line(aes(color=State))+
  labs(y='Retail/Recreation Mobility (7 day average)')+theme(legend.position='none')+
  scale_x_date(limits = as.Date(c(NA, "2020-06-30")))+
  theme_bw()
  #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #      panel.background = element_blank(), axis.line = element_line(colour = "black"))

legend<-get_legend(p2)
p2_nolegend<-p2+theme(legend.position='none')

p3<-ggplot(MobileWeekly, aes(x=Date, y=Retail.Recreation, group=State))+geom_line(aes(color=State))+
  labs(y='Retail/Recreation Mobility (7 day average)')+scale_x_date(limits = as.Date(c(NA, "2020-06-30")))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

require(ggpubr)
ggsave(filename='/Users/aliciakraay/Dropbox/NoroCovid/ResponseComments/FigS1_Mobility.pdf', 
       plot=ggarrange(p1, p2_nolegend, legend, ncol=3, nrow=1, labels=c("A", "B", " ")),
       device='pdf', w=7, h=5)

ggsave(filename='/Users/jo/Desktop/NoroCovid/FigA_Mobility.pdf', plot=ggarrange(p1, p2, ncol=2, nrow=1), 
       device='pdf', w=7, h=5)

ggsave(filename='/Users/jo/Desktop/NoroCovid/FigMobile_Legend.pdf', plot=p3, 
       device='pdf')
MobileNORSState$CovidEra<-0
MobileNORSState$CovidEra[which(MobileNORSState$Date>as.Date('03/01/2020', format='%m/%d/%Y'))]<-1
MobileNORSState$month<-month(MobileNORSState$Date)
April<-subset(MobileNORSState, MobileNORSState$month==4)
require(plyr)
ddply(April, .(state), summarize, AvgRed.wk=mean(workplaces),
      AvgRed.rec=mean(retail.and.recreation), AvgRed.transit=mean(transit.stations))

Michigan<-subset(MobileWeekly, MobileWeekly$State=='Michigan')

load("~/Dropbox/NoroCovid/DistancingPolicies/DistancingPolicies.rdata")
## Policies of 12 states -------------------------------------------------------
Policies_12 <- Policies_all[Policies_all$StateName 
                            %in% c("Massachusetts", "Michigan", "Minnesota", "Nebraska", 
                                   "New Mexico", "Ohio", "Oregon", "South Carolina", 
                                   "Tennessee", "Virginia", "Wisconsin", "Wyoming"), ]

EmergencyDeclared<-as.Date(subset(Policies_12, Policies_12$StateName=='Michigan' & Policies_12$StatePolicy=='EmergDec')$DateEnacted, format='%Y-%m-%d')
SAH<-as.Date(subset(Policies_12, Policies_12$StateName=='Michigan' & Policies_12$StatePolicy=='StayAtHome')$DateEnacted, format='%Y-%m-%d')
Schools<-as.Date(subset(Policies_12, Policies_12$StateName=='Michigan' & Policies_12$StatePolicy=='SchoolClose')$DateEnacted, format='%Y-%m-%d')
SAH.end<-as.Date(subset(Policies_12, Policies_12$StateName=='Michigan' & Policies_12$StatePolicy=='StayAtHome')$DateEnded, format='%Y-%m-%d')
#NE.eased<-as.Date(subset(Policies_12, Policies_12$StateName=='Michigan' & Policies_12$StatePolicy=='NEBusinessClose')$DateEased, format='%Y-%m-%d')
MichiganExample<-ggplot(Michigan)+geom_line(aes(x=Date, y=Work))+
  #geom_line(aes(x=Date, y=Retail.Recreation), color='blue')+
  geom_vline(xintercept=EmergencyDeclared, linetype=2)+
  geom_vline(xintercept=SAH, linetype=2)+
  geom_vline(xintercept=Schools, linetype=2)+
  #geom_vline(xintercept=NE.eased, linetype=3)+
  geom_vline(xintercept=SAH.end, linetype=3)+
  geom_text(aes(x =EmergencyDeclared-5, label="\nPublic Health Emergency", y=-30), 
  color="grey60", angle=90, size = 4)+ 
  geom_text(aes(x = SAH-5, label="\nSAH Order", y=-30), 
            color="grey60", angle=90, size = 4)+ 
  geom_text(aes(x = Schools-5, label="\nSchools Close", y=-30), 
            color="grey60", angle=90, size = 4)+
  geom_text(aes(x =SAH.end-5, label="\nSAH Order Lifted", y=-30), 
            color="grey60", angle=90, size = 4)+theme_classic()+
  labs(y='Mobility (7 day average)')+ggtitle('Michigan')+lims(y=c(-65, 12))

ggsave(plot=MichiganExample, filename='/Users/jo/Desktop/NoroCovid/MobilityOrders_Michigan_Work.pdf', w=7, h=4)

MA<-subset(MobileWeekly, MobileWeekly$State=='Massachusetts')

EmergencyDeclared<-as.Date(subset(Policies_12, Policies_12$StateName=='Massachusetts' & Policies_12$StatePolicy=='EmergDec')$DateEnacted, format='%Y-%m-%d')
SAH<-as.Date(subset(Policies_12, Policies_12$StateName=='Massachusetts' & Policies_12$StatePolicy=='StayAtHome')$DateEnacted, format='%Y-%m-%d')
Schools<-as.Date(subset(Policies_12, Policies_12$StateName=='Massachusetts' & Policies_12$StatePolicy=='SchoolClose')$DateEnacted, format='%Y-%m-%d')
SAH.end<-as.Date(subset(Policies_12, Policies_12$StateName=='Massachusetts' & Policies_12$StatePolicy=='StayAtHome')$DateEased, format='%Y-%m-%d')
#NE.eased<-as.Date(subset(Policies_12, Policies_12$StateName=='Michigan' & Policies_12$StatePolicy=='NEBusinessClose')$DateEased, format='%Y-%m-%d')
MAExample<-ggplot(MA)+geom_line(aes(x=Date, y=Work))+
  #geom_line(aes(x=Date, y=Retail.Recreation), color='blue')+
  geom_vline(xintercept=EmergencyDeclared, linetype=2)+
  geom_vline(xintercept=SAH, linetype=2)+
  geom_vline(xintercept=Schools, linetype=2)+
  #geom_vline(xintercept=NE.eased, linetype=3)+
  geom_vline(xintercept=SAH.end, linetype=3)+
  geom_text(aes(x =EmergencyDeclared-5, label="\nPublic Health Emergency", y=-30), 
            color="grey60", angle=90, size = 4)+ 
  geom_text(aes(x = SAH-5, label="\nSAH Order", y=-30), 
            color="grey60", angle=90, size = 4)+ 
  geom_text(aes(x = Schools-5, label="\nSchools Close", y=-30), 
            color="grey60", angle=90, size = 4)+
  geom_text(aes(x =SAH.end-5, label="\nSAH Order Lifted", y=-30), 
            color="grey60", angle=90, size = 4)+theme_classic()+
  labs(y='Mobility (7 day average)')+ggtitle('Massachusetts')+lims(y=c(-65, 12))

ggsave(plot=MAExample, filename='/Users/jo/Desktop/NoroCovid/MobilityOrders_Massachusetts_Work.pdf', w=7, h=4)


MN<-subset(MobileWeekly, MobileWeekly$State=='Minnesota')

EmergencyDeclared<-as.Date(subset(Policies_12, Policies_12$StateName=='Minnesota' & Policies_12$StatePolicy=='EmergDec')$DateEnacted, format='%Y-%m-%d')
SAH<-as.Date(subset(Policies_12, Policies_12$StateName=='Minnesota' & Policies_12$StatePolicy=='StayAtHome')$DateEnacted, format='%Y-%m-%d')
Schools<-as.Date(subset(Policies_12, Policies_12$StateName=='Minnesota' & Policies_12$StatePolicy=='SchoolClose')$DateEnacted, format='%Y-%m-%d')
SAH.end<-as.Date(subset(Policies_12, Policies_12$StateName=='Minnesota' & Policies_12$StatePolicy=='StayAtHome')$DateEased, format='%Y-%m-%d')
#NE.eased<-as.Date(subset(Policies_12, Policies_12$StateName=='Michigan' & Policies_12$StatePolicy=='NEBusinessClose')$DateEased, format='%Y-%m-%d')
MNExample<-ggplot(MN)+geom_line(aes(x=Date, y=Work))+
  #geom_line(aes(x=Date, y=Retail.Recreation), color='blue')+
  geom_vline(xintercept=EmergencyDeclared, linetype=2)+
  geom_vline(xintercept=SAH[1], linetype=2)+
  geom_vline(xintercept=Schools, linetype=2)+
  #geom_vline(xintercept=NE.eased, linetype=3)+
  geom_vline(xintercept=SAH[2], linetype=3)+
  geom_text(aes(x =EmergencyDeclared-5, label="\nPublic Health Emergency", y=-30), 
            color="grey60", angle=90, size = 4)+ 
  geom_text(aes(x = SAH[1]-5, label="\nSAH Order", y=-30), 
            color="grey60", angle=90, size = 4)+ 
  geom_text(aes(x = Schools-5, label="\nSchools Close", y=-30), 
            color="grey60", angle=90, size = 4)+
  geom_text(aes(x =SAH[2]-5, label="\nSAH Order Relaxed", y=-30), 
            color="grey60", angle=90, size = 4)+theme_classic()+
  labs(y='Mobility (7 day average)')+ggtitle('Minnesota')+lims(y=c(-65, 12))

ggsave(plot=MNExample, filename='/Users/jo/Desktop/NoroCovid/MobilityOrders_MN_Work.pdf', w=7, h=4)

NE<-subset(MobileWeekly, MobileWeekly$State=='Nebraska')

EmergencyDeclared<-as.Date(subset(Policies_12, Policies_12$StateName=='Nebraska' & Policies_12$StatePolicy=='EmergDec')$DateEnacted, format='%Y-%m-%d')
#SAH<-as.Date(subset(Policies_12, Policies_12$StateName=='Nebraska' & Policies_12$StatePolicy=='StayAtHome')$DateEnacted, format='%Y-%m-%d')
Schools<-as.Date(subset(Policies_12, Policies_12$StateName=='Nebraska' & Policies_12$StatePolicy=='SchoolClose')$DateEnacted, format='%Y-%m-%d')
#SAH.end<-as.Date(subset(Policies_12, Policies_12$StateName=='Nebraska' & Policies_12$StatePolicy=='StayAtHome')$DateEased, format='%Y-%m-%d')
Eased<-as.Date(subset(Policies_12, Policies_12$StateName=='Nebraska' & Policies_12$StatePolicy=='BarRestrict')$DateEased, format='%Y-%m-%d')
NEExample<-ggplot(NE)+geom_line(aes(x=Date, y=Work))+
  #geom_line(aes(x=Date, y=Retail.Recreation), color='blue')+
  geom_vline(xintercept=EmergencyDeclared, linetype=2)+
  #geom_vline(xintercept=SAH[1], linetype=2)+
  geom_vline(xintercept=Schools, linetype=2)+
  geom_vline(xintercept=Eased[2], linetype=3)+
  #geom_vline(xintercept=SAH[2], linetype=3)+
  geom_text(aes(x =EmergencyDeclared-5, label="\nPublic Health Emergency", y=-30), 
            color="grey60", angle=90, size = 4)+ 
  #geom_text(aes(x = SAH[1]-5, label="\nSAH Order", y=-30), 
  #          color="grey60", angle=90, size = 4)+ 
  geom_text(aes(x = Schools-5, label="\nSchools Close", y=-30), 
            color="grey60", angle=90, size = 4)+
  geom_text(aes(x=Eased[2]-5, label="\nBusiness Restrictions Eased", y=-30), 
            color="grey60", angle=90, size = 4)+theme_classic()+
  labs(y='Mobility (7 day average)')+ggtitle('Nebraska')+lims(y=c(-65, 12))

ggsave(plot=NEExample, filename='/Users/jo/Desktop/NoroCovid/MobilityOrders_NE_Work.pdf', w=7, h=4)

#New Mexico
NM<-subset(MobileWeekly, MobileWeekly$State=='New Mexico')

EmergencyDeclared<-as.Date(subset(Policies_12, Policies_12$StateName=='New Mexico' & Policies_12$StatePolicy=='EmergDec')$DateEnacted, format='%Y-%m-%d')
SAH<-as.Date(subset(Policies_12, Policies_12$StateName=='New Mexico' & Policies_12$StatePolicy=='StayAtHome')$DateEnacted, format='%Y-%m-%d')
Schools<-as.Date(subset(Policies_12, Policies_12$StateName=='New Mexico' & Policies_12$StatePolicy=='SchoolClose')$DateEnacted, format='%Y-%m-%d')
#SAH.end<-as.Date(subset(Policies_12, Policies_12$StateName=='Nebraska' & Policies_12$StatePolicy=='StayAtHome')$DateEased, format='%Y-%m-%d')
Eased<-as.Date(subset(Policies_12, Policies_12$StateName=='New Mexico' & Policies_12$StatePolicy=='NEBusinessClose')$DateEased, format='%Y-%m-%d')
NMExample<-ggplot(NM)+geom_line(aes(x=Date, y=Work))+
  #geom_line(aes(x=Date, y=Retail.Recreation), color='blue')+
  geom_vline(xintercept=EmergencyDeclared, linetype=2)+
  geom_vline(xintercept=SAH, linetype=2)+
  geom_vline(xintercept=Schools, linetype=2)+
  geom_vline(xintercept=Eased, linetype=3)+
  #geom_vline(xintercept=SAH[2], linetype=3)+
  geom_text(aes(x =EmergencyDeclared-5, label="\nPublic Health Emergency", y=-30), 
            color="grey60", angle=90, size = 4)+ 
  geom_text(aes(x = SAH-5, label="\nSAH Order", y=-30), 
            color="grey60", angle=90, size = 4)+ 
  geom_text(aes(x = Schools-5, label="\nSchools Close", y=-30), 
            color="grey60", angle=90, size = 4)+
  geom_text(aes(x=Eased-5, label="\nBusiness Restrictions Eased", y=-30), 
            color="grey60", angle=90, size = 4)+theme_classic()+
  labs(y='Mobility (7 day average)')+ggtitle('New Mexico')+lims(y=c(-65, 12))

ggsave(plot=NMExample, filename='/Users/jo/Desktop/NoroCovid/MobilityOrders_NM_Work.pdf', w=7, h=4)

#Ohio
OH<-subset(MobileWeekly, MobileWeekly$State=='Ohio')

EmergencyDeclared<-as.Date(subset(Policies_12, Policies_12$StateName=='Ohio' & Policies_12$StatePolicy=='EmergDec')$DateEnacted, format='%Y-%m-%d')
SAH<-as.Date(subset(Policies_12, Policies_12$StateName=='Ohio' & Policies_12$StatePolicy=='StayAtHome')$DateEnacted, format='%Y-%m-%d')
Schools<-as.Date(subset(Policies_12, Policies_12$StateName=='Ohio' & Policies_12$StatePolicy=='SchoolClose')$DateEnacted, format='%Y-%m-%d')
SAH.end<-as.Date(subset(Policies_12, Policies_12$StateName=='Ohio' & Policies_12$StatePolicy=='StayAtHome')$DateExpiry, format='%Y-%m-%d')
Eased<-as.Date(subset(Policies_12, Policies_12$StateName=='Ohio' & Policies_12$StatePolicy=='NEBusinessClose')$DateEased, format='%Y-%m-%d')
OHExample<-ggplot(OH)+geom_line(aes(x=Date, y=Work))+
  #geom_line(aes(x=Date, y=Retail.Recreation), color='blue')+
  geom_vline(xintercept=EmergencyDeclared, linetype=2)+
  geom_vline(xintercept=SAH[1], linetype=2)+
  geom_vline(xintercept=Schools, linetype=2)+
  geom_vline(xintercept=Eased, linetype=3)+
  #geom_vline(xintercept=SAH[2], linetype=3)+
  geom_text(aes(x =EmergencyDeclared-5, label="\nPublic Health Emergency", y=-30), 
            color="grey60", angle=90, size = 4)+ 
  geom_text(aes(x = SAH[1]-5, label="\nSAH Order", y=-30), 
            color="grey60", angle=90, size = 4)+ 
  geom_text(aes(x = Schools-5, label="\nSchools Close", y=-30), 
            color="grey60", angle=90, size = 4)+
  geom_text(aes(x=Eased-5, label="\nBusiness Restrictions Eased", y=-30), 
            color="grey60", angle=90, size = 4)+theme_classic()+
  labs(y='Mobility (7 day average)')+ggtitle('Ohio')+lims(y=c(-65, 12))

ggsave(plot=OHExample, filename='/Users/jo/Desktop/NoroCovid/MobilityOrders_OH_Work.pdf', w=7, h=4)

#Oregon
OR<-subset(MobileWeekly, MobileWeekly$State=='Oregon')

EmergencyDeclared<-as.Date(subset(Policies_12, Policies_12$StateName=='Oregon' & Policies_12$StatePolicy=='EmergDec')$DateEnacted, format='%Y-%m-%d')
SAH<-as.Date(subset(Policies_12, Policies_12$StateName=='Oregon' & Policies_12$StatePolicy=='StayAtHome')$DateEnacted, format='%Y-%m-%d')[1]
Schools<-as.Date(subset(Policies_12, Policies_12$StateName=='Oregon' & Policies_12$StatePolicy=='SchoolClose')$DateEnacted, format='%Y-%m-%d')
SAH.end<-as.Date(subset(Policies_12, Policies_12$StateName=='Oregon' & Policies_12$StatePolicy=='StayAtHome')$DateEased, format='%Y-%m-%d')[1]
#Eased<-as.Date(subset(Policies_12, Policies_12$StateName=='Oregon' & Policies_12$StatePolicy=='NEBusinessClose')$DateEased, format='%Y-%m-%d')
ORExample<-ggplot(OR)+geom_line(aes(x=Date, y=Work))+
  #geom_line(aes(x=Date, y=Retail.Recreation), color='blue')+
  geom_vline(xintercept=EmergencyDeclared, linetype=2)+
  geom_vline(xintercept=SAH, linetype=2)+
  geom_vline(xintercept=Schools, linetype=2)+
  geom_vline(xintercept=SAH.end, linetype=3)+
  #geom_vline(xintercept=SAH[2], linetype=3)+
  geom_text(aes(x =EmergencyDeclared-5, label="\nPublic Health Emergency", y=-30), 
            color="grey60", angle=90, size = 4)+ 
  geom_text(aes(x = SAH-5, label="\nSAH Order", y=-30), 
            color="grey60", angle=90, size = 4)+ 
  geom_text(aes(x = Schools[1]-5, label="\nSchools Close", y=-30), 
            color="grey60", angle=90, size = 4)+
  geom_text(aes(x=SAH.end-5, label="\nSAH order lifted", y=-30), 
            color="grey60", angle=90, size = 4)+theme_classic()+
  labs(y='Mobility (7 day average)')+ggtitle('Oregon')+lims(y=c(-65, 12))

ggsave(plot=ORExample, filename='/Users/jo/Desktop/NoroCovid/MobilityOrders_OR_Work.pdf', w=7, h=4)

#South Carolina
SC<-subset(MobileWeekly, MobileWeekly$State=='South Carolina')

EmergencyDeclared<-as.Date(subset(Policies_12, Policies_12$StateName=='South Carolina' & Policies_12$StatePolicy=='EmergDec')$DateEnacted, format='%Y-%m-%d')
SAH<-as.Date(subset(Policies_12, Policies_12$StateName=='South Carolina' & Policies_12$StatePolicy=='StayAtHome')$DateEnacted, format='%Y-%m-%d')[1]
Schools<-as.Date(subset(Policies_12, Policies_12$StateName=='South Carolina' & Policies_12$StatePolicy=='SchoolClose')$DateEnacted, format='%Y-%m-%d')
SAH.end<-as.Date(subset(Policies_12, Policies_12$StateName=='South Carolina' & Policies_12$StatePolicy=='StayAtHome')$DateExpiry, format='%Y-%m-%d')[2]
#Eased<-as.Date(subset(Policies_12, Policies_12$StateName=='South Carolina' & Policies_12$StatePolicy=='NEBusinessClose')$DateEased, format='%Y-%m-%d')
SCExample<-ggplot(SC)+geom_line(aes(x=Date, y=Work))+
  #geom_line(aes(x=Date, y=Retail.Recreation), color='blue')+
  geom_vline(xintercept=EmergencyDeclared, linetype=2)+
  geom_vline(xintercept=SAH, linetype=2)+
  geom_vline(xintercept=Schools, linetype=2)+
  geom_vline(xintercept=SAH.end, linetype=3)+
  #geom_vline(xintercept=SAH[2], linetype=3)+
  geom_text(aes(x =EmergencyDeclared-5, label="\nPublic Health Emergency", y=-30), 
            color="grey60", angle=90, size = 4)+ 
  geom_text(aes(x = SAH-5, label="\nSAH Order", y=-30), 
            color="grey60", angle=90, size = 4)+ 
  geom_text(aes(x = Schools[1]-5, label="\nSchools Close", y=-30), 
            color="grey60", angle=90, size = 4)+
  geom_text(aes(x=SAH.end-5, label="\nSAH order lifted", y=-30), 
            color="grey60", angle=90, size = 4)+theme_classic()+
  labs(y='Mobility (7 day average)')+ggtitle('South Carolina')+lims(y=c(-65, 12))

ggsave(plot=SCExample, filename='/Users/jo/desktop/NoroCovid/MobilityOrders_SC_Work.pdf', w=7, h=4)

#Tennessee
TN<-subset(MobileWeekly, MobileWeekly$State=='Tennessee')

EmergencyDeclared<-as.Date(subset(Policies_12, Policies_12$StateName=='Tennessee' & Policies_12$StatePolicy=='EmergDec')$DateEnacted, format='%Y-%m-%d')
SAH<-as.Date(subset(Policies_12, Policies_12$StateName=='Tennessee' & Policies_12$StatePolicy=='StayAtHome')$DateEnacted, format='%Y-%m-%d')[2]
Schools<-as.Date(subset(Policies_12, Policies_12$StateName=='Tennessee' & Policies_12$StatePolicy=='SchoolClose')$DateEnacted, format='%Y-%m-%d')
SAH.end<-as.Date(subset(Policies_12, Policies_12$StateName=='Tennessee' & Policies_12$StatePolicy=='StayAtHome')$DateEased, format='%Y-%m-%d')[2]
#Eased<-as.Date(subset(Policies_12, Policies_12$StateName=='South Carolina' & Policies_12$StatePolicy=='NEBusinessClose')$DateEased, format='%Y-%m-%d')
TNExample<-ggplot(TN)+geom_line(aes(x=Date, y=Work))+
  #geom_line(aes(x=Date, y=Retail.Recreation), color='blue')+
  geom_vline(xintercept=EmergencyDeclared, linetype=2)+
  geom_vline(xintercept=SAH, linetype=2)+
  geom_vline(xintercept=Schools, linetype=2)+
  geom_vline(xintercept=SAH.end, linetype=3)+
  #geom_vline(xintercept=SAH[2], linetype=3)+
  geom_text(aes(x =EmergencyDeclared-5, label="\nPublic Health Emergency", y=-30), 
            color="grey60", angle=90, size = 4)+ 
  geom_text(aes(x = SAH-5, label="\nSAH Order", y=-30), 
            color="grey60", angle=90, size = 4)+ 
  geom_text(aes(x = Schools[1]-5, label="\nSchools Close", y=-30), 
            color="grey60", angle=90, size = 4)+
  geom_text(aes(x=SAH.end-5, label="\nSAH order lifted", y=-30), 
            color="grey60", angle=90, size = 4)+theme_classic()+
  labs(y='Mobility (7 day average)')+ggtitle('Tennessee')+lims(y=c(-65, 12))

ggsave(plot=TNExample, filename='/Users/jo/desktop/NoroCovid/MobilityOrders_TN_Work.pdf', w=7, h=4)

#Virginia
VA<-subset(MobileWeekly, MobileWeekly$State=='Virginia')

EmergencyDeclared<-as.Date(subset(Policies_12, Policies_12$StateName=='Virginia' & Policies_12$StatePolicy=='EmergDec')$DateEnacted, format='%Y-%m-%d')
SAH<-as.Date(subset(Policies_12, Policies_12$StateName=='Virginia' & Policies_12$StatePolicy=='StayAtHome')$DateEnacted, format='%Y-%m-%d')
Schools<-as.Date(subset(Policies_12, Policies_12$StateName=='Virginia' & Policies_12$StatePolicy=='SchoolClose')$DateEnacted, format='%Y-%m-%d')
SAH.end<-as.Date(subset(Policies_12, Policies_12$StateName=='Virginia' & Policies_12$StatePolicy=='StayAtHome')$DateEnded, format='%Y-%m-%d')
#Eased<-as.Date(subset(Policies_12, Policies_12$StateName=='South Carolina' & Policies_12$StatePolicy=='NEBusinessClose')$DateEased, format='%Y-%m-%d')
VAExample<-ggplot(VA)+geom_line(aes(x=Date, y=Work))+
  #geom_line(aes(x=Date, y=Retail.Recreation), color='blue')+
  geom_vline(xintercept=EmergencyDeclared, linetype=2)+
  geom_vline(xintercept=SAH, linetype=2)+
  geom_vline(xintercept=Schools, linetype=2)+
  geom_vline(xintercept=SAH.end, linetype=3)+
  #geom_vline(xintercept=SAH[2], linetype=3)+
  geom_text(aes(x =EmergencyDeclared-5, label="\nPublic Health Emergency", y=-30), 
            color="grey60", angle=90, size = 4)+ 
  geom_text(aes(x = SAH-5, label="\nSAH Order", y=-30), 
            color="grey60", angle=90, size = 4)+ 
  geom_text(aes(x = Schools[1]-5, label="\nSchools Close", y=-30), 
            color="grey60", angle=90, size = 4)+
  geom_text(aes(x=SAH.end-5, label="\nSAH order lifted", y=-30), 
            color="grey60", angle=90, size = 4)+theme_classic()+
  labs(y='Mobility (7 day average)')+ggtitle('Virginia')+lims(y=c(-65, 12))

ggsave(plot=VAExample, filename='/Users/jo/Desktop/NoroCovid/MobilityOrders_VA_Work.pdf', w=7, h=4)

#Wisconsin
WI<-subset(MobileWeekly, MobileWeekly$State=='Wisconsin')

EmergencyDeclared<-as.Date(subset(Policies_12, Policies_12$StateName=='Wisconsin' & Policies_12$StatePolicy=='EmergDec')$DateEnacted, format='%Y-%m-%d')
SAH<-as.Date(subset(Policies_12, Policies_12$StateName=='Wisconsin' & Policies_12$StatePolicy=='StayAtHome')$DateEnacted, format='%Y-%m-%d')
Schools<-as.Date(subset(Policies_12, Policies_12$StateName=='Wisconsin' & Policies_12$StatePolicy=='SchoolClose')$DateEnacted, format='%Y-%m-%d')
SAH.end<-as.Date(subset(Policies_12, Policies_12$StateName=='Wisconsin' & Policies_12$StatePolicy=='StayAtHome')$DateEnded, format='%Y-%m-%d')
#Eased<-as.Date(subset(Policies_12, Policies_12$StateName=='South Carolina' & Policies_12$StatePolicy=='NEBusinessClose')$DateEased, format='%Y-%m-%d')
WIExample<-ggplot(WI)+geom_line(aes(x=Date, y=Work))+
  #geom_line(aes(x=Date, y=Retail.Recreation), color='blue')+
  geom_vline(xintercept=EmergencyDeclared[1], linetype=2)+
  geom_vline(xintercept=SAH, linetype=2)+
  geom_vline(xintercept=Schools, linetype=2)+
  geom_vline(xintercept=SAH.end, linetype=3)+
  #geom_vline(xintercept=SAH[2], linetype=3)+
  geom_text(aes(x =EmergencyDeclared[1]-5, label="\nPublic Health Emergency", y=-30), 
            color="grey60", angle=90, size = 4)+ 
  geom_text(aes(x = SAH-5, label="\nSAH Order", y=-30), 
            color="grey60", angle=90, size = 4)+ 
  geom_text(aes(x = Schools[1]-5, label="\nSchools Close", y=-30), 
            color="grey60", angle=90, size = 4)+
  geom_text(aes(x=SAH.end-5, label="\nSAH order lifted", y=-30), 
            color="grey60", angle=90, size = 4)+theme_classic()+
  labs(y='Mobility (7 day average)')+ggtitle('Wisconsin')+lims(y=c(-65, 12))

ggsave(plot=WIExample, filename='/Users/jo/desktop/NoroCovid/MobilityOrders_WI_Work.pdf', w=7, h=4)

ggarrange(MichiganExample, OHExample, NEExample, NMExample, nrow=2, ncol=3)

WorkMobile.12state<-ggplot(data=MobileWeekly, aes(x=Date, y=Work, group=State, color=State))+geom_line()+labs(y='Work Mobility (7 day average)')+
  theme_classic()
ggsave(plot=WorkMobile.12state, filename='/Users/jo/Desktop/NoroCovid/WorkMobility_12states.pdf', w=7, h=4)

RecMobile.12state<-ggplot(data=MobileWeekly, aes(x=Date, y=Retail.Recreation, group=State, color=State))+geom_line()+
  labs(y='Retail/Recreation Mobility (7 day average)')+
  theme_classic()
ggsave(plot=RecMobile.12state, filename='/Users/jo/Desktop/NoroCovid/RecMobility_12states.pdf', w=7, h=4)

summary(subset(MobileWeekly, MobileWeekly$State=='Wisconsin'))

summary(subset(MobileWeekly, MobileWeekly$State=='Wyoming'))
WY<-subset(MobileWeekly, MobileWeekly$State=='Wyoming')

EmergencyDeclared<-as.Date(subset(Policies_12, Policies_12$StateName=='Wyoming' & Policies_12$StatePolicy=='EmergDec')$DateEnacted, format='%Y-%m-%d')
SAH<-as.Date(subset(Policies_12, Policies_12$StateName=='Wyoming' & Policies_12$StatePolicy=='StayAtHome')$DateEnacted, format='%Y-%m-%d')
Schools<-as.Date(subset(Policies_12, Policies_12$StateName=='Wyoming' & Policies_12$StatePolicy=='SchoolClose')$DateEnacted, format='%Y-%m-%d')
SAH.end<-as.Date(subset(Policies_12, Policies_12$StateName=='Wyoming' & Policies_12$StatePolicy=='StayAtHome')$DateEnded, format='%Y-%m-%d')
Eased<-as.Date(subset(Policies_12, Policies_12$StateName=='Wyoming' & Policies_12$StatePolicy=='OtherBusinessClose')$DateEased, format='%Y-%m-%d')[2]
WYExample<-ggplot(WY)+geom_line(aes(x=Date, y=Work))+
  #geom_line(aes(x=Date, y=Retail.Recreation), color='blue')+
  geom_vline(xintercept=EmergencyDeclared, linetype=2)+
  #geom_vline(xintercept=SAH, linetype=2)+
  geom_vline(xintercept=Schools, linetype=2)+
  geom_vline(xintercept=Eased, linetype=3)+
  #geom_vline(xintercept=SAH.end, linetype=3)+
  geom_text(aes(x =EmergencyDeclared-5, label="\nPublic Health Emergency", y=-30), 
            color="grey60", angle=90, size = 4)+ 
  geom_text(aes(x = SAH[1]-5, label="\nSAH Order", y=-30), 
            color="grey60", angle=90, size = 4)+ 
  geom_text(aes(x = Schools-5, label="\nSchools Close", y=-30), 
            color="grey60", angle=90, size = 4)+
  geom_text(aes(x=Eased-5, label="\nBusiness Restrictions Eased", y=-30), 
            color="grey60", angle=90, size = 4)+theme_classic()+
  labs(y='Mobility (7 day average)')+ggtitle('Wyoming')+lims(y=c(-65, 12))

ggsave(plot=WYExample, filename='/Users/jo/Desktop/NoroCovid/MobilityOrders_WY_Work.pdf', w=7, h=4)

WYExample2<-ggplot(WY)+geom_line(aes(x=Date, y=Work))+
  geom_line(aes(x=Date, y=Retail.Recreation), color='blue')+
  geom_vline(xintercept=EmergencyDeclared, linetype=2)+
  #geom_vline(xintercept=SAH, linetype=2)+
  geom_vline(xintercept=Schools, linetype=2)+
  geom_vline(xintercept=Eased, linetype=3)+
  #geom_vline(xintercept=SAH.end, linetype=3)+
  geom_text(aes(x =EmergencyDeclared-5, label="\nPublic Health Emergency", y=-30), 
            color="grey60", angle=90, size = 4)+ 
  geom_text(aes(x = SAH[1]-5, label="\nSAH Order", y=-30), 
            color="grey60", angle=90, size = 4)+ 
  geom_text(aes(x = Schools-5, label="\nSchools Close", y=-30), 
            color="grey60", angle=90, size = 4)+
  geom_text(aes(x=Eased-5, label="\nBusiness Restrictions Eased", y=-30), 
            color="grey60", angle=90, size = 4)+theme_classic()+
  labs(y='Mobility (7 day average)')+ggtitle('Wyoming')+lims(y=c(-65, 12))
ggsave(plot=WYExample2, filename='/Users/jo/Desktop/NoroCovid/MobilityOrders_WY_WorkRec.pdf', w=7, h=4)

MI<-subset(MobileWeekly, MobileWeekly$State=='Michigan')

EmergencyDeclared<-as.Date(subset(Policies_12, Policies_12$StateName=='Michigan' & Policies_12$StatePolicy=='EmergDec')$DateEnacted, format='%Y-%m-%d')
SAH<-as.Date(subset(Policies_12, Policies_12$StateName=='Michigan' & Policies_12$StatePolicy=='StayAtHome')$DateEnacted, format='%Y-%m-%d')
Schools<-as.Date(subset(Policies_12, Policies_12$StateName=='Michigan' & Policies_12$StatePolicy=='SchoolClose')$DateEnacted, format='%Y-%m-%d')
SAH.end<-as.Date(subset(Policies_12, Policies_12$StateName=='Michigan' & Policies_12$StatePolicy=='StayAtHome')$DateEnded, format='%Y-%m-%d')
#NE.eased<-as.Date(subset(Policies_12, Policies_12$StateName=='Michigan' & Policies_12$StatePolicy=='NEBusinessClose')$DateEased, format='%Y-%m-%d')
MIExample2<-ggplot(MI)+geom_line(aes(x=Date, y=Work))+
  geom_line(aes(x=Date, y=Retail.Recreation), color='blue')+
  geom_vline(xintercept=EmergencyDeclared, linetype=2)+
  geom_vline(xintercept=SAH, linetype=2)+
  geom_vline(xintercept=Schools, linetype=2)+
  #geom_vline(xintercept=NE.eased, linetype=3)+
  geom_vline(xintercept=SAH.end, linetype=3)+
  geom_text(aes(x =EmergencyDeclared-5, label="\nPublic Health Emergency", y=-30), 
            color="grey60", angle=90, size = 4)+ 
  geom_text(aes(x = SAH-5, label="\nSAH Order", y=-30), 
            color="grey60", angle=90, size = 4)+ 
  geom_text(aes(x = Schools-5, label="\nSchools Close", y=-30), 
            color="grey60", angle=90, size = 4)+
  geom_text(aes(x =SAH.end-5, label="\nSAH Order Lifted", y=-30), 
            color="grey60", angle=90, size = 4)+theme_classic()+
  labs(y='Mobility (7 day average)')+ggtitle('Michigan')+lims(y=c(-65, 12))

ggsave(plot=MIExample2, filename='/Users/jo/Desktop/NoroCovid/MobilityOrders_Michigan_WorkRec.pdf', w=7, h=4)
