Norostat<-read.csv('/Users/aliciakraay/Dropbox/NoroCovid/NOROSTAT_20200706.csv')

Norostat$OutbreakDate<-as.Date(Norostat$DATEFIRSTILL, format='%m/%d/%y')
Norostat$CovidEra<-0
Norostat$CovidEra[which(Norostat$OutbreakDate>as.Date('03/01/2020', format='%m/%d/%Y'))]<-1

Norostat$Setting<-NA
Norostat$Setting[which(Norostat$setting1 %in% c('assisted living and Alzheimer`s dementia care facility',
                                                'long term care facility',
                                                'Long-term care/nursing home/assisted living facility',
                                                'mental health facility'))]<-'Long term care'
Norostat$Setting[which(Norostat$setting1 %in% c('Hospital', 'Other healthcare facility'))]<-'Hospital/Other healthcare'
Norostat$Setting[which(Norostat$setting1 %in% c('child day care', 'Child day care'))]<-'Child daycare'
Norostat$Setting[which(Norostat$setting1 %in% c('School/college/university'))]<-'School/college/university'

NoroCovid<-subset(Norostat, Norostat$CovidEra==1 & Norostat$OutbreakDate>=as.Date('04/01/2020', format='%m/%d/%Y'))
NoroNonCovid<-subset(Norostat, Norostat$CovidEra==0 & 
                       ((Norostat$OutbreakDate>=as.Date('04/01/2013', format='%m/%d/%Y') & Norostat$OutbreakDate<=as.Date('07/30/2013', format='%m/%d/%Y'))|
                          (Norostat$OutbreakDate>=as.Date('04/01/2014', format='%m/%d/%Y') & Norostat$OutbreakDate<=as.Date('07/30/2014', format='%m/%d/%Y'))|
                          (Norostat$OutbreakDate>=as.Date('04/01/2015', format='%m/%d/%Y') & Norostat$OutbreakDate<=as.Date('07/30/2015', format='%m/%d/%Y'))|
                          (Norostat$OutbreakDate>=as.Date('04/01/2016', format='%m/%d/%Y') & Norostat$OutbreakDate<=as.Date('07/30/2016', format='%m/%d/%Y'))|
                          (Norostat$OutbreakDate>=as.Date('04/01/2017', format='%m/%d/%Y') & Norostat$OutbreakDate<=as.Date('07/30/2017', format='%m/%d/%Y'))|
                          (Norostat$OutbreakDate>=as.Date('04/01/2018', format='%m/%d/%Y') & Norostat$OutbreakDate<=as.Date('07/30/2018', format='%m/%d/%Y'))|
                          (Norostat$OutbreakDate>=as.Date('04/01/2019', format='%m/%d/%Y') & Norostat$OutbreakDate<=as.Date('07/30/2019', format='%m/%d/%Y'))))

NoroCovCompare<-rbind(NoroCovid, NoroNonCovid)
NoroCovCompare<-NoroCovCompare[,c(1:18, 35, 38, 40, 43, 48, 50, 53, 55, 58, 61:64)]

NoroCovCompare2<-subset(NoroCovCompare, !(NoroCovCompare$REPORTINGSITE %in% c('New Mexico', 'Nebraska', 'Wyoming')))

StartDates<-data.frame(REPORTINGSITE=c('Massachusetts', 'Michigan', 'Minnesota', 'Ohio', 'Oregon', 'South Carolina', 
                                       'Tennessee', 'Virginia', 'Wisconsin'), 
                       DateEntry=c(as.Date(c('08/01/2016', '08/01/2015', '08/01/2012', '08/01/2012', '08/01/2012',
                                          '08/01/2015', '08/01/2012', '08/01/2016', '08/01/2012'), format='%m/%d/%Y')))
NoroCovCompare3<-merge(NoroCovCompare2, StartDates, by='REPORTINGSITE', all.x=TRUE)
NoroCovCompare4<-subset(NoroCovCompare3, NoroCovCompare3$OutbreakDate>=NoroCovCompare3$DateEntry)

NoroCovCompare4$Month<-month(NoroCovCompare4$OutbreakDate)

#Adjusted
LTC.p<-glm(ESTIMATEDPRIMARY~CovidEra+REPORTINGSITE, 
            data=subset(NoroCovCompare4, NoroCovCompare4$Setting=='Long term care'), family=poisson)
dispersiontest(LTC.p,trafo=1)
LTC.qp<-glm(ESTIMATEDPRIMARY~CovidEra+REPORTINGSITE, 
            data=subset(NoroCovCompare4, NoroCovCompare4$Setting=='Long term care'), 
            family=quasipoisson)
summary(LTC.qp)
LTC.nb<-glm(ESTIMATEDPRIMARY~CovidEra+REPORTINGSITE, 
            data=subset(NoroCovCompare4, NoroCovCompare4$Setting=='Long term care'), 
            family=quasipoisson)
summary(LTC.nb)

Hosp.p<-glm(ESTIMATEDPRIMARY~CovidEra+REPORTINGSITE, 
            data=subset(NoroCovCompare4, NoroCovCompare4$Setting=='Hospital/Other healthcare'), family=poisson)
dispersiontest(Hosp.p,trafo=1)

summary(glm(ESTIMATEDPRIMARY~CovidEra+REPORTINGSITE, 
            data=subset(NoroCovCompare4, NoroCovCompare4$Setting=='Child daycare'), family=quasipoisson))
summary(glm(ESTIMATEDPRIMARY~CovidEra+REPORTINGSITE, 
            data=subset(NoroCovCompare4, NoroCovCompare4$Setting=='School/college/university'), family=quasipoisson))
summary(glm(ESTIMATEDPRIMARY~CovidEra+REPORTINGSITE, 
            data=subset(NoroCovCompare4, is.na(NoroCovCompare4$Setting)), family=poisson))
summary(glm(ESTIMATEDPRIMARY~CovidEra+REPORTINGSITE, 
            data=NoroCovCompare4, family=quasipoisson))
exp(-0.94776)

#Unadjusted
summary(glm(ESTIMATEDPRIMARY~CovidEra, 
            data=subset(NoroCovCompare4, NoroCovCompare4$Setting=='Long term care'), family=quasipoisson))
summary(glm(ESTIMATEDPRIMARY~CovidEra, 
            data=subset(NoroCovCompare, NoroCovCompare$Setting=='Hospital/Other healthcare'), family=quasipoisson))
summary(glm(ESTIMATEDPRIMARY~CovidEra, 
            data=subset(NoroCovCompare, NoroCovCompare$Setting=='Child daycare'), family=quasipoisson))
summary(glm(ESTIMATEDPRIMARY~CovidEra, 
            data=subset(NoroCovCompare, NoroCovCompare$Setting=='School/college/university'), family=quasipoisson))
summary(glm(ESTIMATEDPRIMARY~CovidEra, 
            data=subset(NoroCovCompare2, is.na(NoroCovCompare2$Setting)), family=quasipoisson))
summary(glm(ESTIMATEDPRIMARY~CovidEra, 
            data=NoroCovCompare2, family=quasipoisson))


NoroCovCompare4$Setting2<-NoroCovCompare4$Setting
NoroCovCompare4$Setting2[which(is.na(NoroCovCompare4$Setting2))]<-'Other'

summary(glm(ESTIMATEDPRIMARY~CovidEra+REPORTINGSITE+Setting2+Setting2*CovidEra, 
            data=NoroCovCompare4, family=poisson))


require(AER)
rd<-glm(ESTIMATEDPRIMARY~CovidEra+REPORTINGSITE+Setting2+Setting2*CovidEra, 
        data=NoroCovCompare4, family=poisson)
dispersiontest(rd,trafo=1)

Outbreak.Sizes1<-ddply(NoroCovCompare4, .(CovidEra, Setting2), summarize, size=mean(ESTIMATEDPRIMARY),
                     standard.dev=sd(ESTIMATEDPRIMARY))
Outbreak.Sizes2<-ddply(NoroCovCompare4, .(CovidEra), summarize, size=mean(ESTIMATEDPRIMARY),
                      standard.dev=sd(ESTIMATEDPRIMARY))

Outbreak.Sizes2$Setting2<-'All'
Outbreak.Sizes<-rbind(Outbreak.Sizes1, Outbreak.Sizes2)

Outbreak.Sizes$Era<-NA
Outbreak.Sizes$Era[which(Outbreak.Sizes$CovidEra==0)]<-'April-July 2013-2019'
Outbreak.Sizes$Era[which(Outbreak.Sizes$CovidEra==1)]<-'April-July 2020'
Outbreak.Sizes$Setting3<-Outbreak.Sizes$Setting2
Outbreak.Sizes$Setting3[which(Outbreak.Sizes$Setting3=='Hospital/Other healthcare')]<-'Healthcare facilities'
#install.packages('EnvStats')
label.df<-data.frame(Setting3=c('All', 'Long term care'),
                     size=c(26.24, 25.78), Era=c(rep('April-July 2020', 2)))
label.df2<-data.frame(Setting3=rep(c('All', 'Child daycare', 'Healthcare facilities', 'Long term care', 'Other', 'School/college/university'), each=2),
                     size=c(24.24, 9.29, 17.7, 3.65, 21.44, 8.65, 
                            23.78, 9.47, 20.01, 14.65, 44, 7.65), 
                     Era=c(rep('April-July 2013-2019', 6), rep('April-July 2020', 6)))

barplot<-ggplot(data=Outbreak.Sizes, aes(fill=Era, y=size, x=Setting3))+
  geom_bar(position='dodge', stat='identity')+
  scale_fill_grey()+
  #geom_errorbar(aes(ymin=size-standard.dev, ymax=size+standard.dev), 
  #width=0.2, position_dodge(0.9))+
  theme_classic()+theme(axis.text.x = element_text(angle = 90, size=12), 
                        legend.title =element_blank())+labs(x=' ', y='Average outbreak size')+
  #stat_n_text()+
  geom_text(data=label.df, label='*', size=6)+
  geom_text(data=label.df2, label=c('      n=1241              ', '           n=32', 
                                    'n=62         ', '        n=2', 
                                    'n=38         ', '        n=3', 
                                    'n=651         ', '           n=25',
                                   'n=370         ', '         n=1',
                                   'n=120         ', '        n=1'), size=4)
ggsave(filename='/Users/aliciakraay/Dropbox/NoroCovid/OutbreakSize_QuasiPoissonSig_grayscale_NoMarch.pdf', 
       plot=barplot, device='pdf',
       w=8, h=7)
ggplot(data=Outbreak.Sizes)


