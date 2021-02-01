require(mgcv)
require(plyr)
require(AER)

#Import NoroSTAT data
#setwd("~/Desktop/NoroCOVID")
#Norostat <- read.csv(file = "DATA/NOROSTAT_20200706.csv")
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

NoroCovid<-subset(Norostat, Norostat$CovidEra==1)
NoroNonCovid<-subset(Norostat, Norostat$CovidEra==0 & 
                         ((Norostat$OutbreakDate>as.Date('03/01/2013', format='%m/%d/%Y') & Norostat$OutbreakDate<=as.Date('07/30/2013', format='%m/%d/%Y'))|
                              (Norostat$OutbreakDate>as.Date('03/01/2014', format='%m/%d/%Y') & Norostat$OutbreakDate<=as.Date('07/30/2014', format='%m/%d/%Y'))|
                              (Norostat$OutbreakDate>as.Date('03/01/2015', format='%m/%d/%Y') & Norostat$OutbreakDate<=as.Date('07/30/2015', format='%m/%d/%Y'))|
                              (Norostat$OutbreakDate>as.Date('03/01/2016', format='%m/%d/%Y') & Norostat$OutbreakDate<=as.Date('07/30/2016', format='%m/%d/%Y'))|
                              (Norostat$OutbreakDate>as.Date('03/01/2017', format='%m/%d/%Y') & Norostat$OutbreakDate<=as.Date('07/30/2017', format='%m/%d/%Y'))|
                              (Norostat$OutbreakDate>as.Date('03/01/2018', format='%m/%d/%Y') & Norostat$OutbreakDate<=as.Date('07/30/2018', format='%m/%d/%Y'))|
                              (Norostat$OutbreakDate>as.Date('03/01/2019', format='%m/%d/%Y') & Norostat$OutbreakDate<=as.Date('07/30/2019', format='%m/%d/%Y'))))

NoroCovCompare<-rbind(NoroCovid, NoroNonCovid)
NoroCovCompare<-NoroCovCompare[,c(1:18, 35, 38, 40, 43, 48, 50, 53, 55, 58, 61:64)]

Norostat$MonthYear<-format(as.Date(Norostat$OutbreakDate), '%Y-%m')
Norostat$Outbreak<-1
MonthState.allsettings<-ddply(Norostat, .(MonthYear, REPORTINGSITE), summarize, NumOutbreaks=sum(Outbreak))
MonthState.allsettings$DateGroup<-as.Date(MonthState.allsettings$MonthYear, format='%Y-%m')

# gam(NumOutbreaks ~ s(Month, bs='cc', k=12)+CovidEra), by different states
## Minnesota
MN<-subset(MonthState.allsettings, MonthState.allsettings$REPORTINGSITE=='Minnesota')
MN.zeroes<-data.frame(MonthYear=c('2013-06', '2014-07', '2020-05', '2020-07'), 
                      REPORTINGSITE=c('Minnesota', 'Minnesota', 'Minnesota', 'Minnesota'), 
                      NumOutbreaks=c(0, 0, 0, 0), DateGroup=c(NA, NA, NA, NA))
MN2<-rbind(MN, MN.zeroes)
MN2$DateGroup<-paste(MN2$MonthYear, '-01', sep='')
MN2$Date<-as.Date(MN2$DateGroup)
MN3<-MN2[order(MN2$Date),]
require(lubridate)
MN3$Month<-month(MN3$Date)
MN3$CovidEra<-0
MN3$CovidEra[which(MN3$Date>=as.Date('03/01/2020', format='%m/%d/%Y'))]<-1
#MN3<-subset(MN3, MN3$Date>as.Date('08/31/2012', format='%m/%d/%Y')) #time lag between outbreak and report
MN4<-subset(MN3, !(MN3$MonthYear %in% '2020-03'))
gam_p <- gam(NumOutbreaks ~ s(Month, bs='cc', k=12)+CovidEra, data=MN4, family=poisson)

dispersiontest(gam_p,trafo=1)

gam_nb <- gam(NumOutbreaks ~ s(Month, bs='cc', k=12)+CovidEra, data=MN4, family=nb) #Can just update to nb
plot.gam(gam_y)
gam_qp <- gam(NumOutbreaks ~ s(Month, bs='cc', k=12)+CovidEra, data=MN4, family=quasipoisson) #Can just update to nb
plot.gam(gam_qp)
title(main = "Minnesota")
summary(gam_p); summary(gam_nb); summary(gam_qp)
exp(-2.07043); exp(-2.06894); exp(-2.0681)

## install.packages("devtools")
## devtools::install_github("gavinsimpson/schoenberg")
library(schoenberg)
library(mgcv)
exp(gam_p[["coefficients"]]) # Rate ratio
confint.default(gam_p)
exp(confint.default(gam_p)) # CI for the rate ratio
exp(gam_nb[["coefficients"]]) # Rate ratio
confint.default(gam_nb)
exp(confint.default(gam_nb)) # CI for the rate ratio
exp(gam_qp[["coefficients"]]) # Rate ratio
confint.default(gam_qp)
exp(confint.default(gam_qp)) # CI for the rate ratio
mean(MN4$NumOutbreaks); var(MN4$NumOutbreaks)
SeasonMN<-subset(MN4, MN4$Month %in% c(4, 5, 6, 7))
ddply(SeasonMN, .(CovidEra), summarize, AvgOutbreaks=mean(NumOutbreaks), 
      MinOutbreaks=min(NumOutbreaks), MaxOutbreaks=max(NumOutbreaks))

## Ohio
OH<-subset(MonthState.allsettings, MonthState.allsettings$REPORTINGSITE=='Ohio')
OH.zeroes<-data.frame(MonthYear=c('2020-05', '2020-06', '2020-07'), REPORTINGSITE=c('Ohio', 'Ohio', 'Ohio'), 
                      NumOutbreaks=c(0, 0, 0), DateGroup=c(NA, NA, NA))
OH2<-rbind(OH, OH.zeroes)
OH2$DateGroup<-paste(OH2$MonthYear, '-01', sep='')
OH2$Date<-as.Date(OH2$DateGroup)
OH3<-OH2[order(OH2$Date),]
require(lubridate)
OH3$Month<-month(OH3$Date)
OH3$CovidEra<-0
OH3$CovidEra[which(OH3$Date>=as.Date('03/01/2020', format='%m/%d/%Y'))]<-1
#OH3<-subset(OH3, OH3$Date>as.Date('08/31/2012', format='%m/%d/%Y'))
OH4<-subset(OH3, !(OH3$MonthYear %in% '2020-03'))
gam_p <- gam(NumOutbreaks ~ s(Month, bs='cc', k=12)+CovidEra, data=OH4, family=poisson)
dispersiontest(gam_p,trafo=1)

gam_nb <- gam(NumOutbreaks ~ s(Month, bs='cc', k=12)+CovidEra, data=OH4, family=nb) #Can just update to nb
plot.gam(gam_y)
gam_qp <- gam(NumOutbreaks ~ s(Month, bs='cc', k=12)+CovidEra, data=OH4, family=quasipoisson) #Can just update to nb
plot.gam(gam_qp)
title(main = "Ohio")
summary(gam_p); summary(gam_nb); summary(gam_qp)
#gam_y <- gam(NumOutbreaks ~ s(Month, bs='cc', k=12)+CovidEra, data=OH4, family=poisson)
#plot.gam(gam_y)
#title(main = "Ohio")
#summary(gam_y)
exp(gam_p[["coefficients"]]) # Rate ratio
confint.default(gam_p)
exp(confint.default(gam_p)) # CI for the rate ratio
exp(gam_nb[["coefficients"]]) # Rate ratio
confint.default(gam_nb)
exp(confint.default(gam_nb)) # CI for the rate ratio
exp(gam_qp[["coefficients"]]) # Rate ratio
confint.default(gam_qp)
exp(confint.default(gam_qp)) # CI for the rate ratio
exp(gam_y[["coefficients"]]) # Rate ratio
confint.default(gam_y)
exp(confint.default(gam_y)) # CI for the rate ratio
mean(OH4$NumOutbreaks)
var(OH4$NumOutbreaks)
SeasonOH<-subset(OH4, OH4$Month %in% c(4, 5, 6, 7))
ddply(SeasonOH, .(CovidEra), summarize, AvgOutbreaks=mean(NumOutbreaks), 
      MinOutbreaks=min(NumOutbreaks), MaxOutbreaks=max(NumOutbreaks))

## Oregon
OR<-subset(MonthState.allsettings, MonthState.allsettings$REPORTINGSITE=='Oregon')
OR.zeroes<-data.frame(MonthYear=c('2020-05'), REPORTINGSITE=c('Oregon'), 
                      NumOutbreaks=c(0), DateGroup=c(NA))
OR2<-rbind(OR, OR.zeroes)
OR2$DateGroup<-paste(OR2$MonthYear, '-01', sep='')
OR2$Date<-as.Date(OR2$DateGroup)
OR3<-OR2[order(OR2$Date),]
OR3$Month<-month(OR3$Date)
OR3$CovidEra<-0
OR3$CovidEra[which(OR3$Date>=as.Date('03/01/2020', format='%m/%d/%Y'))]<-1
#OR3<-subset(OR3, OR3$Date>as.Date('08/31/2012', format='%m/%d/%Y'))
OR4<-subset(OR3, !(OR3$MonthYear %in% '2020-03'))
gam_p <- gam(NumOutbreaks ~ s(Month, bs='cc', k=12)+CovidEra, data=OR4, family=poisson)

dispersiontest(gam_p,trafo=1)

gam_nb <- gam(NumOutbreaks ~ s(Month, bs='cc', k=12)+CovidEra, data=OR4, family=nb) #Can just update to nb
gam_qp <- gam(NumOutbreaks ~ s(Month, bs='cc', k=12)+CovidEra, data=OR4, family=quasipoisson) #Can just update to nb
plot.gam(gam_qp)
title(main = "Oregon")
summary(gam_p); summary(gam_nb); summary(gam_qp)
logLik(gam_p); logLik(gam_nb); logLik(gam_qp)
exp(gam_p[['coefficients']])
exp(confint.default(gam_p))
exp(gam_nb[['coefficients']])
exp(confint.default(gam_nb))
exp(gam_qp[['coefficients']])
exp(confint.default(gam_qp))
mean(OR4$NumOutbreaks)
var(OR4$NumOutbreaks)
SeasonOR<-subset(OR4, OR4$Month %in% c(4, 5, 6, 7))
ddply(SeasonOR, .(CovidEra), summarize, AvgOutbreaks=mean(NumOutbreaks), 
      MinOutbreaks=min(NumOutbreaks), MaxOutbreaks=max(NumOutbreaks))

## Tennessee
TN<-subset(MonthState.allsettings, MonthState.allsettings$REPORTINGSITE=='Tennessee')
TN.zeroes<-data.frame(MonthYear=c('2012-08', '2013-06', '2014-06', '2014-10', '2015-10', 
                                  '2017-05', '2019-07', '2019-08', '2020-04', '2020-05', 
                                  '2020-06', '2020-07'), 
                      REPORTINGSITE=c('Tennessee', 'Tennessee', 'Tennessee', 'Tennessee', 'Tennessee',
                                      'Tennessee', 'Tennessee', 'Tennessee', 'Tennessee', 'Tennessee',
                                      'Tennessee', 'Tennessee'), 
                      NumOutbreaks=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
                      DateGroup=c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))
TN2<-rbind(TN, TN.zeroes)
TN2$DateGroup<-paste(TN2$MonthYear, '-01', sep='')
TN2$Date<-as.Date(TN2$DateGroup)
TN3<-TN2[order(TN2$Date),]
TN3$Month<-month(TN3$Date)
TN3$CovidEra<-0
TN3$CovidEra[which(TN3$Date>=as.Date('03/01/2020', format='%m/%d/%Y'))]<-1
#TN3<-subset(TN3, TN3$Date>as.Date('08/31/2012', format='%m/%d/%Y'))
TN4<-subset(TN3, !(TN3$MonthYear %in% '2020-03'))
gam_p <- gam(NumOutbreaks ~ s(Month, bs='cc', k=12)+CovidEra, data=TN4, family=poisson)
dispersiontest(gam_p,trafo=1)
gam_nb <- gam(NumOutbreaks ~ s(Month, bs='cc', k=12)+CovidEra, data=TN4, family=nb) #Can just update to nb
gam_qp <- gam(NumOutbreaks ~ s(Month, bs='cc', k=12)+CovidEra, data=TN4, family=quasipoisson) #Can just update to nb
plot.gam(gam_qp)
summary(gam_p); summary(gam_nb); summary(gam_qp)
logLik(gam_p); logLik(gam_nb); logLik(gam_qp)
exp(gam_p[['coefficients']])
exp(confint.default(gam_p))
exp(gam_nb[['coefficients']])
exp(confint.default(gam_nb))
exp(gam_qp[['coefficients']])
exp(confint.default(gam_qp))
mean(TN4$NumOutbreaks)
var(TN4$NumOutbreaks)
SeasonTN<-subset(TN4, TN4$Month %in% c(4, 5, 6, 7))
ddply(SeasonTN, .(CovidEra), summarize, AvgOutbreaks=mean(NumOutbreaks), 
      MinOutbreaks=min(NumOutbreaks), MaxOutbreaks=max(NumOutbreaks))

## Wisconsin
WI<-subset(MonthState.allsettings, MonthState.allsettings$REPORTINGSITE=='Wisconsin')
WI.zeroes<-data.frame(MonthYear=c('2013-08', '2017-09'), 
                      REPORTINGSITE=c('Wisconsin', 'Wisconsin'), 
                      NumOutbreaks=c(0, 0), DateGroup=c(NA, NA))
WI2<-rbind(WI, WI.zeroes)
WI2$DateGroup<-paste(WI2$MonthYear, '-01', sep='')
WI2$Date<-as.Date(WI2$DateGroup)
WI3<-WI2[order(WI2$Date),]
WI3$Month<-month(WI3$Date)
WI3$CovidEra<-0
WI3$CovidEra[which(WI3$Date>=as.Date('03/01/2020', format='%m/%d/%Y'))]<-1
#WI3<-subset(WI3, WI3$Date>as.Date('08/31/2012', format='%m/%d/%Y'))
WI4<-subset(WI3, !(WI3$MonthYear %in% '2020-03'))
gam_p <- gam(NumOutbreaks ~ s(Month, bs='cc', k=12)+CovidEra, data=WI4, family=poisson)
dispersiontest(gam_p,trafo=1)
gam_nb <- gam(NumOutbreaks ~ s(Month, bs='cc', k=12)+CovidEra, data=WI4, family=nb) #Can just update to nb
gam_qp <- gam(NumOutbreaks ~ s(Month, bs='cc', k=12)+CovidEra, data=WI4, family=quasipoisson) #Can just update to nb
plot.gam(gam_qp)
summary(gam_p); summary(gam_nb); summary(gam_qp)
logLik(gam_p); logLik(gam_nb); logLik(gam_qp)
exp(gam_p[['coefficients']])
exp(confint.default(gam_p))
exp(gam_nb[['coefficients']])
exp(confint.default(gam_nb))
exp(gam_qp[['coefficients']])
exp(confint.default(gam_qp))
mean(WI4$NumOutbreaks)
var(WI4$NumOutbreaks)
SeasonWI<-subset(WI4, WI4$Month %in% c(4, 5, 6, 7))
ddply(SeasonWI, .(CovidEra), summarize, AvgOutbreaks=mean(NumOutbreaks), 
      MinOutbreaks=min(NumOutbreaks), MaxOutbreaks=max(NumOutbreaks))

## Michigan (Start from Aug, 2015) ---------------------------------------------
MI<-subset(MonthState.allsettings, MonthState.allsettings$REPORTINGSITE=='Michigan')
MI.zeroes<-data.frame(MonthYear=c('2012-09', '2013-05', '2013-08', '2013-09', '2013-11', 
                                  '2014-09', '2014-10', '2016-06', '2019-06', '2020-06', 
                                  '2020-07'), 
                      REPORTINGSITE=c('Michigan', 'Michigan', 'Michigan', 'Michigan', 'Michigan', 
                                      'Michigan', 'Michigan', 'Michigan', 'Michigan', 'Michigan', 
                                      'Michigan'), 
                      NumOutbreaks=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
                      DateGroup=c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))
MI2<-rbind(MI, MI.zeroes)
MI2$DateGroup<-paste(MI2$MonthYear, '-01', sep='')
MI2$Date<-as.Date(MI2$DateGroup)
MI3<-MI2[order(MI2$Date),]
MI3$Month<-month(MI3$Date)
MI3$CovidEra<-0
MI3$CovidEra[which(MI3$Date>=as.Date('03/01/2020', format='%m/%d/%Y'))]<-1
MI3<-subset(MI3, MI3$Date>as.Date('07/31/2015', format='%m/%d/%Y'))
MI4<-subset(MI3, !(MI3$MonthYear %in% '2020-03'))
gam_p <- gam(NumOutbreaks ~ s(Month, bs='cc', k=12)+CovidEra, data=MI4, family=poisson)
dispersiontest(gam_p,trafo=1)
gam_nb <- gam(NumOutbreaks ~ s(Month, bs='cc', k=12)+CovidEra, data=MI4, family=nb) #Can just update to nb
gam_qp <- gam(NumOutbreaks ~ s(Month, bs='cc', k=12)+CovidEra, data=MI4, family=quasipoisson) #Can just update to nb
plot.gam(gam_qp)
summary(gam_p); summary(gam_nb); summary(gam_qp)
logLik(gam_p); logLik(gam_nb); logLik(gam_qp)
exp(gam_p[['coefficients']])
exp(confint.default(gam_p))
exp(gam_nb[['coefficients']])
exp(confint.default(gam_nb))
exp(gam_qp[['coefficients']])
exp(confint.default(gam_qp))
mean(MI4$NumOutbreaks)
var(MI4$NumOutbreaks)
SeasonMI<-subset(MI4, MI4$Month %in% c(4, 5, 6, 7))
ddply(SeasonMI, .(CovidEra), summarize, AvgOutbreaks=mean(NumOutbreaks), 
      MinOutbreaks=min(NumOutbreaks), MaxOutbreaks=max(NumOutbreaks))

## South Carolina
SC<-subset(MonthState.allsettings, MonthState.allsettings$REPORTINGSITE=='South Carolina')
SC.zeroes<-data.frame(MonthYear=c('2012-10', '2013-08', '2014-08', '2014-11', '2016-09', 
                                  '2018-09', '2020-05', '2020-06', '2020-07'), 
                      REPORTINGSITE=c('South Carolina', 'South Carolina', 'South Carolina', 'South Carolina', 'South Carolina', 
                                      'South Carolina', 'South Carolina', 'South Carolina', 'South Carolina'), 
                      NumOutbreaks=c(0, 0, 0, 0, 0, 0, 0, 0, 0), 
                      DateGroup=c(NA, NA, NA, NA, NA, NA, NA, NA, NA))
SC2<-rbind(SC, SC.zeroes)
SC2$DateGroup<-paste(SC2$MonthYear, '-01', sep='')
SC2$Date<-as.Date(SC2$DateGroup)
SC3<-SC2[order(SC2$Date),]
SC3$Month<-month(SC3$Date)
SC3$CovidEra<-0
SC3$CovidEra[which(SC3$Date>=as.Date('03/01/2020', format='%m/%d/%Y'))]<-1
SC3<-subset(SC3, SC3$Date>as.Date('07/31/2015', format='%m/%d/%Y'))
SC4<-subset(SC3, !(SC3$MonthYear %in% '2020-03'))
gam_p <- gam(NumOutbreaks ~ s(Month, bs='cc', k=12)+CovidEra, data=SC4, family=poisson)
dispersiontest(gam_p,trafo=1)
gam_nb <- gam(NumOutbreaks ~ s(Month, bs='cc', k=12)+CovidEra, data=SC4, family=nb) #Can just update to nb
gam_qp <- gam(NumOutbreaks ~ s(Month, bs='cc', k=12)+CovidEra, data=SC4, family=quasipoisson) #Can just update to nb
plot.gam(gam_qp)
summary(gam_p); summary(gam_nb); summary(gam_qp)
logLik(gam_p); logLik(gam_nb); logLik(gam_qp)
exp(gam_p[['coefficients']])
exp(confint.default(gam_p))
exp(gam_nb[['coefficients']])
exp(confint.default(gam_nb))
exp(gam_qp[['coefficients']])
exp(confint.default(gam_qp))
mean(SC4$NumOutbreaks)
var(SC4$NumOutbreaks)
SeasonSC<-subset(SC4, SC4$Month %in% c(4, 5, 6, 7))
ddply(SeasonSC, .(CovidEra), summarize, AvgOutbreaks=mean(NumOutbreaks), 
      MinOutbreaks=min(NumOutbreaks), MaxOutbreaks=max(NumOutbreaks))

## Massachusetts (Start from Aug, 2016) ----------------------------------------
MA<-subset(MonthState.allsettings, MonthState.allsettings$REPORTINGSITE=='Massachusetts')
MA.zeroes<-data.frame(MonthYear=c('2012-08', '2013-07', '2013-08', '2014-05', '2014-07', 
                                  '2014-08', '2014-09', '2014-10', '2014-11', '2015-09', 
                                  '2016-07', '2020-04', '2020-05', '2020-06'), 
                      REPORTINGSITE=c('Massachusetts', 'Massachusetts', 'Massachusetts', 'Massachusetts', 'Massachusetts', 
                                      'Massachusetts', 'Massachusetts', 'Massachusetts', 'Massachusetts', 'Massachusetts',
                                      'Massachusetts', 'Massachusetts', 'Massachusetts', 'Massachusetts'),
                      NumOutbreaks=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
                      DateGroup=c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))
MA2<-rbind(MA, MA.zeroes)
MA2$DateGroup<-paste(MA2$MonthYear, '-01', sep='')
MA2$Date<-as.Date(MA2$DateGroup)
MA3<-MA2[order(MA2$Date),]
MA3$Month<-month(MA3$Date)
MA3$CovidEra<-0
MA3$CovidEra[which(MA3$Date>=as.Date('03/01/2020', format='%m/%d/%Y'))]<-1
MA3<-subset(MA3, MA3$Date>as.Date('07/31/2016', format='%m/%d/%Y'))
MA4<-subset(MA3, !(MA3$MonthYear %in% '2020-03'))
gam_p <- gam(NumOutbreaks ~ s(Month, bs='cc', k=12)+CovidEra, data=MA4, family=poisson)
dispersiontest(gam_p,trafo=1)
gam_nb <- gam(NumOutbreaks ~ s(Month, bs='cc', k=12)+CovidEra, data=MA4, family=nb) #Can just update to nb
gam_qp <- gam(NumOutbreaks ~ s(Month, bs='cc', k=12)+CovidEra, data=MA4, family=quasipoisson) #Can just update to nb
plot.gam(gam_qp)
summary(gam_p); summary(gam_nb); summary(gam_qp)
logLik(gam_p); logLik(gam_nb); logLik(gam_qp)
exp(gam_p[['coefficients']])
exp(confint.default(gam_p))
exp(gam_nb[['coefficients']])
exp(confint.default(gam_nb))
exp(gam_qp[['coefficients']])
exp(confint.default(gam_qp))
mean(MA4$NumOutbreaks)
var(MA4$NumOutbreaks)
SeasonMA<-subset(MA4, MA4$Month %in% c(4, 5, 6, 7))
ddply(SeasonMA, .(CovidEra), summarize, AvgOutbreaks=mean(NumOutbreaks), 
      MinOutbreaks=min(NumOutbreaks), MaxOutbreaks=max(NumOutbreaks))

## Virginia
VA<-subset(MonthState.allsettings, MonthState.allsettings$REPORTINGSITE=='Virginia')
VA.zeroes<-data.frame(MonthYear=c('2016-07', '2018-07', '2020-06', '2020-07'), 
                      REPORTINGSITE=c('Virginia', 'Virginia', 'Virginia', 'Virginia'),
                      NumOutbreaks=c(0, 0, 0, 0), 
                      DateGroup=c(NA, NA, NA, NA))
VA2<-rbind(VA, VA.zeroes)
VA2$DateGroup<-paste(VA2$MonthYear, '-01', sep='')
VA2$Date<-as.Date(VA2$DateGroup)
VA3<-VA2[order(VA2$Date),]
VA3$Month<-month(VA3$Date)
VA3$CovidEra<-0
VA3$CovidEra[which(VA3$Date>=as.Date('03/01/2020', format='%m/%d/%Y'))]<-1
VA3<-subset(VA3, VA3$Date>as.Date('07/31/2016', format='%m/%d/%Y'))
VA4<-subset(VA3, !(VA3$MonthYear %in% '2020-03'))
gam_p <- gam(NumOutbreaks ~ s(Month, bs='cc', k=12)+CovidEra, data=VA4, family=poisson)
dispersiontest(gam_p,trafo=1)
gam_nb <- gam(NumOutbreaks ~ s(Month, bs='cc', k=12)+CovidEra, data=VA4, family=nb) #Can just update to nb
gam_qp <- gam(NumOutbreaks ~ s(Month, bs='cc', k=12)+CovidEra, data=VA4, family=quasipoisson) #Can just update to nb
plot.gam(gam_qp)
summary(gam_p); summary(gam_nb); summary(gam_qp)
logLik(gam_p); logLik(gam_nb); logLik(gam_qp)
exp(gam_p[['coefficients']])
exp(confint.default(gam_p))
exp(gam_nb[['coefficients']])
exp(confint.default(gam_nb))
exp(gam_qp[['coefficients']])
exp(confint.default(gam_qp))
mean(VA4$NumOutbreaks)
var(VA4$NumOutbreaks)
SeasonVA<-subset(VA4, VA4$Month %in% c(4, 5, 6, 7))
ddply(SeasonVA, .(CovidEra), summarize, AvgOutbreaks=mean(NumOutbreaks), 
      MinOutbreaks=min(NumOutbreaks), MaxOutbreaks=max(NumOutbreaks))

## New Mexico (Start from Aug, 2018) -------------------------------------------
NM<-subset(MonthState.allsettings, MonthState.allsettings$REPORTINGSITE=='New Mexico')
NM.zeroes<-data.frame(MonthYear=c('2012-08', '2012-09', '2013-03', '2013-06', '2013-07',
                                  '2013-09', '2013-11', '2013-12', '2014-07', '2015-01',
                                  '2015-05', '2015-07', '2015-09', '2016-06', '2016-07',
                                  '2016-08', '2016-09', '2017-06', '2017-08', '2017-09',
                                  '2018-07', '2018-08', '2018-10', '2018-11', '2019-06', 
                                  '2019-07', '2019-09', '2019-12', '2020-03', '2020-04', 
                                  '2020-05', '2020-06', '2020-07'), 
                      REPORTINGSITE=c('New Mexico', 'New Mexico', 'New Mexico', 'New Mexico', 'New Mexico', 
                                      'New Mexico', 'New Mexico', 'New Mexico', 'New Mexico', 'New Mexico',
                                      'New Mexico', 'New Mexico', 'New Mexico', 'New Mexico', 'New Mexico', 
                                      'New Mexico', 'New Mexico', 'New Mexico', 'New Mexico', 'New Mexico',
                                      'New Mexico', 'New Mexico', 'New Mexico', 'New Mexico', 'New Mexico', 
                                      'New Mexico', 'New Mexico', 'New Mexico', 'New Mexico', 'New Mexico',
                                      'New Mexico', 'New Mexico', 'New Mexico'),
                      NumOutbreaks=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                     0, 0, 0), 
                      DateGroup=c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                  NA, NA, NA))
NM2<-rbind(NM, NM.zeroes)
NM2$DateGroup<-paste(NM2$MonthYear, '-01', sep='')
NM2$Date<-as.Date(NM2$DateGroup)
NM3<-NM2[order(NM2$Date),]
NM3$Month<-month(NM3$Date)
NM3$CovidEra<-0
NM3$CovidEra[which(NM3$Date>=as.Date('03/01/2020', format='%m/%d/%Y'))]<-1
NM3<-subset(NM3, NM3$Date>as.Date('08/31/2018', format='%m/%d/%Y'))
NM4<-subset(NM3, !(NM3$MonthYear %in% '2020-03'))
gam_y <- gam(NumOutbreaks ~ s(Month, bs='cc', k=12)+CovidEra, data=NM4, family=poisson)
plot.gam(gam_y)
title(main = "New Mexico, adjusted for season")
summary(gam_y)
exp(gam_y[["coefficients"]]) # Rate ratio
confint.default(gam_y)
exp(confint.default(gam_y)) # CI for the rate ratio

gam_y <- gam(NumOutbreaks ~ CovidEra, data=NM4, family=poisson)
glm_y <- glm(NumOutbreaks ~ CovidEra, data=NM4, family=poisson)
plot.gam(gam_y)
title(main = "New Mexico")
summary(gam_y)
summary(glm_y)
exp(gam_y[["coefficients"]]) # Rate ratio
confint.default(gam_y)
exp(confint.default(gam_y)) # CI for the rate ratio

## Wyoming
WY<-subset(MonthState.allsettings, MonthState.allsettings$REPORTINGSITE=='Wyoming')
WY.zeroes<-data.frame(MonthYear=c('2012-08', '2012-10', '2013-01', '2013-07', '2013-08',
                                  '2013-09', '2013-12', '2014-02', '2014-06', '2014-08',
                                  '2015-01', '2015-06', '2015-07', '2015-11', '2016-03',
                                  '2016-06', '2016-07', '2016-08', '2016-10', '2017-02',
                                  '2017-05', '2017-06', '2017-07', '2017-08', '2017-09', 
                                  '2017-10', '2018-02', '2018-05', '2018-06', '2018-07',
                                  '2018-08', '2018-10', '2018-11', '2019-04', '2019-06', 
                                  '2019-07', '2019-08', '2019-09', '2020-05', '2020-06', 
                                  '2020-07'), 
                      REPORTINGSITE=c('Wyoming', 'Wyoming', 'Wyoming', 'Wyoming', 'Wyoming', 
                                      'Wyoming', 'Wyoming', 'Wyoming', 'Wyoming', 'Wyoming',
                                      'Wyoming', 'Wyoming', 'Wyoming', 'Wyoming', 'Wyoming', 
                                      'Wyoming', 'Wyoming', 'Wyoming', 'Wyoming', 'Wyoming',
                                      'Wyoming', 'Wyoming', 'Wyoming', 'Wyoming', 'Wyoming', 
                                      'Wyoming', 'Wyoming', 'Wyoming', 'Wyoming', 'Wyoming',
                                      'Wyoming', 'Wyoming', 'Wyoming', 'Wyoming', 'Wyoming', 
                                      'Wyoming', 'Wyoming', 'Wyoming', 'Wyoming', 'Wyoming',
                                      'Wyoming'),
                      NumOutbreaks=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                     0), 
                      DateGroup=c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                  NA))
WY2<-rbind(WY, WY.zeroes)
WY2$DateGroup<-paste(WY2$MonthYear, '-01', sep='')
WY2$Date<-as.Date(WY2$DateGroup)
WY3<-WY2[order(WY2$Date),]
WY3$Month<-month(WY3$Date)
WY3$CovidEra<-0
WY3$CovidEra[which(WY3$Date>=as.Date('03/01/2020', format='%m/%d/%Y'))]<-1
WY3<-subset(WY3, WY3$Date>as.Date('08/31/2018', format='%m/%d/%Y'))
WY4<-subset(WY3, !(WY3$MonthYear %in% '2020-03'))
gam_y <- gam(NumOutbreaks ~ s(Month, bs='cc', k=12)+CovidEra, data=WY4, family=poisson)
plot.gam(gam_y)
title(main = "Wyoming, adjusted for season")
summary(gam_y)
exp(gam_y[["coefficients"]]) # Rate ratio
confint.default(gam_y)
exp(confint.default(gam_y)) # CI for the rate ratio

gam_y <- gam(NumOutbreaks ~ CovidEra, data=WY4, family=poisson)
glm_y <- glm(NumOutbreaks ~ CovidEra, data=WY4, family=poisson)
title(main = "Wyoming")
summary(gam_y)
summary(glm_y)
exp(gam_y[["coefficients"]]) # Rate ratio
confint.default(gam_y)
exp(confint.default(gam_y)) # CI for the rate ratio


## Nebraska
NE<-subset(MonthState.allsettings, MonthState.allsettings$REPORTINGSITE=='Nebraska')
NE.zeroes<-data.frame(MonthYear=c('2013-02', '2013-07', '2013-08', '2013-11', '2014-06', 
                                  '2014-07', '2014-08', '2014-09', '2014-10', '2015-06', 
                                  '2015-07', '2015-08', '2015-10', '2016-07', '2017-08', 
                                  '2017-04', '2018-04', '2018-08', '2018-10', '2020-05'), 
                      REPORTINGSITE=c('Nebraska', 'Nebraska', 'Nebraska', 'Nebraska', 'Nebraska', 
                                      'Nebraska', 'Nebraska', 'Nebraska', 'Nebraska', 'Nebraska', 
                                      'Nebraska', 'Nebraska', 'Nebraska', 'Nebraska', 'Nebraska', 
                                      'Nebraska', 'Nebraska', 'Nebraska', 'Nebraska', 'Nebraska'),
                      NumOutbreaks=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                     0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
                      DateGroup=c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))
NE2<-rbind(NE, NE.zeroes)
NE2$DateGroup<-paste(NE2$MonthYear, '-01', sep='')
NE2$Date<-as.Date(NE2$DateGroup)
NE3<-NE2[order(NE2$Date),]
NE3$Month<-month(NE3$Date)
NE3$CovidEra<-0
NE3$CovidEra[which(NE3$Date>=as.Date('03/01/2020', format='%m/%d/%Y'))]<-1
NE3<-subset(WY3, WY3$Date>as.Date('08/31/2019', format='%m/%d/%Y'))
NE4<-subset(NE3, !(NE3$MonthYear %in% '2020-03'))
gam_y <- gam(NumOutbreaks ~ s(Month, bs='cc', k=12)+CovidEra, data=NE4, family=poisson)
plot.gam(gam_y)
title(main = "Nebraska, adjusted for season")
summary(gam_y)
exp(gam_y[["coefficients"]]) # Rate ratio
confint.default(gam_y)
exp(confint.default(gam_y)) # CI for the rate ratio

gam_y <- gam(NumOutbreaks ~ CovidEra, data=NE4, family=poisson)
glm_y <- glm(NumOutbreaks ~ CovidEra, data=NE4, family=poisson)
summary(gam_y)
summary(glm_y)
exp(gam_y[["coefficients"]]) # Rate ratio
confint.default(gam_y)
exp(confint.default(gam_y)) # CI for the rate ratio

##All 9 states, adjusting using indicator variables
All9States<-rbind(MN4, OH4, OR4, WI4, TN4, MA4, MI4, SC4, VA4)
gam_pooled <- gam(NumOutbreaks ~ s(Month, bs='cc', k=12)+CovidEra+REPORTINGSITE, data=All9States, family=quasipoisson)
plot.gam(gam_pooled)
title(main = "All 9 States")
summary(gam_pooled)
exp(gam_pooled[["coefficients"]]) # Rate ratio
confint.default(gam_pooled)
exp(confint.default(gam_pooled)) # CI for the rate ratio

All9States.noTN<-rbind(MN4, OH4, OR4, WI4, MA4, MI4, SC4, VA4)
gam_pooled.noTN <- gam(NumOutbreaks ~ s(Month, bs='cc', k=12)+CovidEra+REPORTINGSITE, data=All9States.noTN, family=quasipoisson)
summary(gam_pooled.noTN)
exp(gam_pooled.noTN[["coefficients"]]) # Rate ratio
confint.default(gam_pooled.noTN)
exp(confint.default(gam_pooled.noTN)) # CI for the rate ratio

#All 9 states, using only April-July and not adjusting for season any further
All9States.summer<-subset(All9States, All9States$Month %in% c(4, 5, 6, 7))
glm.summer<-glm(NumOutbreaks~CovidEra+REPORTINGSITE, data=All9States.summer, family=quasipoisson)
summary(glm.summer)
exp(glm.summer[["coefficients"]]) # Rate ratio
confint.default(glm.summer)
exp(confint.default(glm.summer)) # CI for the rate ratio

#Graph of monthly incidence by state
All9States.march<-rbind(MN3, OH3, OR3, WI3, TN3, MA3, MI3, SC3, VA3)



PreCovid<-ddply(subset(All9States.march, All9States.march$Month %in% c(3, 4, 5, 6, 7) & All9States.march$CovidEra==0), .(REPORTINGSITE, Month),
      summarize, AvgOutbreaks=mean(NumOutbreaks), MinOutbreaks=min(NumOutbreaks), MaxOutbreaks=max(NumOutbreaks))
PreCovid$MonthName<-NA
PreCovid$MonthName[which(PreCovid$Month==3)]<-'Mar'
PreCovid$MonthName[which(PreCovid$Month==4)]<-'Apr'
PreCovid$MonthName[which(PreCovid$Month==5)]<-'May'
PreCovid$MonthName[which(PreCovid$Month==6)]<-'Jun'
PreCovid$MonthName[which(PreCovid$Month==7)]<-'Jul'

PostCovid<-subset(All9States.march, All9States.march$Month %in% c(3, 4, 5, 6, 7) & All9States.march$CovidEra==1)
PostCovid$MonthName<-NA
PostCovid$MonthName[which(PostCovid$Month==3)]<-'Mar'
PostCovid$MonthName[which(PostCovid$Month==4)]<-'Apr'
PostCovid$MonthName[which(PostCovid$Month==5)]<-'May'
PostCovid$MonthName[which(PostCovid$Month==6)]<-'Jun'
PostCovid$MonthName[which(PostCovid$Month==7)]<-'Jul'

ggplot(data=PreCovid)+geom_ribbon(aes(x=Month, ymin=MinOutbreaks, ymax=MaxOutbreaks, group=REPORTINGSITE), fill="grey70")+
   geom_line(data=PreCovid, aes(x=Month, y=AvgOutbreaks, group=REPORTINGSITE), linetype=1)+
   facet_wrap(~REPORTINGSITE)+geom_line(data=PostCovid, aes(x=Month, y=NumOutbreaks, group=REPORTINGSITE), linetype=2)+
   labs(x='Month', y='Average monthly outbreaks')+
   scale_x_continuous(breaks=c(3, 4, 5, 6, 7), labels=c('Mar', 'Apr', 'May', 'Jun', 'Jul'))+
   theme_bw()
################################################################################
# gam(NumOutbreaks ~ s(Month, bs='cc', k=12)+CovidEra)
# by different settings for 5 states (Minnesota, Ohio, Oregon, Tennessee, Wisconsin)
Norostat_5states <- Norostat[Norostat$REPORTINGSITE
                             %in% c("Minnesota", "Ohio", "Oregon",
                                    "Tennessee","Wisconsin"), ]
Monthsetting.allstates<-ddply(Norostat_5states, .(MonthYear, Setting), summarize, NumOutbreaks=sum(Outbreak))
Monthsetting.allstates$DateGroup<-as.Date(Monthsetting.allstates$MonthYear, format='%Y-%m')

## Long term care
LTC <-subset(Monthsetting.allstates, Monthsetting.allstates$Setting=='Long term care')
LTC.zeroes<-data.frame(MonthYear=c('2013-09'), 
                       Setting=c('Long term care'), 
                       NumOutbreaks=c(0), DateGroup=c(NA))
LTC2<-rbind(LTC, LTC.zeroes)
LTC2$DateGroup<-paste(LTC2$MonthYear, '-01', sep='')
LTC2$Date<-as.Date(LTC2$DateGroup)
LTC3<-LTC2[order(LTC2$Date),]
LTC3$Month<-month(LTC3$Date)
LTC3$CovidEra<-0
LTC3$CovidEra[which(LTC3$Date>=as.Date('03/01/2020', format='%m/%d/%Y'))]<-1
LTC3<-subset(LTC3, LTC3$Date>as.Date('07/31/2012', format='%m/%d/%Y')) #time lag between outbreak and report
LTC4<-subset(LTC3, !(LTC3$MonthYear %in% '2020-03'))
gam_y <- gam(NumOutbreaks ~ s(Month, bs='cc', k=12)+CovidEra, data=LTC4, family=poisson)
plot.gam(gam_y)
title(main = "Long term care")
summary(gam_y)
exp(gam_y[["coefficients"]]) # Rate ratio
confint.default(gam_y)
exp(confint.default(gam_y)) # CI for the rate ratio

## Hopspital/other healthcare
Hos <-subset(Monthsetting.allstates, Monthsetting.allstates$Setting=='Hospital/Other healthcare')
Hos.zeroes<-data.frame(MonthYear=c('2012-08', '2012-09', '2012-10', '2013-05', '2013-06', 
                                   '2013-08', '2013-09', '2014-06', '2014-08', '2014-09', 
                                   '2014-11', '2015-07', '2015-09', '2015-10', '2015-11',
                                   '2016-02', '2016-04', '2016-07', '2016-08', '2016-09',
                                   '2017-05', '2017-07', '2017-08', '2017-10', '2017-12',
                                   '2018-04', '2018-06', '2018-07', '2018-08', '2018-09',
                                   '2018-11', '2019-05', '2019-07', '2020-04', '2020-05', 
                                   '2020-07'), 
                       Setting=c('Hospital/Other healthcare', 'Hospital/Other healthcare', 'Hospital/Other healthcare', 'Hospital/Other healthcare', 'Hospital/Other healthcare', 
                                 'Hospital/Other healthcare', 'Hospital/Other healthcare', 'Hospital/Other healthcare', 'Hospital/Other healthcare', 'Hospital/Other healthcare',
                                 'Hospital/Other healthcare', 'Hospital/Other healthcare', 'Hospital/Other healthcare', 'Hospital/Other healthcare', 'Hospital/Other healthcare',
                                 'Hospital/Other healthcare', 'Hospital/Other healthcare', 'Hospital/Other healthcare', 'Hospital/Other healthcare', 'Hospital/Other healthcare',
                                 'Hospital/Other healthcare', 'Hospital/Other healthcare', 'Hospital/Other healthcare', 'Hospital/Other healthcare', 'Hospital/Other healthcare',
                                 'Hospital/Other healthcare', 'Hospital/Other healthcare', 'Hospital/Other healthcare', 'Hospital/Other healthcare', 'Hospital/Other healthcare',
                                 'Hospital/Other healthcare', 'Hospital/Other healthcare', 'Hospital/Other healthcare', 'Hospital/Other healthcare', 'Hospital/Other healthcare',
                                 'Hospital/Other healthcare'), 
                       NumOutbreaks=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                      0, 0, 0, 0, 0, 0), 
                       DateGroup=c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                   NA, NA, NA, NA, NA, NA))
Hos2<-rbind(Hos, Hos.zeroes)
Hos2$DateGroup<-paste(Hos2$MonthYear, '-01', sep='')
Hos2$Date<-as.Date(Hos2$DateGroup)
Hos3<-Hos2[order(Hos2$Date),]
Hos3$Month<-month(Hos3$Date)
Hos3$CovidEra<-0
Hos3$CovidEra[which(Hos3$Date>=as.Date('03/01/2020', format='%m/%d/%Y'))]<-1
Hos3<-subset(Hos3, Hos3$Date>as.Date('07/31/2012', format='%m/%d/%Y')) #time lag between outbreak and report
Hos4<-subset(Hos3, !(Hos3$MonthYear %in% '2020-03'))
gam_y <- gam(NumOutbreaks ~ s(Month, bs='cc', k=12)+CovidEra, data=Hos4, family=poisson)
plot.gam(gam_y)
title(main = "Hospital/Other healthcare")
summary(gam_y)
exp(gam_y[["coefficients"]]) # Rate ratio
confint.default(gam_y)
exp(confint.default(gam_y)) # CI for the rate ratio

## Child daycare
Child <-subset(Monthsetting.allstates, Monthsetting.allstates$Setting=='Child daycare')
Child.zeroes<-data.frame(MonthYear=c('2012-10', '2012-12', '2013-05', '2013-09', '2013-11', 
                                     '2014-06', '2014-08','2015-03', '2015-06', '2015-07', 
                                     '2015-11', '2016-02', '2016-06', '2016-10', '2018-06', 
                                     '2019-07', '2019-08', '2020-03', '2020-04', '2020-05',
                                     '2020-06', '2020-07'), 
                         Setting=c('Child daycare', 'Child daycare', 'Child daycare', 'Child daycare', 'Child daycare', 
                                   'Child daycare', 'Child daycare', 'Child daycare', 'Child daycare', 'Child daycare', 
                                   'Child daycare', 'Child daycare', 'Child daycare', 'Child daycare', 'Child daycare', 
                                   'Child daycare', 'Child daycare', 'Child daycare', 'Child daycare', 'Child daycare', 
                                   'Child daycare', 'Child daycare'), 
                         NumOutbreaks=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                        0, 0), 
                         DateGroup=c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                     NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                     NA, NA))
Child2<-rbind(Child, Child.zeroes)
Child2$DateGroup<-paste(Child2$MonthYear, '-01', sep='')
Child2$Date<-as.Date(Child2$DateGroup)
Child3<-Child2[order(Child2$Date),]
Child3$Month<-month(Child3$Date)
Child3$CovidEra<-0
Child3$CovidEra[which(Child3$Date>=as.Date('03/01/2020', format='%m/%d/%Y'))]<-1
Child3<-subset(Child3, Child3$Date>as.Date('07/31/2012', format='%m/%d/%Y')) #time lag between outbreak and report
Child4<-subset(Child3, !(Child3$MonthYear %in% '2020-03'))
gam_y <- gam(NumOutbreaks ~ s(Month, bs='cc', k=12)+CovidEra, data=Child4, family=poisson)
plot.gam(gam_y)
title(main = "Child daycare")
summary(gam_y)
exp(gam_y[["coefficients"]]) # Rate ratio
confint.default(gam_y)
exp(confint.default(gam_y)) # CI for the rate ratio

## School/college/university
School <-subset(Monthsetting.allstates, Monthsetting.allstates$Setting=='School/college/university')
School.zeroes<-data.frame(MonthYear=c('2013-06', '2013-07', '2013-08', '2014-07', '2014-08',
                                      '2015-05', '2015-07', '2015-08', '2016-07', '2016-08', 
                                      '2018-03', '2018-06', '2018-07', '2019-06', '2019-07', 
                                      '2019-08', '2020-04', '2020-05', '2020-06'), 
                          Setting=c('School/college/university', 'School/college/university', 'School/college/university', 'School/college/university', 'School/college/university',
                                    'School/college/university', 'School/college/university', 'School/college/university', 'School/college/university', 'School/college/university', 
                                    'School/college/university', 'School/college/university', 'School/college/university', 'School/college/university', 'School/college/university', 
                                    'School/college/university', 'School/college/university', 'School/college/university', 'School/college/university'), 
                          NumOutbreaks=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                         0, 0, 0, 0, 0, 0, 0, 0, 0), 
                          DateGroup=c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                      NA, NA, NA, NA, NA, NA, NA, NA, NA))
School2<-rbind(School, School.zeroes)
School2$DateGroup<-paste(School2$MonthYear, '-01', sep='')
School2$Date<-as.Date(School2$DateGroup)
School3<-School2[order(School2$Date),]
School3$Month<-month(School3$Date)
School3$CovidEra<-0
School3$CovidEra[which(School3$Date>=as.Date('03/01/2020', format='%m/%d/%Y'))]<-1
School3<-subset(School3, School3$Date>as.Date('07/31/2012', format='%m/%d/%Y')) #time lag between outbreak and report
School4<-subset(School3, !(School3$MonthYear %in% '2020-03'))
gam_y <- gam(NumOutbreaks ~ s(Month, bs='cc', k=12)+CovidEra, data=School4, family=poisson)
plot.gam(gam_y)
title(main = "School/college/university")
summary(gam_y)
exp(gam_y[["coefficients"]]) # Rate ratio
confint.default(gam_y)
exp(confint.default(gam_y)) # CI for the rate ratio

## Others
Monthsetting.allstates$Setting[which(Monthsetting.allstates$Setting %in% NA)]<-'Others'
Others <- subset(Monthsetting.allstates, Monthsetting.allstates$Setting=='Others')
Others.zeroes<-data.frame(MonthYear=c('2020-05', '2020-06', '2020-07'), 
                          Setting=c('Others', 'Others', 'Others'), 
                          NumOutbreaks=c(0, 0, 0), 
                          DateGroup=c(NA, NA, NA))
Others2<-rbind(Others, Others.zeroes)
Others2$DateGroup<-paste(Others2$MonthYear, '-01', sep='')
Others2$Date<-as.Date(Others2$DateGroup)
Others3<-Others2[order(Others2$Date),]
Others3$Month<-month(Others3$Date)
Others3$CovidEra<-0
Others3$CovidEra[which(Others3$Date>=as.Date('03/01/2020', format='%m/%d/%Y'))]<-1
Others3<-subset(Others3, Others3$Date>as.Date('07/31/2012', format='%m/%d/%Y')) #time lag between outbreak and report
Others4<-subset(Others3, !(Others3$MonthYear %in% '2020-03'))
gam_y <- gam(NumOutbreaks ~ s(Month, bs='cc', k=12)+CovidEra, data=Others4, family=poisson)
plot.gam(gam_y)
title(main = "Others")
summary(gam_y)
exp(gam_y[["coefficients"]]) # Rate ratio
confint.default(gam_y)
exp(confint.default(gam_y)) # CI for the rate ratio

################################################################################
# Differences between covid and non-covid, by different settings
# adjusting for states
# 5 states (Minnesota, Ohio, Oregon, Tennessee, Wisconsin)
Monthsetting.allstates.adj<-ddply(Norostat_5states, .(MonthYear, Setting, REPORTINGSITE), summarize, NumOutbreaks=sum(Outbreak))
Monthsetting.allstates.adj$DateGroup<-as.Date(Monthsetting.allstates.adj$MonthYear, format='%Y-%m')

## Long term care adjusted for states
LTC.adj <-subset(Monthsetting.allstates.adj, Monthsetting.allstates.adj$Setting=='Long term care')

year <- rep(2012:2020, each = 60)
month <- rep(c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12'), each = 5, times = 9)
Setting <- rep("Long term care", 540)
REPORTINGSITE <- rep(c('Minnesota', 'Ohio', 'Oregon', 'Tennessee', 'Wisconsin'), 108)
NumOutbreaks <- rep(NA, 540)
DateGroup <- rep(NA, 540)

LTC.blank <- data.frame(Year = year, Month = month, Setting = Setting, 
                        REPORTINGSITE = REPORTINGSITE, NumOutbreaks = NumOutbreaks, 
                        DateGroup = DateGroup)
LTC.blank$MonthYear <- paste(LTC.blank$Year, LTC.blank$Month, sep = '-')
LTC.blank <- LTC.blank[, 3:7]

library(dplyr)
LTC_merge <- left_join(LTC.blank, LTC.adj, by  = c("MonthYear", "REPORTINGSITE", "Setting"))
LTC_m2 <- LTC_merge[, c(5, 1, 2, 6, 7)]
LTC_m2$NumOutbreaks.y[which(is.na(LTC_m2$NumOutbreaks.y))] <- 0

LTC_m2$DateGroup.y<-paste(LTC_m2$MonthYear, '-01', sep='')
LTC_m2$Date<-as.Date(LTC_m2$DateGroup)
LTC_m3<-LTC_m2[order(LTC_m2$Date),]
LTC_m3$Month<-month(LTC_m3$Date)
LTC_m3$CovidEra<-0
LTC_m3$CovidEra[which(LTC_m3$Date>=as.Date('03/01/2020', format='%m/%d/%Y'))]<-1
LTC_m3<-subset(LTC_m3, LTC_m3$Date>as.Date('07/31/2012', format='%m/%d/%Y')) #time lag between outbreak and report
#LTC_m3<-subset(LTC_m3, LTC_m3$Date<as.Date('08/01/2020', format='%m/%d/%Y')) #time lag between outbreak and report
LTC_m4<-subset(LTC_m3, !(LTC_m3$MonthYear %in% '2020-03'))
gam_y <- gam(NumOutbreaks.y ~ s(Month, bs='cc', k=12)+ CovidEra + REPORTINGSITE, data=LTC_m4, family=poisson)
plot.gam(gam_y)
title(main = "Long term care")
summary(gam_y)
exp(gam_y[["coefficients"]]) # Rate ratio
confint.default(gam_y)
exp(confint.default(gam_y)) # CI for the rate ratio

## Hospital/other healthcare adjusted for states
hos.adj <-subset(Monthsetting.allstates.adj, Monthsetting.allstates.adj$Setting=='Hospital/Other healthcare')
year <- rep(2012:2020, each = 60)
month <- rep(c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12'), each = 5, times = 9)
Setting_hos <- rep("Hospital/Other healthcare", 540)
REPORTINGSITE <- rep(c('Minnesota', 'Ohio', 'Oregon', 'Tennessee', 'Wisconsin'), 108)
NumOutbreaks <- rep(NA, 540)
DateGroup <- rep(NA, 540)

hos.blank <- data.frame(Year = year, Month = month, Setting = Setting_hos, 
                        REPORTINGSITE = REPORTINGSITE, NumOutbreaks = NumOutbreaks, 
                        DateGroup = DateGroup)
hos.blank$MonthYear <- paste(hos.blank$Year, hos.blank$Month, sep = '-')
hos.blank <- hos.blank[, 3:7]

hos_merge <- left_join(hos.blank, hos.adj, by  = c("MonthYear", "REPORTINGSITE", "Setting"))
hos_m2 <- hos_merge[, c(5, 1, 2, 6, 7)]
hos_m2$NumOutbreaks.y[which(is.na(hos_m2$NumOutbreaks.y))] <- 0

hos_m2$DateGroup.y<-paste(hos_m2$MonthYear, '-01', sep='')
hos_m2$Date<-as.Date(hos_m2$DateGroup)
hos_m3<-hos_m2[order(hos_m2$Date),]
hos_m3$Month<-month(hos_m3$Date)
hos_m3$CovidEra<-0
hos_m3$CovidEra[which(hos_m3$Date>=as.Date('03/01/2020', format='%m/%d/%Y'))]<-1
hos_m3<-subset(hos_m3, hos_m3$Date>as.Date('07/31/2012', format='%m/%d/%Y')) #time lag between outbreak and report
#hos_m3<-subset(hos_m3, hos_m3$Date<as.Date('08/01/2020', format='%m/%d/%Y')) #time lag between outbreak and report
hos_m4<-subset(hos_m3, !(hos_m3$MonthYear %in% '2020-03'))
gam_y <- gam(NumOutbreaks.y ~ s(Month, bs='cc', k=12)+ CovidEra + REPORTINGSITE, data=hos_m4, family=poisson)
plot.gam(gam_y)
title(main = "Hospital/Other healthcare")
summary(gam_y)
exp(gam_y[["coefficients"]]) # Rate ratio
confint.default(gam_y)
exp(confint.default(gam_y)) # CI for the rate ratio

## Child daycare adjusted for states
Child.adj <-subset(Monthsetting.allstates.adj, Monthsetting.allstates.adj$Setting=='Child daycare')

year <- rep(2012:2020, each = 60)
month <- rep(c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12'), each = 5, times = 9)
Setting_child <- rep("Child daycare", 540)
REPORTINGSITE <- rep(c('Minnesota', 'Ohio', 'Oregon', 'Tennessee', 'Wisconsin'), 108)
NumOutbreaks <- rep(NA, 540)
DateGroup <- rep(NA, 540)

Child.blank <- data.frame(Year = year, Month = month, Setting = Setting_child, 
                          REPORTINGSITE = REPORTINGSITE, NumOutbreaks = NumOutbreaks, 
                          DateGroup = DateGroup)
Child.blank$MonthYear <- paste(Child.blank$Year, Child.blank$Month, sep = '-')
Child.blank <- Child.blank[, 3:7]

Child_merge <- left_join(Child.blank, Child.adj, by  = c("MonthYear", "REPORTINGSITE", "Setting"))
Child_m2 <- Child_merge[, c(5, 1, 2, 6, 7)]
Child_m2$NumOutbreaks.y[which(is.na(Child_m2$NumOutbreaks.y))] <- 0

Child_m2$DateGroup.y<-paste(Child_m2$MonthYear, '-01', sep='')
Child_m2$Date<-as.Date(Child_m2$DateGroup)
Child_m3<-Child_m2[order(Child_m2$Date),]
Child_m3$Month<-month(Child_m3$Date)
Child_m3$CovidEra<-0
Child_m3$CovidEra[which(Child_m3$Date>=as.Date('03/01/2020', format='%m/%d/%Y'))]<-1
Child_m3<-subset(Child_m3, Child_m3$Date>as.Date('07/31/2012', format='%m/%d/%Y')) #time lag between outbreak and report
C#hild_m3<-subset(Child_m3, Child_m3$Date<as.Date('08/01/2020', format='%m/%d/%Y')) #time lag between outbreak and report
Child_m4<-subset(Child_m3, !(Child_m3$MonthYear %in% '2020-03'))
gam_y <- gam(NumOutbreaks.y ~ s(Month, bs='cc', k=12)+ CovidEra + REPORTINGSITE, data=Child_m4, family=poisson)
plot.gam(gam_y)
title(main = "Child daycare")
summary(gam_y)
exp(gam_y[["coefficients"]]) # Rate ratio
confint.default(gam_y)
exp(confint.default(gam_y)) # CI for the rate ratio


## School/college/university adjusted for states
School.adj <-subset(Monthsetting.allstates.adj, Monthsetting.allstates.adj$Setting=='School/college/university')

year <- rep(2012:2020, each = 60)
month <- rep(c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12'), each = 5, times = 9)
Setting_sch <- rep("School/college/university", 540)
REPORTINGSITE <- rep(c('Minnesota', 'Ohio', 'Oregon', 'Tennessee', 'Wisconsin'), 108)
NumOutbreaks <- rep(NA, 540)
DateGroup <- rep(NA, 540)

sch.blank <- data.frame(Year = year, Month = month, Setting = Setting_sch, 
                        REPORTINGSITE = REPORTINGSITE, NumOutbreaks = NumOutbreaks, 
                        DateGroup = DateGroup)
sch.blank$MonthYear <- paste(sch.blank$Year, sch.blank$Month, sep = '-')
sch.blank <- sch.blank[, 3:7]

sch_merge <- left_join(sch.blank, School.adj, by  = c("MonthYear", "REPORTINGSITE", "Setting"))
sch_m2 <- sch_merge[, c(5, 1, 2, 6, 7)]
sch_m2$NumOutbreaks.y[which(is.na(sch_m2$NumOutbreaks.y))] <- 0

sch_m2$DateGroup.y<-paste(sch_m2$MonthYear, '-01', sep='')
sch_m2$Date<-as.Date(sch_m2$DateGroup)
sch_m3<-sch_m2[order(sch_m2$Date),]
sch_m3$Month<-month(sch_m3$Date)
sch_m3$CovidEra<-0
sch_m3$CovidEra[which(sch_m3$Date>=as.Date('03/01/2020', format='%m/%d/%Y'))]<-1
sch_m3<-subset(sch_m3, sch_m3$Date>as.Date('07/31/2012', format='%m/%d/%Y')) #time lag between outbreak and report
#sch_m3<-subset(sch_m3, sch_m3$Date<as.Date('08/01/2020', format='%m/%d/%Y')) #time lag between outbreak and report
sch_m4<-subset(sch_m3, !(sch_m3$MonthYear %in% '2020-03'))
gam_y <- gam(NumOutbreaks.y ~ s(Month, bs='cc', k=12)+ CovidEra + REPORTINGSITE, data=sch_m4, family=poisson)
plot.gam(gam_y)
title(main = "School/college/university")
summary(gam_y)
exp(gam_y[["coefficients"]]) # Rate ratio
confint.default(gam_y)
exp(confint.default(gam_y)) # CI for the rate ratio

## Others adjusted for states
Monthsetting.allstates.adj$Setting[which(Monthsetting.allstates.adj$Setting %in% NA)]<-'Others'
Others.adj <-subset(Monthsetting.allstates.adj, Monthsetting.allstates.adj$Setting=='Others')

year <- rep(2012:2020, each = 60)
month <- rep(c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12'), each = 5, times = 9)
Setting_oth <- rep("Others", 540)
REPORTINGSITE <- rep(c('Minnesota', 'Ohio', 'Oregon', 'Tennessee', 'Wisconsin'), 108)
NumOutbreaks <- rep(NA, 540)
DateGroup <- rep(NA, 540)

oth.blank <- data.frame(Year = year, Month = month, Setting = Setting_oth, 
                        REPORTINGSITE = REPORTINGSITE, NumOutbreaks = NumOutbreaks, 
                        DateGroup = DateGroup)
oth.blank$MonthYear <- paste(oth.blank$Year, oth.blank$Month, sep = '-')
oth.blank <- oth.blank[, 3:7]

oth_merge <- left_join(oth.blank, Others.adj, by  = c("MonthYear", "REPORTINGSITE", "Setting"))
oth_m2 <- oth_merge[, c(5, 1, 2, 6, 7)]
oth_m2$NumOutbreaks.y[which(is.na(oth_m2$NumOutbreaks.y))] <- 0

oth_m2$DateGroup.y<-paste(oth_m2$MonthYear, '-01', sep='')
oth_m2$Date<-as.Date(oth_m2$DateGroup)
oth_m3<-oth_m2[order(oth_m2$Date),]
oth_m3$Month<-month(oth_m3$Date)
oth_m3$CovidEra<-0
oth_m3$CovidEra[which(oth_m3$Date>=as.Date('03/01/2020', format='%m/%d/%Y'))]<-1
oth_m3<-subset(oth_m3, oth_m3$Date>as.Date('07/31/2012', format='%m/%d/%Y')) #time lag between outbreak and report
oth_m3<-subset(oth_m3, oth_m3$Date<as.Date('08/01/2020', format='%m/%d/%Y')) #time lag between outbreak and report
oth_m4<-subset(oth_m3, !(oth_m3$MonthYear %in% '2020-03'))
gam_y <- gam(NumOutbreaks.y ~ s(Month, bs='cc', k=12)+ CovidEra + REPORTINGSITE, data=oth_m4, family=poisson)
plot.gam(gam_y)
title(main = "Others")
summary(gam_y)
exp(gam_y[["coefficients"]]) # Rate ratio
confint.default(gam_y)
exp(confint.default(gam_y)) # CI for the rate ratio

###All Nine States#####3
Norostat$MonthYear<-format(as.Date(Norostat$OutbreakDate), '%Y-%m')
Norostat$Outbreak<-1
Norostat_9states <- Norostat[Norostat$REPORTINGSITE
                             %in% c("Minnesota", "Ohio", "Oregon",
                                    "Tennessee","Wisconsin", "Massachusetts", 
                                    'South Carolina', "Michigan", "Virginia"), ]

# 9 states (Minnesota, Ohio, Oregon, Tennessee, Wisconsin)
Monthsetting.allstates.adj<-ddply(Norostat_9states, .(MonthYear, Setting, REPORTINGSITE), summarize, 
                                  NumOutbreaks=sum(Outbreak))
Monthsetting.allstates.adj$DateGroup<-as.Date(Monthsetting.allstates.adj$MonthYear, format='%Y-%m')

## Long term care adjusted for states
LTC.adj <-subset(Monthsetting.allstates.adj, Monthsetting.allstates.adj$Setting=='Long term care')

year <- rep(2012:2020, each = 108)
month <- rep(c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12'), each = 9, times = 9)
Setting <- rep("Long term care", 972)
REPORTINGSITE <- rep(c('Minnesota', 'Ohio', 'Oregon', 'Tennessee', 'Wisconsin', 'Michigan', 'South Carolina',
                       'Massachusetts', 'Virginia'), 108)
NumOutbreaks <- rep(NA, 972)
DateGroup <- rep(NA, 972)

LTC.blank <- data.frame(Year = year, Month = month, Setting = Setting, 
                        REPORTINGSITE = REPORTINGSITE, NumOutbreaks = NumOutbreaks, 
                        DateGroup = DateGroup)
LTC.blank$MonthYear <- paste(LTC.blank$Year, LTC.blank$Month, sep = '-')
LTC.blank <- LTC.blank[, 3:7]

library(dplyr)
LTC_merge <- left_join(LTC.blank, LTC.adj, by  = c("MonthYear", "REPORTINGSITE", "Setting"))
LTC_m2 <- LTC_merge[, c(5, 1, 2, 6, 7)]

StartDates<-data.frame(REPORTINGSITE=c('Massachusetts', 'Michigan', 'Minnesota', 'Ohio', 'Oregon', 'South Carolina', 
                                       'Tennessee', 'Virginia', 'Wisconsin'), 
                       DateEntry=c(as.Date(c('08/01/2016', '08/01/2015', '08/01/2012', '08/01/2012', '08/01/2012',
                                             '08/01/2015', '08/01/2012', '08/01/2016', '08/01/2012'), format='%m/%d/%Y')))
LTC_m3<-merge(LTC_m2, StartDates, by='REPORTINGSITE', all.x=TRUE)
LTC_m4<-subset(LTC_m3, as.Date(paste(LTC_m3$MonthYear, '-01', sep=''), format='%Y-%m-%d')>=LTC_m3$DateEntry)

LTC_m4$NumOutbreaks.y[which(is.na(LTC_m4$NumOutbreaks.y))] <- 0

LTC_m4$DateGroup.y<-paste(LTC_m4$MonthYear, '-01', sep='')
LTC_m4$Date<-as.Date(LTC_m4$DateGroup)
LTC_m4<-LTC_m4[order(LTC_m4$Date),]
require(lubridate)
LTC_m4$Month<-month(LTC_m4$Date)
LTC_m4$CovidEra<-0
LTC_m4$CovidEra[which(LTC_m4$Date>=as.Date('03/01/2020', format='%m/%d/%Y'))]<-1
LTC_m4<-subset(LTC_m4, LTC_m4$Date>as.Date('07/31/2012', format='%m/%d/%Y')) #time lag between outbreak and report
LTC_m4<-subset(LTC_m4, LTC_m4$Date<as.Date('08/01/2020', format='%m/%d/%Y')) #time lag between outbreak and report
LTC_m4<-subset(LTC_m4, !(LTC_m4$MonthYear %in% '2020-03'))
gam_y <- gam(NumOutbreaks.y ~ s(Month, bs='cc', k=12)+ CovidEra + REPORTINGSITE, data=LTC_m4, family=poisson)
dispersiontest(gam_y,trafo=1)
plot.gam(gam_y)
title(main = "Long term care")
exp(gam_y[["coefficients"]]) # Rate ratio
confint.default(gam_y)
exp(confint.default(gam_y)) # CI for the rate ratio
gam_nb <- gam(NumOutbreaks.y ~ s(Month, bs='cc', k=12)+ CovidEra + REPORTINGSITE, data=LTC_m4, family=nb)
gam_qp <- gam(NumOutbreaks.y ~ s(Month, bs='cc', k=12)+ CovidEra + REPORTINGSITE, data=LTC_m4, family=quasipoisson)
summary(gam_y); summary(gam_nb); summary(gam_qp)
exp(gam_y[["coefficients"]]) # Rate ratio
confint.default(gam_y)
exp(confint.default(gam_y)) # CI for the rate ratio

## Hospital/other healthcare adjusted for states, all 9
hos.adj <-subset(Monthsetting.allstates.adj, Monthsetting.allstates.adj$Setting=='Hospital/Other healthcare')
year <- rep(2012:2020, each = 108)
month <- rep(c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12'), each = 9, times = 9)
Setting_hos <- rep("Hospital/Other healthcare", 972)
NumOutbreaks <- rep(NA, 972)
DateGroup <- rep(NA, 972)

hos.blank <- data.frame(Year = year, Month = month, Setting = Setting_hos, 
                        REPORTINGSITE = REPORTINGSITE, NumOutbreaks = NumOutbreaks, 
                        DateGroup = DateGroup)
hos.blank$MonthYear <- paste(hos.blank$Year, hos.blank$Month, sep = '-')
hos.blank <- hos.blank[, 3:7]


hos_merge <- left_join(hos.blank, hos.adj, by  = c("MonthYear", "REPORTINGSITE", "Setting"))
hos_m2 <- hos_merge[, c(5, 1, 2, 6, 7)]
hos_m3<-merge(hos_m2, StartDates, by='REPORTINGSITE', all.x=TRUE)
hos_m4<-subset(hos_m3, as.Date(paste(hos_m3$MonthYear, '-01', sep=''), format='%Y-%m-%d')>=hos_m3$DateEntry)

hos_m4$NumOutbreaks.y[which(is.na(hos_m4$NumOutbreaks.y))] <- 0

hos_m4$DateGroup.y<-paste(hos_m4$MonthYear, '-01', sep='')
hos_m4$Date<-as.Date(hos_m4$DateGroup)
hos_m4<-hos_m4[order(hos_m4$Date),]
hos_m4$Month<-month(hos_m4$Date)
hos_m4$CovidEra<-0
hos_m4$CovidEra[which(hos_m4$Date>=as.Date('03/01/2020', format='%m/%d/%Y'))]<-1
#hos_m4<-subset(hos_m4, hos_m4$Date>as.Date('07/31/2012', format='%m/%d/%Y')) #time lag between outbreak and report
hos_m4<-subset(hos_m4, hos_m4$Date<as.Date('08/01/2020', format='%m/%d/%Y')) #time lag between outbreak and report
hos_m5<-subset(hos_m4, !(hos_m4$MonthYear %in% '2020-03'))
gam_y <- gam(NumOutbreaks.y ~ s(Month, bs='cc', k=12)+ CovidEra + REPORTINGSITE, data=hos_m5, family=poisson)
dispersiontest(gam_y,trafo=1)
plot.gam(gam_y)
title(main = "Hospital/Other healthcare")
summary(gam_y)
exp(gam_y[["coefficients"]]) # Rate ratio
confint.default(gam_y)
exp(confint.default(gam_y)) # CI for the rate ratio
gam_nb <- gam(NumOutbreaks.y ~ s(Month, bs='cc', k=12)+ CovidEra + REPORTINGSITE, data=hos_m5, family=nb)
gam_qp <- gam(NumOutbreaks.y ~ s(Month, bs='cc', k=12)+ CovidEra + REPORTINGSITE, data=hos_m5, family=quasipoisson)
summary(gam_nb); summary(gam_qp)

## Child daycare adjusted for states
Child.adj <-subset(Monthsetting.allstates.adj, Monthsetting.allstates.adj$Setting=='Child daycare')
Setting_child <- rep("Child daycare", 972)


Child.blank <- data.frame(Year = year, Month = month, Setting = Setting_child, 
                          REPORTINGSITE = REPORTINGSITE, NumOutbreaks = NumOutbreaks, 
                          DateGroup = DateGroup)
Child.blank$MonthYear <- paste(Child.blank$Year, Child.blank$Month, sep = '-')
Child.blank <- Child.blank[, 3:7]

Child_merge <- left_join(Child.blank, Child.adj, by  = c("MonthYear", "REPORTINGSITE", "Setting"))
Child_m2 <- Child_merge[, c(5, 1, 2, 6, 7)]
Child_m3<-merge(Child_m2, StartDates, by='REPORTINGSITE', all.x=TRUE)
Child_m4<-subset(Child_m3, as.Date(paste(Child_m3$MonthYear, '-01', sep=''), format='%Y-%m-%d')>=Child_m3$DateEntry)


Child_m4$NumOutbreaks.y[which(is.na(Child_m4$NumOutbreaks.y))] <- 0

Child_m4$DateGroup.y<-paste(Child_m4$MonthYear, '-01', sep='')
Child_m4$Date<-as.Date(Child_m4$DateGroup)
Child_m4<-Child_m4[order(Child_m4$Date),]
Child_m4$Month<-month(Child_m4$Date)
Child_m4$CovidEra<-0
Child_m4$CovidEra[which(Child_m4$Date>=as.Date('03/01/2020', format='%m/%d/%Y'))]<-1
Child_m4<-subset(Child_m4, Child_m4$Date<as.Date('08/01/2020', format='%m/%d/%Y')) #time lag between outbreak and report
Child_m4<-subset(Child_m4, !(Child_m4$MonthYear %in% '2020-03'))
gam_y <- gam(NumOutbreaks.y ~ s(Month, bs='cc', k=12)+ CovidEra + REPORTINGSITE, data=Child_m4, family=poisson)
dispersiontest(gam_y,trafo=1)
gam_nb <- gam(NumOutbreaks.y ~ s(Month, bs='cc', k=12)+ CovidEra + REPORTINGSITE, data=Child_m4, family=nb)
gam_qp <- gam(NumOutbreaks.y ~ s(Month, bs='cc', k=12)+ CovidEra + REPORTINGSITE, data=Child_m4, family=quasipoisson)
plot.gam(gam_y)
title(main = "Child daycare")
summary(gam_y); summary(gam_nb); summary(gam_qp)
exp(gam_y[["coefficients"]]) # Rate ratio
confint.default(gam_y)
exp(confint.default(gam_y)) # CI for the rate ratio

## School/college/university adjusted for states
School.adj <-subset(Monthsetting.allstates.adj, Monthsetting.allstates.adj$Setting=='School/college/university')
Setting_sch <- rep("School/college/university", 972)

sch.blank <- data.frame(Year = year, Month = month, Setting = Setting_sch, 
                        REPORTINGSITE = REPORTINGSITE, NumOutbreaks = NumOutbreaks, 
                        DateGroup = DateGroup)
sch.blank$MonthYear <- paste(sch.blank$Year, sch.blank$Month, sep = '-')
sch.blank <- sch.blank[, 3:7]

sch_merge <- left_join(sch.blank, School.adj, by  = c("MonthYear", "REPORTINGSITE", "Setting"))
sch_m2 <- sch_merge[, c(5, 1, 2, 6, 7)]
sch_m3<-merge(sch_m2, StartDates, by='REPORTINGSITE', all.x=TRUE)
sch_m4<-subset(sch_m3, as.Date(paste(sch_m3$MonthYear, '-01', sep=''), format='%Y-%m-%d')>=sch_m3$DateEntry)


sch_m4$NumOutbreaks.y[which(is.na(sch_m4$NumOutbreaks.y))] <- 0

sch_m4$DateGroup.y<-paste(sch_m4$MonthYear, '-01', sep='')
sch_m4$Date<-as.Date(sch_m4$DateGroup)
sch_m4<-sch_m4[order(sch_m4$Date),]
sch_m4$Month<-month(sch_m4$Date)
sch_m4$CovidEra<-0
sch_m4$CovidEra[which(sch_m4$Date>=as.Date('03/01/2020', format='%m/%d/%Y'))]<-1
sch_m4<-subset(sch_m4, sch_m4$Date<as.Date('08/01/2020', format='%m/%d/%Y')) #time lag between outbreak and report
sch_m4<-subset(sch_m4, !(sch_m4$MonthYear %in% '2020-03'))
gam_y <- gam(NumOutbreaks.y ~ s(Month, bs='cc', k=12)+ CovidEra + REPORTINGSITE, data=sch_m4, family=poisson)
dispersiontest(gam_y,trafo=1)
gam_nb <- gam(NumOutbreaks.y ~ s(Month, bs='cc', k=12)+ CovidEra + REPORTINGSITE, data=sch_m4, family=nb)
gam_qp <- gam(NumOutbreaks.y ~ s(Month, bs='cc', k=12)+ CovidEra + REPORTINGSITE, data=sch_m4, family=quasipoisson)
plot.gam(gam_y)
title(main = "School/college/university")
summary(gam_y); summary(gam_nb); summary(gam_qp)
exp(gam_y[["coefficients"]]) # Rate ratio
confint.default(gam_y)
exp(confint.default(gam_y)) # CI for the rate ratio

## Others adjusted for states
Monthsetting.allstates.adj$Setting[which(Monthsetting.allstates.adj$Setting %in% NA)]<-'Others'
Others.adj <-subset(Monthsetting.allstates.adj, Monthsetting.allstates.adj$Setting=='Others')
Setting_oth <- rep("Others", 972)

oth.blank <- data.frame(Year = year, Month = month, Setting = Setting_oth, 
                        REPORTINGSITE = REPORTINGSITE, NumOutbreaks = NumOutbreaks, 
                        DateGroup = DateGroup)
oth.blank$MonthYear <- paste(oth.blank$Year, oth.blank$Month, sep = '-')
oth.blank <- oth.blank[, 3:7]

oth_merge <- left_join(oth.blank, Others.adj, by  = c("MonthYear", "REPORTINGSITE", "Setting"))
oth_m2 <- oth_merge[, c(5, 1, 2, 6, 7)]
oth_m3<-merge(oth_m2, StartDates, by='REPORTINGSITE', all.x=TRUE)
oth_m4<-subset(oth_m3, as.Date(paste(oth_m3$MonthYear, '-01', sep=''), format='%Y-%m-%d')>=oth_m3$DateEntry)

oth_m4$NumOutbreaks.y[which(is.na(oth_m4$NumOutbreaks.y))] <- 0

oth_m4$DateGroup.y<-paste(oth_m4$MonthYear, '-01', sep='')
oth_m4$Date<-as.Date(oth_m4$DateGroup)
oth_m4<-oth_m4[order(oth_m4$Date),]
oth_m4$Month<-month(oth_m4$Date)
oth_m4$CovidEra<-0
oth_m4$CovidEra[which(oth_m4$Date>=as.Date('03/01/2020', format='%m/%d/%Y'))]<-1
oth_m4<-subset(oth_m4, oth_m4$Date<as.Date('08/01/2020', format='%m/%d/%Y')) #time lag between outbreak and report
oth_m4<-subset(oth_m4, !(oth_m4$MonthYear %in% '2020-03'))
gam_y <- gam(NumOutbreaks.y ~ s(Month, bs='cc', k=12)+ CovidEra + REPORTINGSITE, data=oth_m4, family=poisson)
dispersiontest(gam_y,trafo=1)

gam_nb <- gam(NumOutbreaks.y ~ s(Month, bs='cc', k=12)+ CovidEra + REPORTINGSITE, data=oth_m4, family=nb)
gam_qp <- gam(NumOutbreaks.y ~ s(Month, bs='cc', k=12)+ CovidEra + REPORTINGSITE, data=oth_m4, family=quasipoisson)
plot.gam(gam_y)
title(main = "Others")
summary(gam_y); summary(gam_nb); summary(gam_qp)
exp(gam_y[["coefficients"]]) # Rate ratio
confint.default(gam_y)
exp(confint.default(gam_y)) # CI for the rate ratio

## Boxplot for April-July pre/post COVID
require(ggplot2)
MN_bp<-subset(MN4, MN4$Month %in% c("4", "5", "6", "7"))
MN_bp$CovidEra <- factor(MN_bp$CovidEra,
                         levels = c(0,1),
                         labels = c("April-July 2012-2019", "April-July 2020"))
MN_bp$State <- "Minnesota"
OH_bp<-subset(OH4, OH4$Month %in% c("4", "5", "6", "7"))
OH_bp$CovidEra <- factor(OH_bp$CovidEra,
                         levels = c(0,1),
                         labels = c("April-July 2012-2019", "April-July 2020"))
OH_bp$State <- "Ohio"
OR_bp<-subset(OR4, OR4$Month %in% c("4", "5", "6", "7"))
OR_bp$CovidEra <- factor(OR_bp$CovidEra,
                         levels = c(0,1),
                         labels = c("April-July 2012-2019", "April-July 2020"))
OR_bp$State <- "Oregon"
TN_bp<-subset(TN4, TN4$Month %in% c("4", "5", "6", "7"))
TN_bp$CovidEra <- factor(TN_bp$CovidEra,
                         levels = c(0,1),
                         labels = c("April-July 2012-2019", "April-July 2020"))
TN_bp$State <- "Tennessee"
WI_bp<-subset(WI4, WI4$Month %in% c("4", "5", "6", "7"))
WI_bp$CovidEra <- factor(WI_bp$CovidEra,
                         levels = c(0,1),
                         labels = c("April-July 2012-2019", "April-July 2020"))
WI_bp$State <- "Wisconsin"
MI_bp<-subset(MI4, MI4$Month %in% c("4", "5", "6", "7"))
MI_bp$CovidEra <- factor(MI_bp$CovidEra,
                         levels = c(0,1),
                         labels = c("April-July 2012-2019", "April-July 2020"))
MI_bp$State <- "Michigan"
SC_bp<-subset(SC4, SC4$Month %in% c("4", "5", "6", "7"))
SC_bp$CovidEra <- factor(SC_bp$CovidEra,
                         levels = c(0,1),
                         labels = c("April-July 2012-2019", "April-July 2020"))
SC_bp$State <- "South Carolina"
MA_bp<-subset(MA4, MA4$Month %in% c("4", "5", "6", "7"))
MA_bp$CovidEra <- factor(MA_bp$CovidEra,
                         levels = c(0,1),
                         labels = c("April-July 2012-2019", "April-July 2020"))
MA_bp$State <- "Massachusetts"
VA_bp<-subset(VA4, VA4$Month %in% c("4", "5", "6", "7"))
VA_bp$CovidEra <- factor(VA_bp$CovidEra,
                         levels = c(0,1),
                         labels = c("April-July 2012-2019", "April-July 2020"))
VA_bp$State <- "Virginia"

bp_9states <- rbind(MN_bp, OH_bp, OR_bp, 
                    TN_bp, WI_bp, MI_bp,
                    SC_bp, MA_bp, VA_bp)

label.df1<-data.frame(State=rep(c('Minnesota', 'Ohio', 'Oregon', 
                                  'Tennessee', 'Wisconsin', 'Michigan', 
                                  'South Carolina', 'Massachusetts', 'Virginia'), each=2),
                      NumOutbreaks=c(5.82, 0.75, 8.36, 0.50, 7.93, 1.25, 
                                     2.56, 0, 8.96, 2, 6.88, 0.5,
                                     3.75, 0.50, 5.67, 0.75, 5.75, 0.75), 
                      CovidEra=rep(c(0, 1)))
label.df1$CovidEra <- factor(label.df1$CovidEra,
                             levels = c(0,1),
                             labels = c("April-July 2012-2019", "April-July 2020"))

bp_9s <- ggplot(bp_9states, aes(x = State, y = NumOutbreaks, fill=CovidEra)) + 
   geom_boxplot()+theme_classic()+ labs(x=' ', y='Number of outbreaks')+
   theme(axis.text.x = element_text(angle = 90, size=12), 
         legend.title =element_blank())+
   geom_text(data=label.df1, label=c('+     ', '     +', 
                                     '+     ', '     +', 
                                     '+     ', '     +', 
                                     '+     ', '     +', 
                                     '+     ', '     +', 
                                     '+     ', '     +', 
                                     '+     ', '     +', 
                                     '+     ', '     +', 
                                     '+     ', '     +'), size=6)
bp_9s
ggsave("bp_9states.pdf", plot = bp_9s)

bp_9s2 <- ggplot(bp_9states, aes(x = State, y = NumOutbreaks, fill=CovidEra)) + 
   geom_boxplot()+theme_classic()+ labs(x=' ', y='Number of outbreaks')+
   theme(axis.text.x = element_text(angle = 90, size=12), 
         legend.title =element_blank())+
   geom_text(data=label.df1, label=c('+     ', '     +', 
                                     '+     ', '     +', 
                                     '+     ', '     +', 
                                     '+     ', '     +', 
                                     '+     ', '     +', 
                                     '+     ', '     +', 
                                     '+     ', '     +', 
                                     '+     ', '     +', 
                                     '+     ', '     +'), size=6)+
   ggtitle('A')
bp_9s2

require(ggpubr)
ggsave(filename='/Users/aliciakraay/Dropbox/NoroCovid/BarsCombined.pdf', 
       plot=ggarrange(bp_9s, barplot.size, ncol=1, nrow=2), device='pdf', w=9, h=15)
