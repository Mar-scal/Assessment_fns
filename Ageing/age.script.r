

source("Y:\\Development\\Georges\\Data Inputs\\Ageing\\r\\fn\\import.age.data.r")
age.dat<-import.age.data()

banq.age<-subset(age.dat,bank=='Banquereau')
BN.age<-subset(age.dat,bank=='Browns North')
BS.age<-subset(age.dat,bank=='Browns South')
ger.age<-subset(age.dat,bank=='German')
sab.age<-subset(age.dat,bank=='Sable')

source("Y:\\Development\\Georges\\Data Inputs\\Ageing\\r\\fn\\lvb.plt.r")

#lvb.plt(BN.age)

yrs<-unique(banq.age$yr)

lvb.plt(banq.age,years=yrs)


yrs<-unique(ger.age$yr)

lvb.plt(ger.age,years=yrs)
