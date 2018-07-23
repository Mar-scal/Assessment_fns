# source("Y:\\Development\\Georges\\Data Inputs\\Ageing\\r\\fn\\age.back.r")

age.back<-function(ht,linf=145.4,k=0.38,t0=0.5,dif=-1){

	a <- t0-log(1-ht/linf)/k

	a2<-a+dif 

	ht2 <- linf*(1-exp(-k*(A-t0)))

	data.frame(a,ht,a2,ht2,sug=(round(ht2/5)+1)*5)
	
}                   