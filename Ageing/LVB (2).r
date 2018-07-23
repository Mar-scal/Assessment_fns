# source("Y:\\Development\\Georges\\Data Inputs\\Ageing\\r\\fn\\LVB.r")


LVB <- function(age.dat, ylab='Shell height (mm)',xlab='Age',xlim=F,ylim=F,plt=T,color=rainbow(6)){

	param<-c()

	age.dat<-age.dat[!is.na(age.dat$age),]
	age.dat$bank<-as.character(age.dat$bank)

	
	ifelse(xlim == F,xlm <- c(min(floor(age.dat$age)), max(ceiling(age.dat$age))),xlm <- xlim)
	ifelse(ylim == F,ylm <- c(min(floor(age.dat$height)), max(ceiling(age.dat$height))),ylm <- ylim)

	
	# LVB model
	A <- sort(unique(age.dat$age))
	bk<- unique(age.dat$bank)
	parameters<-data.frame(matrix(nrow = length(bk)+1, ncol = 7))
	
	for(i in 1:length(bk)){
			
	 	# initial parameters
 		a <- sort(unique(subset(age.dat,bank==bank[i])$age))
	 	mh <- c()
	 	for(j in 1:length(a)){mh[j] <- mean(age.dat$height[age.dat$bank==bk[i]&age.dat$age==a[j]],na.rm=T)}
	 	walford <- lm(mh[2:length(mh)]~mh[1:(length(mh)-1)])
	 	hinf <- walford$coefficients[1]/(1-walford$coefficients[2])
	 	K <- -log(walford$coefficients[2])
	 	hmf <- lm(log(hinf-mh)~a)
	 	t0 <- (hmf$coefficients[1]-log(hinf))/K
 	 	start.par <- list(hinf=hinf, K=K, t0=t0)

		lvb.fit <- summary(nls(height~hinf*(1-exp(-K*(age-t0))), data = subset(age.dat,bank==bk[i]),start = start.par))
	
		for(j in 1:6){parameters[i,j+1]<-lvb.fit$parameters[j]}

	}
	parameters[1:length(bk),1]<-bk
	parameters[length(bk)+1,1]<-'Georges a'
	parameters[length(bk)+1,2:4]<-c(145.4,0.38,0.5)
	names(parameters)<-c("Bank","Hinf","K","t0","Hinf.se","K.se","t0.se")
	
	
	if(plt==T){
		windows(12,9)
		plot(age.dat$age,age.dat$height,type='n',ylab=ylab,xlab=xlab,ylim=ylm,xlim=xlm,cex.lab=1.25)
		for(i in 1:nrow(parameters)){
			ht <- parameters[i,2]*(1-exp(-parameters[i,3]*(A-parameters[i,4])))     # von Bertalanffy equation #
			lines(A,ht,lwd=2,col=color[i])
		}
		legend("bottomright",c('Georges a',as.character(bk)[5:1]),col=color[6:1],lty=1,bty='n',lwd=2,inset=0.1)
		#		mtext(xlab, 1, 3, outer = T, cex = 1.6)
		#		mtext(ylab, 2, 3, outer = T, cex = 1.6)
	}
	
	return(parameters)
}
