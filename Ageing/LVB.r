

LVB <- function(bank='Browns North',ylab='Shell height (mm)',xlab='Age',xlim=F,ylim=F,plot=T){

parameters<-c()


	

	A<-age.dat$age[age.dat$Bank==bank&!is.na(age.dat$age)]
	H<-age.dat$height[age.dat$Bank==bank&!is.na(age.dat$age)]
	DATA <- data.frame(A,H)
	ifelse(xlim == F,xlm <- c(min(floor(A)), max(ceiling(A))),xlm <- xlim)
	ifelse(ylim == F,ylm <- c(min(floor(H)), max(ceiling(H))),ylm <- ylim)

	if(plot==T){
		plot(A,H,ylab=ylab,xlab=xlab,main=bank,ylim=ylm,xlim=xlm)
		
	}
	
	# LVB model
	t <- sort(unique(A))

	 	# initial parameters
	 	mh <- c()
	 	for(j in 1:length(t)){mh[j] <- mean(DATA$H[DATA$A==t[j]])}
	 	walford <- lm(mh[2:length(mh)]~mh[1:(length(mh)-1)])
	 	hinf <- walford$coefficients[1]/(1-walford$coefficients[2])
	 	K <- -log(walford$coefficients[2])
	 	hmf <- lm(log(hinf-mh)~t)
	 	t0 <- (hmf$coefficients[1]-log(hinf))/K
 	 	start.par <- list(hinf=hinf, K=K, t0=t0)

	lvb.fit <- summary(nls(H~hinf*(1-exp(-K*(A-t0))), data = DATA,start = start.par))
	ht <- lvb.fit$parameters[1]*(1-exp(-lvb.fit$parameters[2]*(t-lvb.fit$parameters[3])))     # von Bertalanffy equation #

	if(plot==T){
		lines(t,ht,lwd=2,col='red')
#		mtext(xlab, 1, 3, outer = T, cex = 1.6)
#		mtext(ylab, 2, 3, outer = T, cex = 1.6)
	}

	for(j in 1:6){parameters[j]<-lvb.fit$parameters[j]}




names(parameters)<-c("Hinf","K","t0","Hinf.se","K.se","t0.se")
return(parameters)
}
