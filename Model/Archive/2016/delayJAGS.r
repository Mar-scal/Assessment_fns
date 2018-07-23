# source("Y:/Assessment/2012/r/bugs/delayBUGS.r")


#delayBUGS <- function(model, input, priors, years, n = 60000, burn = 20000, thin = 10, nchains = 2, debug = F, wd="",parameters=c(names(priors),'K','P','B','R','mu','B.p','mu.p','B.change','pB0'),add.parameters=NULL,add.inits=NULL,pe=F){     #don't project within the model now, so B.p (projected biomass), etc, removed


delayJAGS <- function(model, input, priors, years, n = 60000, burn = 20000, parallel = T,seed = 123,
                      thin = 10, nchains = 2, wd="",parameters=c(names(priors),'K','P','B','R','mu'),
                      add.parameters=NULL,add.inits=NULL,pe=F){

	  require(R2WinBUGS)
		start<-Sys.time()
		
		data=input	
		if(!missing(priors)){
			
			# Prepare priors for WinBUGS
			for(i in 1:length(priors)){
				if(priors[[i]]$d%in%c("dlnorm","dnorm"))priors[[i]]$b<-1/priors[[i]]$b^2
			#	if(priors[[i]]$d=="dgamma")priors[[i]]$b<-1/priors[[i]]$b
			}
			prior.dat<- data.frame(par=names(priors),do.call("rbind",lapply(priors,rbind)))
			prior.lst<-list()
			for(i in seq(1,nrow(prior.dat)*2,2)){
				prior.lst[[i]]<-prior.dat$a[[ceiling(i/2)]]
				prior.lst[[i+1]]<-prior.dat$b[[ceiling(i/2)]]
			}
			names(prior.lst)<-paste(rep(prior.dat$par,2)[order(rep(1:nrow(prior.dat),2))],rep(c('a','b'),nrow(prior.dat)),sep='.')
			#browser()
			if(pe)prior.lst[which(lapply(prior.lst,length)>1)]<-as.list(data.frame(prior.lst[which(lapply(prior.lst,length)>1)])[1:input$NY,]) 	
			data=c(prior.lst,input)
		}
		# parameters to save
		parameters<-c(parameters, add.parameters)
		
		# initial values
		inits<-list()
		prior.dat<-data.frame(do.call("rbind",priors))
		
		for(i in 1:1){
			inits[[i]]<-sapply(1:nrow(prior.dat),function(x){rep(unlist(prior.dat[paste('i',i,sep='')][x,]),prior.dat$l[[x]])})
			if(pe){
				prior.dat$l[prior.dat$l>1]<-input$NY
				inits[[i]]<-sapply(1:nrow(prior.dat),function(x){rep(unlist(prior.dat[paste('i',i,sep='')][x,]),prior.dat$l[[x]])})
			}
			names(inits[[i]])<-names(priors)
			inits[[i]]<-c(inits[[i]],list(P=rep(1,length(years))),add.inits[[i]])
		}
		
		#browser()
		## Call to JAGS, do you want to run in parallel?
		if(parallel==F)
		{
		  tmp <- jags(data = data, parameters.to.save = parameters,  
		                       model.file = paste(wd,model,".bug",sep=""),n.chains = nchains, n.iter = n, n.burnin = burn, 
		                       n.thin = thin,jags.seed = seed)
		}
		
		if(parallel==T)
		{
		  tmp <- jags.parallel(data = data, parameters.to.save = parameters,  
		                       model.file = paste(wd,model,".bug",sep=""),n.chains = nchains, n.iter = n, n.burnin = burn, 
		                       n.thin = thin,jags.seed = seed)
		}
		
		print(Sys.time()-start)

		if(missing(years))years=1:input$NY
		input$iyr<-years
		out.lst<-list(data=input, sims.list=tmp$BUGSoutput$sims.list,median=tmp$BUGSoutput$median,mean=tmp$BUGSoutput$mean,
		              summary=tmp$BUGSoutput$summary)
		rm(tmp)

		out.lst	
} 
