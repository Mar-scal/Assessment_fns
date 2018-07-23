#			source("Y:\\Survey\\r\\summary\\fn\\LVB.plt.r")



LVB.plt <- function(bank='Browns North',xlim=F,ylim=F,years,plot=T,get.data=T, Win = T){

	if(Win){
		if(get.data)	import.survey.data('age')
		}
			
	if(missing(years)) 	years<-sort(years.age(bank))

	ifelse(xlim == F,xlm <- c(0, max(ceiling(age.dat$age),na.rm=T)),xlm <- xlim)
	ifelse(ylim == F,ylm <- c(0, max(ceiling(age.dat$height),na.rm=T)),ylm <- ylim)
	
	age.dat <- age.dat[age.dat$bank==bank,]
	age.dat <- age.dat[order(age.dat$yr), ]
	age.gdat <- groupedData(height ~ age | yr, data = age.dat, order.groups = F)
	start <- c(150, 0.3, 0)
	
	ht.nlme <- nlme(model = height ~ linf * (1 - exp(-k * (age - tzero))), data = age.gdat, fixed = linf + k + tzero ~ 1, random = linf + k ~ 1, start = start, na.action = na.omit, control = list(maxIter = 100), method = "ML")
		
	fit <- coef(ht.nlme)
	fixed <- ht.nlme$coef$fixed
	age <- list(NULL)
	
	if(plot){
		pdf("lvb.pdf", width = 9, height = length(years)/2*3, pointsize = 14)
		par(mfrow=c(length(years)/2,2), mar = c(2,2,0,0), omi = c(0.75, 0.75, 0.1, 0.1))
		j <- 0
		for(i in years){
			j <- j + 1
			age[[j]] <- with(subset(age.gdat, yr == i), seq(min(age, na.rm = T), max(age, na.rm = T)))
			plot(height ~ age, data = age.gdat, subset = yr == i, las = 1, ylim = ylm, xlim = xlm, mgp = c(0.5, 0.5, 0), tcl = -0.3, xlab = "", ylab = "", col = "grey50")
			grid(col = "grey40")
			lines(age[[j]], fit[j,1] * (1 - exp(-fit[j,2] * (age[[j]] - fit[j,3]))), lwd = 2, col = 'red')
			lines(age[[j]], fixed[1] * (1 - exp(-fixed[2] * (age[[j]] - fixed[3]))), lwd = 2, col = 'blue')
			text(1.5, ylm[2]-10, i, cex = 1.5)
			linf <- round(fit[j,1], 2)
			k1 <- round(fit[j,2], 2)
			t0 <- round(fixed[3], 2)
			text(0.5, ylm[2]-25, substitute(L[infinity] * " =  " * linf), adj = 0, cex = 1.1)
			text(0.5, ylm[2]-40, substitute(k * " = " * k1) ,adj = 0, cex = 1.1)
			text(0.5, ylm[2]-55, substitute(t[0] * " = " * t0), adj = 0, cex = 1.1)
		}
		mtext("Age", 1, 1, outer = T)
		mtext("Shell height (mm)", 2, 1, outer = T)
		
		dev.off()
	}
	
	if(plot==F) return(fixed)

}
