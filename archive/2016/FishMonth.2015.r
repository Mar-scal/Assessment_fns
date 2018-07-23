FishMonth <- function(bank = "Georges a", year = 2015, fleet = "ALL", boxes=F,get.data='dump',print=F,output=T,nafo.div="5ZEJ",export="F"){

# Function for calculating fishing effort on user specified bank by month
#		
#		Created by Ian Jonsen 14Mar2008, edited by Brad Hubley 28May2008
#
#	Requires: BB.xy (Browns Bank 100m isobath)
#               get.log() to retrieve MARFIS landings data
#				and the splancs library

	require(splancs)

	# get data from MARFIS (note this isn't sourced above so will give an error if file hasn't be previously loaded)
	get.marfis.logs(year=year,get.data=get.data,export=export)

	
#	if(get.data=='dump'){
#		dump.log<-read.csv("Y:\\Offshore scallop\\Amy\\2010fisherydata\\2010log.csv")
#		dump.slip<-read.csv("Y:\\Offshore scallop\\Amy\\2010fisherydata\\2010slip.csv")
#		assign("log.dat", dump.log, pos = 1)
#		assign("slip.dat", dump.slip, pos = 1)
#	}

	variables <- as.character(c("days", "h", "hm","crhm", "lbs",  "kg","mt","kg.h", "kg.hm","kg.crhm"))
	month <- as.character(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Total"))
	days.by.boat <- c()
	days <- c()
	h <- c()
	hm <- c()
	crhm <- c()
	lbs <- c()
	kg <- c()
	mt <- c()
	kg.h <- c()
	kg.hm <- c()
	kg.crhm <- c()
	days1 <- c()
	h1 <- c()
	hm1 <- c()
	crhm1 <- c()
	lbs1 <- c()
	kg1 <- c()
	mt1 <- c()
	kg.h1 <- c()
	kg.hm1 <- c()
	kg.crhm1 <- c()

	mdate <- seq(as.Date("2015-01-01"), length.out = 13, by = "1 month") # 

		
	


	if(fleet == "FT") boats <-  c(105736,105912,105457,106604,106605,102056)
	if(fleet == "WF"){ boats <- c(1555,4062,100199,101965,4031,1516,1518,4051,1548,5409,4055,152320)} # fortune lady 152320 moved to WF
	if(fleet == "ALL"){boats <- unique(log.dat$vrnum)}

	nafo1 <- unique(log.dat$nafo)
	if(bank == "German") bank1 <- "26C"
	if(bank == "Sable"){ bank1 <- "25A"
  	nafo1 <- c("4WH", "4WJ", "4WF", "4WL", "4WG")}
	if(bank == "Middle"){ bank1 <- "25A"
		nafo1 <- "4WE"}
	if(bank == "SPB"){ bank1 <- "3PS"}
	if(bank == "Banquereau"){ bank1 <- "25B"}
	if(bank == "Georges a") { 
		bank1 <- "27A"
		seedboxes <- GB.boxes
		nafo1 <- nafo.div
		}
	if(bank == "Georges b") { 
		bank1 <- "27B"
		nafo1 <- nafo.div
		}
	if(bank == "Browns North"){ bank1 <- "26A"
		seedboxes <- BBn.boxes}
	if(bank == "Browns South"){ bank1 <- "26B"}
	 
	if (bank =="Georges") {
		nafo1 <- nafo.div
		bank1 <- c("27A","27B")
	}
	
	
	for(m in 1:12){

		# Effort
		for(b in 1:length(boats)){days.by.boat[b] <- with(subset(log.dat, nafo %in% nafo1 & sfa %in% bank1 & fished >= mdate[m] & fished < mdate[m+1] & vrnum == boats[b]), length(unique(fished)))}
		days[m] <- sum(days.by.boat)
		h[m] <- round(sum(with(subset(log.dat, nafo %in% nafo1 &  sfa %in% bank1 & vrnum %in% boats & fished >= mdate[m] & fished < mdate[m+1]), numtow * avgtime / 60), na.rm = T))
		hm[m] <- round(sum(with(subset(log.dat, nafo %in% nafo1 &  sfa %in% bank1 & vrnum %in% boats& fished >= mdate[m] & fished < mdate[m+1]), slip.dat[match(mdid, slip.dat$mdid),]$gear * 0.3048 * numrake * numtow * avgtime / 60), na.rm = T))
		crhm[m] <- round(sum(with(subset(log.dat, nafo %in% nafo1 &  sfa %in% bank1 & vrnum %in% boats& fished >= mdate[m] & fished < mdate[m+1]), with(slip.dat[match(mdid, slip.dat$mdid),], gear * 0.3048 * numshuck) * numrake * numtow * avgtime / 60), na.rm = T))
	
		# Catch
		lbs[m] <- round(with(subset(log.dat, nafo %in% nafo1 &  sfa %in% bank1 & vrnum %in% boats & fished >= mdate[m] & fished < mdate[m+1]), sum(pro.repwt * 2.2046, na.rm = T)))
		kg[m] <- round(lbs[m] / 2.2046)
		mt[m] <- round(kg[m] * 0.001)

		# CPUE
		kg.h[m] <- round(kg[m] / sum(with(subset(log.dat, nafo %in% nafo1 &  sfa %in% bank1 & vrnum %in% boats & fished >= mdate[m] & fished < mdate[m+1]), numtow * avgtime / 60), na.rm = T), 2)
		kg.hm[m] <- round(kg[m] / hm[m], 2)
		kg.crhm[m] <- round(kg[m] / crhm[m], 2)	
		
	}
	
	if(boxes==T){
		table2 <- array(NA,c(13,10,length(seedboxes)))
		dimnames(table2) <- list(month,variables,names(seedboxes))
		table3 <- array(NA,c(13,11,length(seedboxes)))
		for(i in length(seedboxes)){
			for(m in 1:12){
				# seedboxes
				xy <- data.frame(seedboxes[i])
				names(xy)<-c("lon","lat")
				log.dat.box <- log.dat[with(log.dat[!is.na(log.dat$lon), ], inout(cbind(lon,lat),xy , bound = T)), ]
				# Effort
				for(b in 1:length(boats)){days.by.boat[b] <- with(subset(log.dat.box, sfa %in% bank1 & fished >= mdate[m] & fished < mdate[m+1] & vrnum == boats[b]), length(unique(fished)))}
				days1[m] <- sum(days.by.boat)
				h1[m] <- round(sum(with(subset(log.dat.box, sfa %in% bank1 & vrnum %in% boats & fished >= mdate[m] & fished < mdate[m+1]), numtow * avgtime / 60), na.rm = T))
				hm1[m] <- round(sum(with(subset(log.dat.box, sfa %in% bank1 & vrnum %in% boats& fished >= mdate[m] & fished < mdate[m+1]), slip.dat[match(mdid, slip.dat$mdid),]$gear * 0.3048 * numrake * numtow * avgtime / 60), na.rm = T))
				crhm1[m] <- round(sum(with(subset(log.dat.box, sfa %in% bank1 & vrnum %in% boats& fished >= mdate[m] & fished < mdate[m+1]), with(slip.dat[match(mdid, slip.dat$mdid),], gear * 0.3048 * numshuck) * numrake * numtow * avgtime / 60), na.rm = T))
				# Catch
				lbs1[m] <- round(with(subset(log.dat.box, sfa %in% bank1 & vrnum %in% boats& fished >= mdate[m] & fished < mdate[m+1]), sum(pro.repwt * 2.2046, na.rm = T)))
				kg1[m] <- round(lbs1[m] / 2.2046)
				mt1[m] <- round(kg1[m] * 0.001)
				# CPUE
				kg.h1[m] <- round(kg1[m] / sum(with(subset(log.dat.box, sfa %in% bank1 & vrnum %in% boats & fished >= mdate[m] & fished < mdate[m+1]), numtow * avgtime / 60), na.rm = T), 2)
				kg.hm1[m] <- round(kg1[m] / hm1[m], 2)
				kg.crhm1[m] <- round(kg1[m] / crhm1[m], 2)	
			}	
			days1[13] <- sum(days1)
			h1[13] <- sum(h1)
			hm1[13] <- sum(hm1)
			crhm1[13] <- sum(crhm1)
			lbs1[13] <- sum(lbs1)
			kg1[13] <- sum(kg1)
			mt1[13] <- sum(mt1)
			kg.h1[13] <- round(mean(kg.h1, na.rm = T),2)
			kg.hm1[13] <- round(mean(kg.hm1, na.rm = T),2)
			kg.crhm1[13] <- round(mean(kg.crhm1, na.rm = T),2)
			table2[,,i] <- cbind( days1, h1, hm1, crhm1, lbs1, kg1, mt1, kg.h1, kg.hm1, kg.crhm1)
			table3[,,i] <- cbind(month, days1, h1, hm1, crhm1, lbs1, kg1, mt1, kg.h1, kg.hm1, kg.crhm1)
		}
	}

	days[13] <- sum(days)
	h[13] <- sum(h)
	hm[13] <- sum(hm)
	crhm[13] <- sum(crhm)
	lbs[13] <- sum(lbs)
	kg[13] <- sum(kg)
	mt[13] <- sum(mt)
	kg.h[13] <- round(mean(kg.h, na.rm = T),2)
	kg.hm[13] <- round(mean(kg.hm, na.rm = T),2)
	kg.crhm[13] <- round(mean(kg.crhm, na.rm = T),2)


	table1 <- data.frame(month, days, h, hm, crhm, lbs, kg, mt, kg.h, kg.hm, kg.crhm)

	
	if(print==T){
		print(bank)
		print(table1)
		if(boxes==T){
			for(i in 1:length(names(seedboxes))){
				table2<-data.frame(table3[,,i])
				dimnames(table2)[2]<-dimnames(table1)[2]
				print(names(seedboxes)[i])
				print(table2)
			}
		}
	}

	if(output==T){
		
		output<-list(bank=bank,total.bank=table1,fleet=boats)
		if(boxes==T){output<-list(bank=bank,total.bank=table1,box.names=names(seedboxes),management.boxes=table2,fleet=boats)}
		
		return(output)
	}
	
}
