# This function is specific to the Condition environment project, but could be useful if you need to 
# get a big block of corrleation/r2 or some other measure data.  This is used to get the data for the correlation figure used in the 
# Condition-Envionment paper collaboration with Emmanuel Devred and Catherine Johnson.

#####################################  Function Summary ########################################################
# ARGUMENTS

# dat:        The raw data you want to compare, it should be a time series, the orginal intent of the data was a monthly time series of some variable.  I called the variable
#             of interest "covar", so that's what you'll need to have in your data
# cond.dat:   In this case this is the condition data, basically this could be any other data set, but for now it is set to pull out the "Condition"  variable, could easily be
#             generatlized to suit some other purpose.
#year         Let's you control the year range you want

# Here's a function to get all the correlation and r2 value
cor_dat <- function(dat = sst.raw.dat,cond.dat = aug.dat,year = 1998:2015)
{
  # Set up the output matrix.
  str.names <- c(paste(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),"last",sep=" "),
                 paste(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),"cur",sep=" "))
  #end.names <- c(paste(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),"last_end",sep="_"),
  #               paste(c("Jan","Feb","Mar","Apr"),"cur_end",sep="_"))
  #str.names <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr")
  
  resp <- data.frame(start = rep(NA,24*24),end = NA,
                     r2 = NA,p.r2 = NA,p.r2.lev = NA,
                     pearson=NA,p.pearson = NA,p.pear.lev = NA,
                     kendall=NA,p.kendall = NA,p.kend.lev=NA,
                     start.num = NA,end.num = NA)
  
  #names(resp.r2) <- end.names
  #resp.p <-  as.data.frame(matrix(NA,nrow=16,ncol=16),row.names = str.names)
  #names(resp.p) <- end.names
  # Subest the data to the years of interest
  dat <- dat[dat$year %in% year ,]
  cond.dat <- cond.dat[cond.dat$year %in% year,]
  count <- 1 # Start a counter
  # Run throught the loops to get the table data.
  for(j in 24:1)
  {
    for(i in 24:1)
    {
      # browser()
      # i=23
      # j=24
      # This gets the correlation for the current year spring data
      if(i > 12 && i <= j) 
      {
        if(i == j) tmp.covar <- dat[dat$month==(i-12),]
        if(i < j) {tmp.covar <- aggregate(value ~ year,FUN=sum,data=dat[dat$month %in% c((i:j)-12),])
        missingyr <- year[!year %in% tmp.covar$year]
        if(length(missingyr) > 0) tmp.covar <- arrange(join(tmp.covar, data.frame(year=missingyr, value=NA), type="full"), year)
        }
        tmp.covar <- tmp.covar$value
        cond.tmp <- cond.dat
      } # end if(i > 12 && i <= j) 
      # This gets the correlation for the previous year data
      if(i <= 12 && i <= j) 
      {
        # This is position i,i, eg. December is 12,12, so when they are the same is just the diagonal...
        if(i == j) {tmp.covar <- dat$value[dat$month==i] ; tmp.covar <- tmp.covar[-length(tmp.covar)] }
        if(i < j) 
        {
          covar.last.tmp <- aggregate(value ~ year,FUN=sum,data=dat[dat$month %in% c(i:j),])
          missingyr <- year[!year %in% covar.last.tmp$year]
          if(length(missingyr) > 0) covar.last.tmp <- arrange(join(covar.last.tmp, data.frame(year=missingyr, value=NA), type="full"), year)
          # Here the most recent year of data is of no use to us...
          covar.last.tmp <- covar.last.tmp[-nrow(covar.last.tmp),]
          # For this the first year we can't use, it is captured above in the i > 12 loop...
          # And in all cases it is all for months down in this loop.
          
          # Now add together, or not, as appropriate
          if(j > 12) 
          {
            covar.cur.tmp <- aggregate(value ~ year,FUN=sum,data=dat[dat$month %in% c(1:(j-12)),])[-1,]
            tmp.covar <- covar.last.tmp$value + covar.cur.tmp$value
          } # end if(j > 12) 
          
          if(j <= 12) tmp.covar <- covar.last.tmp$value 
        } # end if(i < j)
        cond.tmp <- cond.dat[-1,]
      } # end if if(i <= 12) 
      # And now we only do the below if i <= j
      # The various models to run.
      if(i <= j)
      {
        # Run the lm's for each
        print(paste0("i", i))
        print(paste0("j", j))
        lm.resp <- lm(cond.tmp$value~tmp.covar)
        pearson <- cor.test(tmp.covar, cond.tmp$value,use = "na.or.complete",method = "pearson")
        kendall <- cor.test(tmp.covar, cond.tmp$value, use = "na.or.complete",method = "kendall")
        #lm.resp <- lm(tmp.covar~aug.tmp$Condition)
        # Now extract the model results we want.
        resp$r2[count] <- signif(summary(lm.resp)$r.squared,digits=2)
        resp$p.r2[count] <- signif(coefficients(summary(lm.resp))[2,4],digits=2)
        if(resp$p.r2[count] < 0.05) resp$p.r2.lev[count] <- "*"
        if(resp$p.r2[count] < 0.01) resp$p.r2.lev[count] <- "**"
        if(resp$p.r2[count] < 0.001) resp$p.r2.lev[count] <- "***"
        # Now pearson results
        resp$pearson[count] <- signif(pearson$estimate,digits=2)
        resp$p.pearson[count] <- signif(pearson$p.value,digits=2)
        if(resp$p.pearson[count] < 0.05) resp$p.pear.lev[count] <- "*"
        if(resp$p.pearson[count] < 0.01) resp$p.pear.lev[count] <- "**"
        if(resp$p.pearson[count] < 0.001) resp$p.pear.lev[count] <- "***"
        # Finally Kendall
        # Now pearson results
        resp$kendall[count] <- signif(kendall$estimate,digits=2)
        resp$p.kendall[count] <- signif(kendall$p.value,digits=2)
        if(resp$p.kendall[count] < 0.05) resp$p.kend.lev[count] <- "*"
        if(resp$p.kendall[count] < 0.01) resp$p.kend.lev[count] <- "**"
        if(resp$p.kendall[count] < 0.001) resp$p.kend.lev[count] <- "***"
        
        resp$start[count] <- str.names[i]
        resp$end[count] <- str.names[j]
        resp$start.num[count] <- i
        resp$end.num[count] <- j
        count <- count +1
      } # end  if(i <= j)
    } # End i loop
  } # End j loop
  return(list(resp = resp))
} #end function
