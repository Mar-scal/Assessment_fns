####################################################################
## This function creates the biomass plots + projection boxplot for offshore
## This is a very useful function.
###################################################################
# Update history
# January 2016 - Revised by DK 
# April 2016, Revised to include labels and generally cleaned up.
# March 2017, Revised the location of the Text for the zones
#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##  1:  Update_function_JAGS.r
###############################################################################################################


###############################################################################################################
# Arguments
# 1:  out.data:   The output from a model run.  Missing by default so must be added
# 2:  years:      The years of interest. Missing by default so must be added
# 3:  graphic:    What to do if making a figure.  Print to "screen" by default, optionally save as a "pdf".
# 4:  avg.line:   Add a line at to the figures. Default = NULL which does not add the line.  Only added if refs is set to NULL
#                 Generally we set this as median (no quotes) but any R function is possible, mean (no quotes) is a reasonable option
#                 The value of the current years biomass is excluded from this calculation.
# 5:  ht:         The height of the figure.  Default = 11
# 6:  wd:         The width of the figure.  Default = 6.5
# 7:  Rymax:      The maximum value of the y axis for the recruit biomass panel  Default = missing and is calculated from data
# 8:  Bymax:      The maximum value of the y axis for the fully recruited biomass panel  Default = missing and is calculated from data
# 9:  TAC:        The TAC to use.  Missing by default and the catch for the final year of interest is used.
#10:  pred:       The number of prediction years to include.  Default =1, the code only supports 1 or 2.
#11:  kt:         Is the biomass plot in kilo-tonnes.  T/F and default = T 
#12:  refs:       The reference points of interest.  Default = c("URP","LRP","zones") which plots a line for the Upper and lower reference 
#                 points and color codes the zones.  If you don't want reference points set this to NULL.  
#                 If URP2 or LRP2 is included this will draw secondary reference points at the value indicated by URP2 and LRP2.
#13:  index:      The years you want to use to specify LRP and URP   Only used if either URP or LRP is not specified.
#                 For example putting this as 4:15 will use years 4-15 to calculate the URP and LRP (if they are not  otherwise specified)
#14:  URP:        The upper stock reference point.Missing by default which will set it to 80% of median biomass over the years specified by index.
#15:  LRP:        The lower stock reference point.Missing by default which will set it to 30% of median biomass over the years specified by index.
#16:  URP2:       A secondary URP to specify.  This will be added as a line to the plot if "URP2" is included in "refs" above.  Default = NULL
#17:  LRP2:       A secondary LRP to specify.  This will be added as a line to the plot if "LRP2" is included in "refs" above.  Default = NULL 
#18:  out.data2:  A secondary set of data. Missing by default and not sure it's purpose but will plot instead of the recruits if specified.
#19:  lab:        A label to add to the figure name, only useful if making a pdf figure.
#20:  alpha:      The Credible intervals to use.  Default = c(0.05,0.2).  The first term is used for the CI lines on the figure and creates 
#                 95% confidence intervals.  The second term defines the whiskers for the box plot, 0.2 gives an "80%" boxplot.
#21:  path:       The path to save the figure if making a pdf.  Default = blank and will plot to the current R directory...
#22:  language    default is "en" for english, use "fr" if french
###############################################################################################################



biomass.plt <- function(out.data, years, graphic='screen',avg.line=NULL,ht=11,wd=8.5,Rymax,Bymax,TAC,pred=1,kt=T,
                        refs=c("URP","LRP","zones"),index,URP,LRP,URP2=NULL,LRP2=NULL,out.data2,lab='',alpha=c(0.05,0.2),path='',
                        language="en")
{
  # Set up the plot device 
  if(graphic=='pdf') pdf(paste(path,lab,"Biomass.pdf",sep=''), width = wd, height = ht)
  if(graphic=='png') png(paste(path,lab,"Biomass.png",sep=''), width = wd, height = ht,res=920,units="in")
  if(graphic=='screen')windows(wd,ht)
  
  # If only 1 alpha is defined add the same number to the second.	
  if(length(alpha)==1) alpha[2]<-alpha[1]
  #If TAC isn't specified set it as the catch from the data for the final year of data we are interested in..
  if(missing(TAC)==T) TAC <- out.data$data$C[DD.out[[bnk]]$data$year == max(years)]
  
  # Get the projected years TAC.
  TACI<-which(out.data$data$C.p==TAC)
  
  # Set up the plot window
  par(mfrow = c(2,1), mar = c(1, 4, 1, 1), omi = c(0.2, 0.1, 0, 0.2))
  
  # If Bymax isn't specified than set the maxium using the Biomass data for the Upper credible limit.
  if(missing(Bymax)==T | is.null(Bymax))
  {
    if(!is.null(TAC)) Bymax <- max(c(apply(out.data$sims.list$B, 2, quantile, 1-alpha[1]/2),quantile(out.data$sims.list$B.p[,TACI],1-alpha[2]/2)))
    if(is.null(TAC)) Bymax <- max(apply(out.data$sims.list$B, 2, quantile, 1-alpha[1]/2))
  } # if(missing(Bymax)==T )
  
  # Make the plot
  plot(years, out.data$median$B, type = 'b', pch = 16, ylim = c(0, Bymax), ylab = "", las = 1, mgp = c(0.5, 0.5, 0), xlab = "", 
       tcl = -0.3, asp = 'xy', xlim=c(min(years)-1,max(years)+max(pred)),yaxt='n' )
  # Add a y-axis on the right side
  axis(4, lab = F, tcl = -0.3)
  # If we are plotting in kilo-tonnes than divide biomass maximum by 1000.
  if(kt == T) axis(2, pretty(c(0, Bymax)),lab =  pretty(c(0, Bymax))/1000,las=1)
  else axis(2) # otherwise it's in tonnes.
  # If we don't specify a second set of data add the text to the y axis margin.
  if(missing(out.data2)==T)
  {
    # Either tonnes or kilo-tonnes
  	if(kt==T & language=="en") mtext("Fully-Recruited Biomass (kt)", 2, 3, cex=1.2)
    if(kt==F & language=="en") mtext("Fully-Recruited Biomass (t)", 2, 3, cex=1.2)
    if(kt==T & language=="fr") mtext("Biomasse pleinement recrut\u{E9}e (kt)", 2, 3, cex=1.2)
    if(kt==F & language=="fr") mtext("Biomasse pleinement recrut\u{E9}e (t)", 2, 3, cex=1.2)
  } # end if(missing(out.data2)==T)
  # Add the alpha credible intervals.
  lines(years, apply(out.data$sims.list$B, 2, quantile, alpha[1]/2), lty = 2)
  lines(years, apply(out.data$sims.list$B, 2, quantile, 1-alpha[1]/2), lty = 2)
  
  # If reference points are not specified and we want to add a median to the plot this is how we role.
  # Note the current year isn't included in that median.
  if(is.null(refs)==T && is.null(avg.line)==F)
  {
    abline(h=avg.line(out.data$median$B[-length(out.data$median$B)]),lty=2,col='blue',lwd=2)
  } # end if(is.null(refs)==T && is.null(avg.line)==F)
  # Print to the screen the Biomass, projected biomass (under the interim TAC) and the biomass 2 years out.  
  print(out.data$median$B[length(out.data$median$B)])
  print(out.data$median$B.p[TACI])
  print(out.data$median$B.p2)
  
  # Add in the labels for the zones
  # Updated the location for these so they don't get in the way of the most recent years, should automatically put them in a nice location.
  if(is.null(refs) == F & language=="en")
  {
    text(max(years)-5, LRP*0.1, "CRITICAL", col='firebrick1', cex=1.5)
    text(max(years)-5, LRP*1.2, "CAUTIOUS", col='goldenrod1',cex=1.5)
    text(max(years)-5, 0.9*Bymax, "HEALTHY", col='chartreuse2',cex=1.5)
  } # end if(is.null(refs) == F)
  if(is.null(refs) == F & language=="fr")
  {
    text(max(years)-5, LRP*0.1, "CRITIQUE", col='firebrick1', cex=1.5)
    text(max(years)-5, LRP*1.2, "PRUDENCE", col='goldenrod1',cex=1.5)
    text(max(years)-5, 0.9*Bymax, "SANT\u00C9", col='chartreuse2',cex=1.5)
  } # end if(is.null(refs) == F)
  
  # This gets the one year prediction based on the same interim TAC. By default the whiskers here are the 80% whiskers.
  if( 1%in% pred)
  {
    # Get the projection using the interim TAC
  	pB<-out.data$sims.list$B.p[,TACI]
  	# Subset the data to be limited to 80% of the data.  DK Note I'm still not convinced this is really the way we should do these.
  	pB.box<-pB[pB>quantile(pB,alpha[2]/2)&pB<quantile(pB,1-alpha[2]/2)]
  	# Throw down the boxplot onto our other figure
  	boxplot(pB.box,add=T,at=max(years)+1,axes=F,range=0,lty=1)
  	# Add the median point to the figure.
  	points(max(years)+1,median(pB.box),pch=16)
  } # end if(1 %in% pred)
  
  # This gets the two year prediction based on the same interim TAC.  Identical otherwise to what we just did above.
  if(2 %in% pred)
  {
  	pB2<-out.data$sims.list$B.p2[,TACI]
  	pB2.box<-pB2[pB2>quantile(pB,alpha[2]/2) & pB2<quantile(pB2,1-alpha[2]/2)]
  	boxplot(pB2.box,add=T,at=max(years)+2,axes=F,range=0,lty=1)
  	points(max(years)+2,median(pB2.box),pch=16)
  } # end if(2 %in% pred)
  
  # If the index isn't specified set it to be the length of the data.
  if(missing(index)==T)index<-1:length(out.data$median$B)
  # If the URP isn't specified set it at 80% of the median biomass over the time series chosen by index
  if(missing(URP)==T) URP1<-median(out.data$median$B[index])*0.8
  # If it was supplied rename it
  else URP1<-URP
  # If the URP isn't specified set it as 30% of median biomass over the time series chosen by index
  if(missing(LRP))LRP1<-median(out.data$median$B[index])*0.3
  # If it was supplied rename it
  else LRP1<-LRP
  
  # Add the line if it was specified in the refs obejct.
  if("URP" %in% refs) abline(h=URP1,lty=3,col='grey')
  if("LRP" %in% refs)abline(h=LRP1,lty=3,col='grey')
  if("URP2" %in% refs)abline(h=URP2,lty=3,col='grey')
  if("LRP2" %in% refs)abline(h=LRP2,lty=3,col='grey')
  # If zones was specified make 3 rectangles
  if("zones"%in%refs)
  {
    # The first for the healthy zone
  	rect(min(years)-5, -Bymax/5, max(years)+5, LRP1, border=NA, col=rgb(1,0,0,0.2) )
  	# The second for the cautious zone
    rect(min(years)-5, LRP1, max(years)+5, URP1, border=NA, col=rgb(1,1,0,0.2) )
  	# and finally one for the critical zone.
    rect(min(years)-5, URP1, max(years)+5, Bymax*1.2, border=NA, col=rgb(0,1,0,0.2) )
  } # if("zones"%in%refs)
  # Output the URP/LRP.
  print(list(URP=URP1,LRP=LRP1))
  
  # If out.data2 was actually specified than we want a second plot. I'm not sure when we would use this figure but is
  # is basically the same as the above figure using this second set of data so I won't put any detailed comment in here...
  if(missing(out.data2)==F)
  {
  	# Add the text to the first figure including one piece of text saying this is figure "a'
  	if(kt==T & language=="en") mtext("Fully-Recruited Biomass (kt)", 2, -1, cex=1.2,outer=T)
    if(kt==F & language=="en") mtext("Fully-Recruited Biomass (t)", 2, -1, cex=1.2,outer=T)
    if(kt==T & language=="fr") mtext("Biomasse pleinement recrut\u{E9}e (kt)", 2, -1, cex=1.2,outer=T)
    if(kt==F & language=="fr") mtext("Biomasse pleinement recrut\u{E9}e (t)", 2, -1, cex=1.2,outer=T)
  	text(min(years), Bymax*0.95, "a", cex=1.25)
  	
  	# Now we use the second set of data to do what we did above...
  	TACI<-which(out.data2$data$C.p==TAC)
  	if(missing(Bymax)==T)Bymax <- max(c(apply(out.data2$sims.list$B, 2, quantile, 1-alpha[1]/2),quantile(out.data2$sims.list$B.p,1-alpha[2]/2)))
  	# Make the plot
  	plot(years, out.data2$median$B, type = 'b', pch = 16, ylim = c(0, Bymax), ylab = "", las = 1, 
  	     mgp = c(0.5, 0.5, 0), xlab = "", tcl = -0.3, asp = 'xy', xlim=c(min(years)-1,max(years)+max(pred)),yaxt='n' )
  	axis(4, lab = F, tcl = -0.3)
  	if(kt==T)axis(2, pretty(c(0, Bymax)),lab =  pretty(c(0, Bymax))/1000,las=1)
  	else axis(2)
  	text(min(years), Bymax*0.95, "b", cex=1.25)
  	lines(years, apply(out.data2$sims.list$B, 2, quantile, alpha[1]/2), lty = 2)
  	lines(years, apply(out.data2$sims.list$B, 2, quantile, 1-alpha[1]/2), lty = 2)
  	# If reference points are not specified and we want to add a median to the plot this is how we role.
  	# Note the current year isn't included in that median.
  	if(is.null(refs)==T && is.null(avg.line)==F)
  	{
  	  abline(h=avg.line(out.data2$median$B[-length(out.data$median$B)]),lty=2,col='blue',lwd=2)
  	} # end if(is.null(refs)==T && is.null(avg.line)==F)
  	print(out.data2$median$B[length(out.data2$median$B)])
  	print(out.data2$median$B.p[TACI])
  	print(out.data2$median$B.p2)

  	
  	# Add the 1 year predicted biomass from data2
  	if(1%in%pred)
  	{
  		pB<-out.data2$sims.list$B.p[,TACI]
  		pB.box<-pB[pB>quantile(pB,alpha[2]/2)&pB<quantile(pB,1-alpha[2]/2)]
  		boxplot(pB.box,add=T,at=max(years)+1,axes=F,range=0,lty=1)
  		points(max(years)+1,median(pB.box),pch=16)
  	}# end if(1%in%pred)
  	
  	# Add the 2 year predicted biomass from data2
  	if(2%in%pred)
  	{
  		pB2<-out.data2$sims.list$B.p2
  		pB2.box<-pB2[pB2>quantile(pB,alpha[2]/2)&pB2<quantile(pB2,1-alpha[2]/2)]
  		#pY<-max(years)+1
  		boxplot(pB2.box,add=T,at=max(years)+2,axes=F,range=0,lty=1)
  		points(max(years)+2,median(pB2.box),pch=16)
  	} # end if(2%in%pred)
  	
  	# Get the dat2 LRP zones all set up as above.
  	if(missing(index)==T)index<-1:length(out.data$median$B)
  	if(missing(URP)==T)URP1<-median(out.data2$median$B[index])*0.8
  	else URP1<-URP
  	if(missing(LRP))LRP1<-median(out.data2$median$B[index])*0.3
  	else LRP1<-LRP
  	
  	if("URP"%in%refs)abline(h=URP1,lty=3,col='grey')
  	if("LRP"%in%refs)abline(h=LRP1,lty=3,col='grey')
  	if("URP2"%in%refs)abline(h=URP2,lty=3,col='grey')
  	if("LRP2"%in%refs)abline(h=LRP2,lty=3,col='grey')
  	if("zones"%in%refs)
  	 {
  		rect(min(years)-5, -Bymax/5, max(years)+5, LRP1, border=NA, col=rgb(1,0,0,0.2) )
  		rect(min(years)-5, LRP1, max(years)+5, URP1, border=NA, col=rgb(1,1,0,0.2) )
  		rect(min(years)-5, URP1, max(years)+5, Bymax*1.2, border=NA, col=rgb(0,1,0,0.2) )
  	} # end if("zones"%in%refs)

  	# Add in the labels for the zones
  	if(is.null(refs) == F  & language=="en")
  	{
  	  text(2011, 100, "CRITICAL", col='firebrick1', cex=1.5)
  	  text(2011, 10000, "CAUTIOUS", col='goldenrod1',cex=1.5)
  	  text(2011, 45000, "HEALTHY", col='chartreuse2',cex=1.5)
  	} # end if(is.null(refs) == F)
  	if(is.null(refs) == F & language=="fr")
  	{
  	  text(2011, 100, "CRITIQUE", col='firebrick1', cex=1.5)
  	  text(2011, 10000, "PRUDENCE", col='goldenrod1',cex=1.5)
  	  text(2011, 45000, "SANT\u00C9", col='chartreuse2',cex=1.5)
  	} # end if(is.null(refs) == F)
  	
  } # end if(missing(out.data2)==F)
  
  # If we were missing out.data2 than we make the recruit figure.
  if(missing(out.data2)==T)
  {
    # If the maximum for the Recruit time series y axis isn't specified than we find it using the alpha value.
  	if(missing(Rymax)==T) Rymax <- max(apply(out.data$sims.list$R, 2, quantile, 1-alpha[1]/2))
  	# Make the plot
  	plot(years, out.data$median$R, type = 'b', pch = 16, ylim = c(0, Rymax), ylab = "", las = 1, mgp = c(0.5, 0.5, 0), 
  	     xlab = "", tcl = -0.3, asp = 'xy', xlim=c(min(years)-1,max(years)+1),yaxt='n')
  	# Add the y-axis tick marks on the right
  	axis(4, lab = F, tcl = -0.3)
  	# if it is in kilo-tonnes than these are our y-axis tick marks and label
  	if(kt==T)
  	{
  	  axis(2, pretty(c(0, Rymax)),lab =  pretty(c(0, Rymax))/1000,las=1)
  	  if(language=="en") mtext("Recruit Biomass (kt)", 2, 3, cex=1.2)
  	  if(language=="fr") mtext("Biomasse des recrues (kt)", 2, 3, cex=1.2)
  	}
  	# Otherwise it is this.
  	if(kt==F)
  	{
  	  axis(2)
  	  if(language=="en") mtext("Recruit Biomass (t)", 2, 3, cex=1.2)
  	  if(language=="fr") mtext("Biomasse des recrues (t)", 2, 3, cex=1.2)
  	} #end if(kt==F)
  	
  	lines(years, apply(out.data$sims.list$R, 2, quantile, alpha[1]/2), lty = 2)
  	lines(years, apply(out.data$sims.list$R, 2, quantile, 1-alpha[1]/2), lty = 2)
  	
  	if(!is.null(avg.line))abline(h=avg.line(out.data$median$R[-length(out.data$median$R)]),lty=2,col='blue',lwd=2)
  }# end if(missing(out.data2)==T)
  # Print the URP and LRP
  print(list(URP=URP1,LRP=LRP1))
  # Close the plot device if making a pdf.
  if(graphic != "screen")dev.off()
}


