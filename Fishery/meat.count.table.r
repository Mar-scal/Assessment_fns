# This function is used to extract the monthly meat counts from the semi-monthly Reports (word docs).  
#Longer term I think we should integrate this function into the general port sampling analysis/updates that get sent out.
# Created Oct 2018 by FK, revised by DK

###############################################################################################################

# Arguments
#1:  filenames  The year of survey data you want to pull. Default = last year (current year -1)
#2:  path       The date of the logs you are pulling (sometimes the most recent aren't up to date), defaults to current date
#               make sure this is set as a "date" object so stick an "as.Date" in front of the data you specify.

meat.count.table <- function(filenames, path) 
{
  # Load in the necessary packages
  require(textreadr) || stop("Package textreadr cannot be found")
  require(reshape2) || stop("Package reshape2 cannot be found")
  require(plyr) || stop("Package plyr cannot be found")
  
  #Initiailze some objects.
  txtfile <- NULL
  txtfiles <- NULL
  bank <- NULL
  # Extract the data from each word document in the list of filenames.  First we need to exclude anything that isn't a doc or txt
  fnames <- filenames[sort(c(grep(filenames, pattern=".doc"),grep(filenames, pattern=".txt")))]
  # Exclude anything that doesn't have an underscore because sometimes processed files end up in this folder location
  fnames <- fnames[which(grepl(fnames, pattern='_', fixed=T)==TRUE)]
  # If there happens to be any temporary files we need to toss them out the window too
  if(length(grep("~",fnames))>0) fnames <- fnames[c(-grep("~",fnames))]
  
  num.files <- length(fnames)
  for(i in 1:num.files) 
  {
    # How we extract the data depeds on the type of file we have, annoyingly it's a mix of txt and doc, but amazingly 
    # we can actually get the both to work pretty easily...
    docs <- grepl(fnames[i], pattern=".doc")
    txts <- grepl(fnames[i], pattern=".txt")
    
    # Make sure any character strings are not being converted to factors.
    if(docs == T) txtfile[[i]] <- read.table(text=read_doc(paste0(path, fnames[i])), fill=T,stringsAsFactors = F)
    if(txts == T) txtfile[[i]] <- read.table(paste0(path, fnames[i]), fill=T,stringsAsFactors = F)
    
    # Now we need to organize and extract the data 
    bank <- paste(txtfile[[i]]$V2[1], txtfile[[i]]$V3[1], sep="") 
    month <- unique(month(lubridate::ymd(c(txtfile[[i]]$V5[txtfile[[i]]$V1=="Period"],
                                    txtfile[[i]]$V7[txtfile[[i]]$V1=="Period"]))))
    if(length(month)>1) month <- month[!is.na(month)]
    year <- unique(year(lubridate::ymd(c(txtfile[[i]]$V5[txtfile[[i]]$V1=="Period"],
                                           txtfile[[i]]$V7[txtfile[[i]]$V1=="Period"]))))
    if(length(year)>1) year <- year[!is.na(year)]
    # This pulls out the meat counts, start, end, and filename they were extracted from.  Make sure the data are not turned into factors...
    txtfile[[i]] <- data.frame(mc=txtfile[[i]]$V3[grepl(x=txtfile[[i]]$V1, pattern="Trip") & 
                                                    !is.na(as.numeric(txtfile[[i]]$V2))],
                               start = txtfile[[i]]$V5[txtfile[[i]]$V1=="Period"],
                               end = txtfile[[i]]$V7[txtfile[[i]]$V1=="Period"],
                               filename = fnames[i],stringsAsFactors =F,
                               month = month,
                               year = year)
    
    # Add the bank the the type of vessel.
    txtfile[[i]]$bank <- bank
    if(grepl(fnames[i], pattern="Fzn")) txtfile[[i]]$fleet <- "FT"
    if(grepl(fnames[i], pattern="Fsh")) txtfile[[i]]$fleet <- "WF"
    print(dim(txtfile[[i]])) # Print the progress
  } # end for(i in 1:num.files) 
  # Unwrap the list into a dataframe
  txtfiles <- do.call(rbind, txtfile)
  # Make sure the meat counts are numbers and the bank names are in the same case
  txtfiles$mc <- as.numeric(txtfiles$mc)
  txtfiles$bank <- tolower(txtfiles$bank)
  txtfiles$unID <- 1:nrow(txtfiles)
  # Now make a summary table with min/max and mean by banks and fleet, clearly the work of FK and her ddply skills on this one!
  mctable <- arrange(join(ddply(.data=txtfiles, .(fleet, bank),
                                summarize,
                                min=min(mc),
                                max=max(mc),
                                mean=mean(mc),
                                trips=length(unique(unID))), 
                          ddply(.data=txtfiles, .(bank),
                                summarize,
                                min=min(mc),
                                max=max(mc),
                                mean=mean(mc),
                                trips=length(unique(unID)),
                                fleet="all"),
                          type="full"),
                     bank, fleet)
  
  # Return the pieces of interest
  return(list(meatcounts=txtfiles,
              summarytable=mctable))
} # end function