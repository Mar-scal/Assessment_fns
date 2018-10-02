# this looks at the word documents containing port sampling reports and creates the meat count table for the ppt!

meat.count.table <- function(filenames, year) {
  require(textreadr)
  require(reshape2)
  txtfile <- NULL
  txtfiles <- NULL
  bank <- NULL
  for(i in 1:length(filenames)) {
    if(grepl(filenames[i], pattern=".doc")) {
      txtfile[[i]] <- read.table(text=read_doc(paste0(direct, "Data/Port_Sampling/", year, "/", filenames[i])), fill=T)
    }
    if(grepl(filenames[i], pattern=".txt")) {
      txtfile[[i]] <- read.table(paste0(direct, "Data/Port_Sampling/", year, "/", filenames[i]), fill=T)
    }
    bank <- paste(txtfile[[i]]$V2[1], txtfile[[i]]$V3[1], sep="") 
    txtfile[[i]] <- data.frame(mc=txtfile[[i]]$V3[grepl(x=txtfile[[i]]$V1, pattern="Trip") & !is.na(as.numeric(txtfile[[i]]$V2))],
                               sail = txtfile[[i]]$V5[txtfile[[i]]$V1=="Period"],
                               land = txtfile[[i]]$V7[txtfile[[i]]$V1=="Period"],
                               filename = filenames[i])
    txtfile[[i]]$bank <- bank
    if(grepl(filenames[i], pattern="Fzn")) txtfile[[i]]$fleet <- "FT"
    if(grepl(filenames[i], pattern="Fsh")) txtfile[[i]]$fleet <- "WF"
    print(dim(txtfile[[i]]))
  }
  
  txtfiles <- do.call(rbind, txtfile)
  
  txtfiles$mc <- as.numeric(txtfiles$mc)
  
  txtfiles$bank <- tolower(txtfiles$bank)
  
  require(plyr)
  
  mctable <- arrange(join(ddply(.data=txtfiles, .(fleet, bank),
                                summarize,
                                min=min(mc),
                                max=max(mc),
                                mean=mean(mc)), 
                          ddply(.data=txtfiles, .(bank),
                                summarize,
                                min=min(mc),
                                max=max(mc),
                                mean=mean(mc),
                                fleet="all"),
                          type="full"),
                     bank, fleet)
  
  return(list(meatcounts=txtfiles,
              summarytable=mctable))
}