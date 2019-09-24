df <- NULL

# year data
df$years <- 1976:2018

# TAC data
df$TAC <- round(rnorm(length(1976:2018), mean = 10, sd=5), 0)^2

# landings(?) data
df$landings <- abs(df$TAC - 10)

# switch from a list to a dataframe
df <- as.data.frame(df)

# melt the dataframe so that it's in long format (TAC and landings in one column, but labelled by type in another column)
df <- reshape2::melt(df, id.vars = "years")

# here's the line to overplot on the data. It gets lined up with the data in a new column of the dataframe.
df$otherline <- c(rep(NA, 20), round(rnorm(n=23, mean=500, sd=100), 0)) # length is 43, but it gets repeated twice since we're in long format

require(ggplot2)

ggplot() + geom_bar(data=df, aes(years, value, fill=variable), colour="black", stat="identity") + # plots the bars
  theme_bw() + theme(panel.grid=element_blank()) + # white background, no gridlines
  geom_line(data=df, aes(years, otherline), lwd=1) + # adds the overlay line
  scale_fill_manual(values=c("white", "grey"), guide=F) + # sets the colours of the bars, and hides the legend. Replace this line with the # lines at the end if you want to keep the legend...  
  ylab(NULL) + # no y label please, or maybe you want ylab("Tonnes") or something?
  scale_y_continuous(expand = expand_scale(mult = c(0.01,0.1))) + # 
  scale_x_continuous(breaks=seq(1976,2018,4)) #+
  #scale_fill_manual(values=c("white", "grey"), labels=c("A", "B"), name="Type") +
  #theme(legend.position=c(0.9, 0.2)) # play with the location if you want it inside the plotting panel
                     