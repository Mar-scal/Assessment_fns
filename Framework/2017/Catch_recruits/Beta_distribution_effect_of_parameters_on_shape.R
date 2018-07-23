# This little script produces a huge pdf which what different a and b values does to the shape of the beta distribution
# if looking to modify the a/b values of a beta distribution this pdf is super handy to pick appropriate values.
# What this does is takes different probbailities and shows how much varialibity around this probability the
# combinations of a and b parameters results in.
#################### A Beta distribution moment ###################### A Beta distribution moment ###################################
# The mode of a beta distribution  x = (alpha-1)/(alpha+Beta-2) WHEN ALPHA AND BETA > 1
# so for a mode of  "x" and solving for "Beta we get  Beta = (alpha - x*alpha +2x -1)/x
direct <- "d:/r/2017/Framework/Catch_recruits/"
x <- c(0.001,signif(seq(0.01,0.99,by=0.01),digits=2),0.999) # Get some probability modes (e.g. natural mortality, ), you can make these whatever you'd like b/t 0 and 1
# Increasing the alpha, as you will see in a moment, will increase the informatativeness of the priors.
alpha <- c(seq(1,1.95,by =0.05),seq(2,2.9,by=0.1),seq(3,48,by=5),seq(50,500,by=25)) # alpha needs to be > 1
# The data frame to store the results.
dist <- data.frame(x = rep(NA,length(x)*length(alpha)),alpha = rep(NA,length(x)*length(alpha)),beta = rep(NA,length(x)*length(alpha)))
count = 0

for(j in 1:length(alpha))
{    
  for(i in 1:length(x))
  {
    count = count + 1
    dist$beta[count] = (alpha[j] - x[i] * alpha[j] +2*x[i] -1)/x[i]
    dist$x[count] <- x[i]
    dist$alpha[count] <- alpha[j]
  } # end for(j in 1:length(alpha))
} # end for(i in 1:length(x))


# We can plot these historgrams and determine what values we'd like to use
pdf(file=paste(direct,"beta_dist_historgrams.pdf",sep=""),onefile=T)
par(mfrow=c(3,3))
# There are probably quicker ways to make this plot as it takes a bit!
for(i in 1:(length(x)*length(alpha)))
{
  # This makes a histogram using 10000 pulls from the beta distribution using the alpha and beta values
  hist(rbeta(10000,dist$alpha[i],dist$beta[i]),main = paste("Alpha=",dist$alpha[i]," Beta=",
                                                           signif(dist$beta[i],digits=3), " & proportion=", dist$x[i],sep=""),
       xlab="",ylab="",cex.main=0.7)
} #end for(i in 1:length(x)*length(alpha))
dev.off()