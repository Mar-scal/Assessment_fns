#################### A Beta distribution moment ###################### A Beta distribution moment ###################################
# The mode of a beta distribution  x = (alpha-1)/(alpha+Beta-2) WHEN ALPHA AND BETA > 1
# so for a mode of  "x" and solving for "Beta we get  Beta = (alpha - x*alpha +2x -1)/x

x <- signif(seq(0.01,0.9,by=0.01),digits=2) # Get some natural mortality modes, you can make these whatever you'd like...
# Increasing the alpha, as you will see in a moment, will increase the informatativeness of the priors.
alpha <- c(seq(1,3,by=0.2),seq(3,50,by=5)) # alpha needs to be > 1
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
pdf(file=paste(direct,"results/beta_dist_historgrams.pdf",sep=""),onefile=T)
par(mfrow=c(3,3))
for(i in 1:(length(x)*length(alpha)))
{
  hist(rbeta(1000,dist$alpha[i],dist$beta[i]),main = paste("Alpha=",dist$alpha[i]," Beta=",
                                                           signif(dist$beta[i],digits=3), " & mort=", dist$x[i],sep=""),
       xlab="",ylab="",cex.main=0.7)
} #end for(i in 1:length(x)*length(alpha))
dev.off()