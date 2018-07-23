# THis is a simple example discussing what goes into a simple model.....
# Here's a sample of men and women from Nepal
library(tidyverse)
library(ggfortify)
direct <- "D:/r/"
nepal.m.height <- data.frame(height = rnorm(500,163,6), sex = "Male",country = "Nepal")
nepal.f.height <- data.frame(height =rnorm(500,147,5.5), sex = "Female",country = "Nepal")
bosnia.m.height <- data.frame(height = rnorm(500,183.9,7.11), sex = "Male",country = "Bosnia")
bosnia.f.height <- data.frame(height =rnorm(500,171.1,6.56), sex = "Female",country = "Bosnia")



dat <- rbind(nepal.m.height,nepal.f.height,bosnia.m.height,bosnia.f.height)
dat$weight[dat$sex == "Male"] <- 110 + 0.9 * (dat$height[dat$sex == "Male"] - 152)# + rnorm(length(dat$height[dat$sex == "Male"]),0,10)
# Let's make the Bosnia men fatter than the Nepal men
dat$weight[dat$sex == "Male" & dat$country == "Bosnia"] <- dat$weight[dat$sex == "Male" & dat$country == "Bosnia"] +  
                                                                                                rlnorm(length(dat$height[dat$sex == "Male"  & dat$country == "Bosnia"]),log(20),0.4)
dat$weight[dat$sex == "Male" & dat$country == "Nepal"] <- dat$weight[dat$sex == "Male" & dat$country == "Nepal"] - 
                                                                                                rlnorm(length(dat$height[dat$sex == "Male"  & dat$country == "Nepal"]),log(3),0.6)
dat$weight[dat$sex == "Female"] <- 100 + 0.9 * (dat$height[dat$sex == "Female"] -152)# + rnorm(length(dat$height[dat$sex == "Female"]),-3,3)
dat$weight[dat$sex == "Female" & dat$country == "Bosnia"] <- dat$weight[dat$sex == "Female" & dat$country == "Bosnia"]  +  
                                                                                                rlnorm(length(dat$height[dat$sex == "Female"  & dat$country == "Bosnia"]),log(4),0.6)
dat$weight[dat$sex == "Female" & dat$country == "Nepal"] <-  dat$weight[dat$sex == "Female" & dat$country == "Nepal"] + 
                                                                                                rlnorm(length(dat$height[dat$sex == "Female"  & dat$country == "Nepal"]),log(10),0.4)


windows(11,11)
ggplot(dat, aes(height,weight)) + geom_point()  +theme_bw(base_size = 20) + geom_smooth(method = 'lm')

mod.lm <- lm(weight~height*sex*country,dat)
dat$resid <- resid(mod.lm)

ggplot(dat,aes(height,resid)) + geom_point() +theme_bw(base_size = 20) + geom_hline(yintercept = 0)

windows(11,11)
ggplot(dat, aes(height,weight,group = sex,colour=sex)) + geom_point()  +theme_bw(base_size = 20) + geom_smooth(method = 'lm') + facet_wrap(~country)

# Or is an example using a time series of money in the bank more fitting...
set.seed(126)
years <- 1965:2018
NY <- length(years)
int <- rlnorm(NY,log(0.05),0.1)
fees <- rlnorm(NY,log(1000),0.1)
# Cut the withdrawls in half about half way through the proces
withdrawls <- c(rlnorm(10,log(0.4),0.1),rlnorm(NY-10,log(0.2),0.1))
deposits <- rlnorm(NY,log(5000),0.1)
Initial.funds <- 15000
rand.deviations <- c(rnorm(NY-10,0,500),rnorm(10,-500,100))

Balance.act <- Initial.funds
Balance.mod <- Initial.funds
for(i in 2:NY) Balance.mod[i] <- Balance.mod[i-1]*(1+int[i-1]) - Balance.mod[i-1]*withdrawls[i-1] - fees[i-1] + deposits[i]
for(i in 2:NY) Balance.act[i] <- Balance.act[i-1]*(1+int[i-1]) - Balance.act[i-1]*withdrawls[i-1] - fees[i-1] + deposits[i] + rand.deviations[i]# Just add some random noise

dat.mod <- data.frame(years = years, int = int, fees = fees, withdrawls = withdrawls, deposits = deposits, balance = Balance.mod/1000,type = "modelled")
dat.act <- data.frame(years = years, int = int, fees = fees, withdrawls = withdrawls, deposits = deposits, balance = Balance.act/1000,type = "actual")


lin.mod <- lm(balance~years,dat.act)
dat.act$resid <- resid(lin.mod) # So actual is using the "lm" residuals, "modelled" is using the residuals from the "good" model
dat.act$pred <- predict(lin.mod)
dat.mod$resid <- dat.act$balance - dat.mod$balance
dat.mod$pred <- dat.mod$balance # Just adding so the two data frames have the same data...
dat <- rbind(dat.mod,dat.act)
dat$type <- as.factor(dat$type)
# Time series
windows(14,11)
ggplot(dat.act)+ geom_line(aes(years,balance),linetype = "solid",colour="black",size=2)  +theme_bw(base_size = 24) + xlab("") + ylab("Balance (1000 $)") + ylim(10,30)# + geom_smooth(method = 'lm')
ggsave(paste0(direct,"2018/Presentations/Special_Industry_presentations/Financial_time_series_example.png"),width=14,height=11,units = "in")
# Time series with linear model
windows(14,11)
ggplot(dat.act)+ geom_line(aes(years,balance),linetype = "solid",colour="black",size=2)  +theme_bw(base_size = 24) + xlab("") + ylab("Balance (1000 $)") + ylim(10,30)+
                 geom_line(aes(years,pred),linetype = "solid",colour="grey",size=1.5) 
ggsave(paste0(direct,"2018/Presentations/Special_Industry_presentations/Financial_time_series_example_with_linear_model.png"),width=14,height=11,units = "in")

# Linear model residuals
windows(14,11)
ggplot(dat.act, aes(years,resid))+ geom_point(size=3) + theme_bw(base_size = 24) + xlab("") + ylab("Modelled deviations (1000 $)") + 
                                   geom_hline(yintercept = 0,linetype ='solid', size =2,col="red") + ylim(-5,5) # + geom_smooth(method = 'lm')
ggsave(paste0(direct,"2018/Presentations/Special_Industry_presentations/linear_model_Residual_patterns.png"),width=14,height=11,units = "in")

windows(14,11)
ggplot(dat.mod, aes(years,resid))+ geom_point(size=3) + theme_bw(base_size = 24) + xlab("") + ylab("Modelled deviations (1000 $)") + 
  geom_hline(yintercept = 0,linetype ='solid', size =2,col="red") + ylim(-5,5)# + geom_smooth(method = 'lm')
ggsave(paste0(direct,"2018/Presentations/Special_Industry_presentations/accounting_model_Residual_patterns.png"),width=14,height=11,units = "in")


# complex model comparision
windows(14,11)
ggplot(dat) + geom_line(aes(years,balance,colour = type,size=type)) + theme_bw(base_size = 24) + xlab("") + ylab("Balance (1000 $)") + ylim(10,30) +
               scale_colour_manual(values = c("black","grey")) + scale_size_manual(values = c(2,1))
ggsave(paste0(direct,"2018/Presentations/Special_Industry_presentations/Complex_model_vs_actual.png"),width=14,height=11,units = "in")


# complex model residuals
windows(14,11)
ggplot(transform(dat, type = c("Linear model","Accounting model")[as.numeric(type)]), aes(years,resid,colour = type))+ geom_point(size=3) + theme_bw(base_size = 24) + xlab("") + ylab("Modelled deviations (1000 $)") + facet_wrap(~type) +
                               geom_hline(yintercept = 0,linetype ='solid', size =2,col="red") + theme(legend.position="none") + scale_colour_manual(values = c("grey","blue"))
ggsave(paste0(direct,"2018/Presentations/Special_Industry_presentations/Comparing_Residual_patterns.png"),width=14,height=11,units = "in")

