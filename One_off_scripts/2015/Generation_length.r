# Calculation of Generation Length for our friends the Scallop.  From the IUCN documentation there are really 3 ways to approach this...




#  The first is a rigorous method using the old equation
# G = sum(x*lx*mx)/sum(lx*mx), x being age, lx being survivorship, and mx being either fecundity or something proportional to fecundity (weight?) 
# We may be able to fudge survivorship from our models, and weight shouldn't be too hard to get at between our shell height/MW relationship and our 
# VonB relationship.  In fact we probably could just use the SH^3 for this

load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_all_results.R",sep=""))
# So as a rough pass, I could assume this...
#1	30-40
#2	40-50
#3	50 - 65
#4	65 - 85
#5	85 - 100
#6	100 - 110
#7	110-115
#8	115-120
#9	120-125
#10	125-130
#11	130-140


abund <- survey.obj[[2]]$n.yst
colnames(abund) <- paste(seq(5,200,by=5),sep="")
zero.year <- 1:length(seq(5,20,by=5))
one.year <- (max(zero.year) + 1):(length(seq(25,40,by=5)) + max(zero.year))
two.year <- (max(one.year) + 1):(length(seq(45,50,by=5)) + one.year)


# The second method is more straightforward model, best used if annual mortality after the age of first reproduction iswell know and 
# mortality and fecundity don't change with age after age of first reproduction.  Mortality we might be able to argue, but certainly 
# fecundity is in theory way higher (unless of course we mean fertility, which is the number of offspring produced by one individual in year x)
# rather than the number of eggs produced that year.  I don't see why these wouldn't be at least linearly related for scallop, so probably the
# weight = fecudity=fertility arguement would hold.  That would mean this is not a great way to go about this!
#G = 1/z + A(m)  (where z = adult mortality, and A(m) = age at first reproduction)

z = seq(0.1,0.35,by=0.025)
Am <- 2
# This gives a nice result but I'm not sure we should use it due to the fecundity problem.
G <- 1/z + 2


# The third method is similar to method #2.  
# G = A(m) + [q+ A(r)]  Where q is not really definded more than to say what is said below

#where q is usually <0.5, depending on survivorship and the relative fecundity of young vs. old individuals
#in the population. For age of first reproduction, see (2) above. This approximation is useful when ages of first and last reproduction are 
#the only available data, but finding the correct value of z may be tricky. In general, for a given length of reproductive period, z is lower 
#for higher mortality during reproductive years and it is higher for relative fecundity skewed towards older age classes.

q <- seq(0.05,0.5,by=0.01)
Am <- 2
Ar <- 15

G <- Am + (q*Ar)

