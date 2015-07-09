# Example is a problem from "Bootstrap Methods and their Application" by Davison/Hinkley
# Dataset is of 27 cancer patients, 9 went into remission (indicated by remiss=1)
# LI is a measure of cancer activity 
# Testing whether or not LI values affect the probability of remission

remission <- read.table("/var/folders/t3/tfcry0l52h3f4fv68_p_z7hh0000gn/T//RtmpGNhq0t/dataec811042ef1", 
                        header=TRUE, quote="\"")

attach(remission) #access variables without giving dataset names
rem.glm <- glm(remiss~li, binomial, data=remission) #glm for remission varying on li level
x <- seq(0.4, 2.0, 0.02) # li values range from 0.4 to 1.9, generates sequence every 0.02
liVals <- cbind(rep(1,81), x) #generates column of x vals
eta <- liVals %*%coefficients(rem.glm) #fits li sequence to rem.glm model
rem.perm <- function(data, i)
{
  d <- data
  d$li <- d$li[i] #for each li value
  d.glm <- glm(remiss~li, binomial, data=d) #fit remission chances
  coefficients(d.glm) #gives coefficients for that li value
}
rem.boot <- boot(remission, rem.perm, R=199, sim="permutation") #199 bootstrap permutations of above loop
qqnorm(rem.boot$t[,2], ylab="Coefficient of LI", ylim=c(-3, 3)) #qqplot of li coefficients

# in rem.boot, t2 estimate is coefficient for rem.glm–it acts as our observed value?

#for alternative H_A b1 =/= 0
#p=
(sum(abs(rem.boot$t[,1]) <= rem.boot$t[,2]) +1)/(199+1)

