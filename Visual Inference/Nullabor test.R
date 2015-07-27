# From nullabor git

install.packages("nullabor")
library(nullabor)
data(lakers)

threept <- subset(lal, type == "3pt" & !is.na(x) & !is.na(y))
threept <- threept[c(".id", "period", "time", "team", "etype", "player", "points", "result", "x", "y")]

threept <- transform(threept, 
                     x = x + runif(length(x), -0.5, 0.5),
                     y = y + runif(length(y), -0.5, 0.5))
threept <- transform(threept, 
                     r = sqrt((x - 25) ^ 2 + y ^ 2),
                     angle = atan2(y, x - 25))
threept <- subset(threept, r > 20 & r < 39)

#Some additions

threept$angle <- threept$angle*(180/pi)
lm <- lm(r~angle+I(angle^2), data=threept)
threept$resid <- resid(lm)

qplot(angle, resid, data = threept) %+% lineup(null_permute('angle'), threept) + facet_wrap(~ .sample)
# Angle vs. residual plots generated under null hypothesis that angle and residuals are independent
# and that residuals are standard/normally distributed

qplot(angle, r, data=threept) %+% lineup(null_lm(r~angle+I(angle^2)), threept) + facet_wrap(~.sample) 
# Simulation based on H_0 that data follows quadratic line