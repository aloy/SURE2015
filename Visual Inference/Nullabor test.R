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

library(Lock5Data)
data(RestaurantTips)
filteredData() <- function(x){data.frame(RestaurantTips)}
colnames(filteredData()) #change 2 to x and y

#qqPlot doesn't work with the lineup function, so calculate Q-Q plot stuff manually
# Simulate from target distribution, N(0, sd) using rnorm function

data(RestaurantTips)
data <- RestaurantTips
names(data)[1:2] <- c("y","x")
filteredData <- function(x){data}

xnorm <- rnorm(filteredData()$x, mean=mean(filteredData()$x), sd=sd(filteredData()$x))
qqLineup <- function(x){
  x <- rnorm(filteredData()$x, mean=mean(filteredData()$x), sd=sd(filteredData()$x))
  data.frame(qqnorm(x, plot.it=FALSE))
}
w <- nrow(filteredData())
samples <- data.frame(plyr::rdply(n, qqLineup(xnorm)))
samples$.n <- as.numeric(samples$.n)
qq.df <- data.frame(.n=rep(as.numeric(r), w), x=sort(qqLineup(xnorm)$x), y=sort(qqLineup(xnorm)$y))
startrow <- function(x){w*(x-1)} #replace one of the samples with the actual data
nextrow <- function(x){(w*(x))+1}
endrow <- function(x){w*n}
new.df <- data.frame(rbind(samples[1:startrow(r),], qq.df, samples[nextrow(r):endrow(n),]))
new.df <- new.df[complete.cases(new.df),]
ggplot(new.df, aes(x=x, y=y)) + geom_point() + facet_wrap(~.n)

## More fun with spine plots

Backpack <- read.csv("~/Downloads/Stat2Data2012CSV/Backpack.csv") # on my (Alex's) desktop
data <- Backpack
names(data)[3:4] <- c("y", "x")
filteredData <- function(x){data}
w <- nrow(filteredData())
spineplot(factor(x)~y, data=filteredData())
spineLineup <- function(x){data.frame(x=sample(filteredData()$x, size=w, replace=FALSE), y=filteredData()$y)}
samples <- plyr::rdply(n - 1,spineLineup(filteredData()))
r <- sample(n, 1)
origSpine <- data.frame(.n=rep(as.numeric(r), w),x=filteredData()$x, y=filteredData()$y)
View(origSpine)
startrow <- function(x){w*(x-1)} 
nextrow <- function(x){(w*(x))+1}
endrow <- function(x){(w*n)}
if(r!=1){
  new.df <- data.frame(rbind(samples[1:startrow(r),], origSpine, samples[nextrow(r):endrow(n),]))

}else{
  new.df <- data.frame(rbind(origSpine, samples[nextrow(r):endrow(n),]))
}
new.df <- new.df[complete.cases(new.df),]
