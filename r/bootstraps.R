#----------------------------------------------------------
# Implementing different bootstrapping procedures for
# elementary statistics for use with a Shiny app.
#
# June 2015
# Adam Loy
#
#----------------------------------------------------------

#----------------------------------------------------------
# Option 1: Using the mosaic package
#----------------------------------------------------------

# one-sample bootstrap
library(mosaic)
library(Lock5Data)
data(SleepStudy)

## One quantitative variable
original_data <- SleepStudy$AverageSleep # specify in panel

# set no. of bootstrap samples in panel
B <- 1000

# set function in the panel
trials <- do(B) * mean(sample(original_data, replace = TRUE))
trials <- do(B) * median(resample(original_data, replace = TRUE))
trials <- do(B) * sd(resample(original_data, replace = TRUE))

library(dplyr)
library(ggvis)
w <- .025 # set in panel

# without kernel density
trials %>% 
  ggvis(~result) %>% 
  layer_histograms(binwidth = w) %>% 
  add_axis("x", title = "means")

# with kernel density
trials %>% 
  ggvis(~result) %>% 
  layer_densities() %>%
  add_axis("x", title = "means")

# Summary statistics (example for mean)
observed <- mean(original_data)
resampled <- mean(trials)
bias <- resampled - observed
SE <- sd(trials)

bootstrap_stats <- c(Observed = observed, SE = SE, Mean = resampled, Bias = bias)

# percentile CI
level <- 0.95
alpha <- 1 - level
quantile(trials, probs = c(alpha/2, 1-alpha/2)) # two-tailed
quantile(trials, probs = c(alpha)) # lower bound
quantile(trials, probs = c(1-alpha)) # upper bound

# normal-based CI

level <- 0.95
alpha <- 1 - level
# two-tailed CI, make adjustments for one-tailed
observed - qnorm(1 - alpha/2) * SE
observed + qnorm(1 - alpha/2) * SE

## Two quantitative variables

tv <- read.csv("../data/TV.csv")

dat <- tv
grp <- "Cable"
qvar <- "Time"

colnames(dat)[colnames(dat) == grp()] <- "group"
colnames(dat)[colnames(dat) == qvar] <- "variable"

grouped <- group_by(tv, "Cable")

B <- 1000
trials <- do(B) * sample(grouped, replace = TRUE)

grouped_trials <- group_by(trials, .index, Cable)
group_means <- summarise(grouped_trials, mean = mean(Time))
diffs <- summarise(group_means, mean.diff = diff(mean))

mean(diffs$mean.diff)
# without kernel density
w <- .2
diffs %>% 
  ggvis(~mean.diff) %>% 
  layer_histograms(width = w) %>% 
  add_axis("x", title = "means")

# with kernal density
diffs %>% 
  ggvis(~mean.diff) %>% 
  layer_densities() %>%
  add_axis("x", title = "difference in means")