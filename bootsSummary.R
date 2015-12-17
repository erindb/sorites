## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
bootsSummary <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                         conf.interval=.95, .drop=TRUE, n_boots_samps=1000) {
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=na.rm) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop, .fun = function(xx, col) {
    quantiles = quantile( #doesn't play nice with na.rm
      na.rm=na.rm,
      replicate(n_boots_samps, mean(sample(xx[[col]], replace = TRUE))),
      c(0.025, 0.975)
    )
    c(N    = length2(xx[[col]], na.rm=na.rm),
      mean = mean   (xx[[col]], na.rm=na.rm),
      bootsci_high = quantiles[["97.5%"]],
      bootsci_low = quantiles[["2.5%"]]
    )
  }, measurevar)
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  return(datac)
}