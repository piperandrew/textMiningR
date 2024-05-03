#### Simpler version #####

#Calculate CI on measurements for multiple groups
#takes as input two columns of a data frame: the measure and the groups

calculate_metrics_by_group <- function(data_column, group_column) {
  # Combine the data into a data frame
  data_df <- data.frame(value = data_column, group = factor(group_column))
  
  # Calculate the metrics by group
  results <- do.call(rbind, lapply(split(data_df$value, data_df$group), function(group_data) {
    n <- length(group_data)
    mean_val <- mean(group_data, na.rm = T)
    sd_val <- sd(group_data, na.rm = T)
    se_val <- sd_val / sqrt(n)
    ci_lower_val <- mean_val - qt(0.975, df=n-1) * se_val
    ci_upper_val <- mean_val + qt(0.975, df=n-1) * se_val
    
    return(data.frame(Mean = mean_val, SD = sd_val, SE = se_val, CI_Lower = ci_lower_val, CI_Upper = ci_upper_val))
  }))
  
  # Add the group names to the results
  rownames(results) <- levels(factor(group_column))
  
  return(results)
}

#For a single group
calculate_metrics <- function(data_column) {
  n <- length(data_column)
  mean <- mean(data_column)
  sd <- sd(data_column)
  se <- sd / sqrt(n)
  ci_lower <- mean - qt(0.975, df=n-1) * se
  ci_upper <- mean + qt(0.975, df=n-1) * se
  
  return(c(mean=mean, sd=sd, se=se, ci_lower=ci_lower, ci_upper=ci_upper))
}

## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summarized
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}