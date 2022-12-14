# Loading Packages
library(DALEX)
library(wavelets)
library(scales)
library(moments)

library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(reshape2)


t <- 2 * pi * seq(0, 1, length = 1024)
y <- sin(3.14 * t) + 0.5 * cos(6.09 * t) + 0.1 * sin(10.11 * t + 1 / 6) + 0.1 * sin(15.3 * t + 1 / 3)
x_normal <- abs(y + 0.06*rnorm(length(y),1))   # Positive values + noise


min_max_scaling <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

#feature transformation
my_dwt <- function(signal, same_size=T, normalize=T) {
  signal <- signal[complete.cases(signal)]
  
  if(log2(length(signal))%%1!=0){
    signal <- signal[1:(2^floor(log2(length(signal))))]
    print(paste('The only first', (2^floor(log2(length(signal)))), 'are used!'))
  }
  
  w <- dwt(signal, filter="haar") #decompose level 10
  result <- matrix(data = NA, nrow = length(signal), ncol = length(w@V))
  for (i in 1:length(w@V)) {
    if (same_size==T) {
      # get the same size of orignal signal
      result[,i] <- rep(as.numeric(w@V[[i]]),each=length(signal)/length(w@V[[i]]))
    }else{
      result[,i] <- append(as.numeric(w@V[[i]]), rep(NA, length(signal) - length(as.numeric(w@V[[i]]))))
    }
    
  }
  result <- as.data.frame(result)
  colnames(result) <- paste0('level',1:length(w@V))

  if (normalize==T) {
    result <- sapply(result, min_max_scaling)
    result[,length(w@V)] <- mean(signal)
    signal <- min_max_scaling(signal)
  }
  
  return(list('wavelet_dec'=result, 'signal'=signal))
}


signal_features <- function(signal) {
  result <- c(mean(signal), 
              sd(signal),
              skewness(signal),
              kurtosis(signal))
  names(result) <- c('mean', 'sd', 'skewness', 'kurtosis')
  return(result)
}

fwt <- function(signal, levels=c(5,6)) {
  w <- dwt(signal, filter="haar") #decompose level 10
  result <- matrix(data = NA, nrow = 1, ncol = 4 * (length(levels)+1)) # +1 adding the value of main signal without decomposition
  tmp_result <- signal_features(signal)
  tmp_names <- c('mean', 'sd', 'skewness', 'kurtosis')
  for (i in levels) {
    tmp_result <- append(tmp_result, signal_features(as.numeric(w@V[[i]])))
    tmp_names <- append(tmp_names, paste0(c('mean', 'sd', 'skewness', 'kurtosis'),'_',i))
  }
  result[1,] <- tmp_result
  result <- as.data.frame(result)
  colnames(result) <- tmp_names
  return(result)
}


# simulating signals
signal_simulate <- function(n_simulation = 100, limit_mean = c(-0.1,0.1), limit_sd=c(0.01, 0.1), dec_leveles=c(4,5,6,7)){
  signal <- as.data.frame(matrix(data = NA, nrow = length(x_normal), ncol = n_simulation))
  
  for (i in 1:n_simulation) {
    signal[,i] <- x_normal+ rnorm(n = length(x_normal),mean = runif(n = 1,min = limit_mean[1], max = limit_mean[2]), sd = runif(n = 1,min = limit_sd[1], max = limit_sd[2]))
  }
  
  signal_features <- fwt(signal[,1], levels = dec_leveles)
  for (i in 2:n_simulation) {
    signal_features <- rbind(signal_features, fwt(signal[,i], levels = dec_leveles))
  }
  return(list('signal'=signal, 'features'=signal_features))
}


mean_in_control <- c(-0.05,0.07)
mean_out_control <- c(0.05,0.1)

sd_in_control <- c(0.05, 0.1)
sd_out_control <- c(0.1,0.15)

# simulating in control data
in_control <- signal_simulate(limit_mean = mean_in_control, limit_sd=sd_in_control)
#simulating anomalies (out of control)
out_control <- signal_simulate(limit_mean = mean_out_control,limit_sd = sd_out_control)
# View(in_control$features)
mydata = 
  cbind.data.frame(in_control$features, 'class'=0)
mydata = rbind.data.frame(mydata, cbind.data.frame(out_control$features, 'class'=1))


