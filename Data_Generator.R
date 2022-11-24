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
y <- sin(3.14 * t) + 0.5 * cos(6.09 * t) + 0.1 *
  sin(10.11 * t + 1 / 6) + 0.1 * sin(15.3 * t + 1 / 3)
x_normal <- abs(y + 0.1*rnorm(length(y),1))   # Positive values + noise

plot(x_normal, type = 'l', col="#69b3a2", lwd=1.5)
points(x_normal + rnorm(n = length(x_normal),mean = 0,sd = 0.1), type = 'l', col=alpha("red",0.5), lwd=1)


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



fit_wavelet <- my_dwt(x_normal)
plot(min_max_scaling(x_normal), type = 'l', col="#69b3a2", lwd=1.5)
points(min_max_scaling(fit_wavelet$level4), type = 'l', col=alpha("red",0.5), lwd=2)


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

plot(x_normal + rnorm(n = length(x_normal),sd = 0.1), type = 'l')


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

mydata = 
  cbind.data.frame(in_control$features, 'class'=0)
mydata = rbind.data.frame(mydata, cbind.data.frame(out_control$features, 'class'=1))



# ploting signals ---------------------------------------------------------
df_plot <- cbind.data.frame('InControl'=in_control$signal$V1, 'OutControl'= out_control$signal$V1)
df_plot['Time'] <- 1:nrow(df_plot)
  
# ggplot(data = df_plot,aes(x = Time, y = InControl)) + geom_line(color='#69b3a2') + geom_point(shape=21, color="black", fill="#69b3a2", size=1) +ggtitle("Signal Power Consumption")+ theme(panel.background = element_rect(fill = 'white', colour = 'gray'))+ geom_line(data = df_plot,aes(x = Time, y = OutControl), color='red', alpha=0.5)+ geom_point(data = df_plot,aes(x = Time, y = OutControl), shape=21, color="black", fill="red", size=1, alpha=0.5)

ggplot(data = df_plot,aes(x = Time, y = InControl)) + geom_line(color='#69b3a2') + geom_line(data = df_plot,aes(x = Time, y = OutControl), color='red', alpha=0.5)+ggtitle("Signal Power Consumption")+ theme(panel.background = element_rect(fill = 'white', colour = 'gray'))


# ploting wavelet decomposing ---------------------------------------------
dec <- my_dwt(signal = in_control$signal$V1)
wavelet_dec <- dec$wavelet_dec
wavelet_dec <- melt(wavelet_dec[,seq(2,ncol(wavelet_dec)-1,3)],  id.vars = 'Time', variable.name = 'InControl')
colnames(wavelet_dec) <- c('Time','InControl', 'Values')

df_plot <- cbind.data.frame('InControl'=dec$signal, 'Time'= 1:length(dec$signal))

ggplot(wavelet_dec, aes(Time,Values)) + geom_line(aes(colour = InControl))+ ggtitle("Signal Power Consumption")+ theme(panel.background = element_rect(fill = 'white', colour = 'gray'))+geom_line(data = df_plot,aes(x = Time, y = InControl), color='black', alpha=0.5) 


# in and out decompose ----------------------------------------------------


# ploting wavelet decomposing ---------------------------------------------
dec_in <- my_dwt(signal = in_control$signal$V1)
dec_out <- my_dwt(signal = out_control$signal$V1)

df_plot <- cbind.data.frame('InControl'=dec_in$wavelet_dec[,5], 'OutControl'= dec_out$wavelet_dec[,5])
df_plot['Time'] <- 1:nrow(df_plot)

ggplot() + geom_line(data = df_plot,aes(x = Time, y = InControl, color='InControl'), size=1.3) + geom_line(data = df_plot,aes(x = Time, y = OutControl, color='OutControl'), alpha=0.5, size=1.1)+ggtitle("Signal Power Consumption")+ theme(panel.background = element_rect(fill = 'white', colour = 'gray')) + ylab("") + scale_colour_manual("", values = c("InControl"="#69b3a2", "OutControl"="red"))

