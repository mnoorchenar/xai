# Loading Packages
library(DALEX)
library(wavelets)
library(scales)
library(moments)

library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(reshape2)
library(trendsegmentR)

t <- 2 * pi * seq(0, 1, length = 1024)
y <- sin(3.14 * t) + 0.5 * cos(6.09 * t) + 0.1 * sin(10.11 * t + 1 / 6) + 0.1 * sin(15.3 * t + 1 / 3)
x_normal <- abs(y)
# x_normal <- abs(y + 0.05*rnorm(length(y),1))   # Positive values + noise

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
  
  return(list('Wavelet_Coef'=result, 'Signal'=signal))
}


up_down <- trendsegmentR::trendsegment(in_control$signal$V1)
up_down$no.of.cpt

plot(in_control$signal$V1, type = 'l')
points(up_down$est, type = 'l', col='red')

up_down$cpt







 # simulating signals
signal_simulate <- function(n_simulation = 100, dec_leveles=c(1:9), anomaly=F, coef_sd_anomaly=3){
  signal <- as.data.frame(matrix(data = NA, nrow = length(x_normal), ncol = n_simulation))
  
  for (i in 1:n_simulation) {
    signal[,i] <- abs(x_normal + 0.05*rnorm(length(x_normal),1))
    
    if (anomaly==T) {
      step <- round(0.01 * length(x_normal))
      change_point <- sample(c(1:(length(x_normal)-step-1)),size = 1)
      
      signal[change_point:(step+change_point),i] <- signal[change_point:(step+change_point),i] + mean(signal[change_point:(step+change_point),i]) + sample(x = c(-coef_sd_anomaly,coef_sd_anomaly), size = 1) * sd(signal[change_point:(step+change_point),i])
      
      # mean ± coef_sd_anomaly * SD
    }
  }
  
signal_features <- my_dwt(signal[,1])
  for (i in 2:n_simulation) {
    signal_features <- rbind(signal_features, fwt(signal[,i], levels = dec_leveles))
  }
  return(list('signal'=signal, 'features'=signal_features))
}

# simulating in control data
in_control <- signal_simulate()
#simulating anomalies (out of control)
out_control <- signal_simulate(anomaly = T, coef_sd_anomaly = 1)

mydata = 
  cbind.data.frame(in_control$features, 'class'=0)
mydata = rbind.data.frame(mydata, cbind.data.frame(out_control$features, 'class'=1))







# ploting signals ---------------------------------------------------------
df_plot <- cbind.data.frame('InControl'=in_control$signal$V1, 'OutControl'= out_control$signal$V1)
df_plot['Time'] <- 1:nrow(df_plot)

ggplot(data = df_plot,aes(x = Time, y = InControl)) + geom_line(color='#69b3a2') + geom_line(data = df_plot,aes(x = Time, y = OutControl), color='red', alpha=0.5)+ggtitle("Signal Power Consumption")+ theme(panel.background = element_rect(fill = 'white', colour = 'gray'))


