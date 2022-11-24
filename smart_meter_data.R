library(ggplot2)
library(hrbrthemes)
library(gridExtra)
library(wavelets)

df <- read.csv('data/difference_based_anomaly_prediction_user_1017024.csv')
View(df)

result <- my_dwt(signal = as.numeric(df$Incremental.Reading.Sum))

fit <- result$wavelet_dec[,3]
plot(result$signal, col="#69b3a2", lwd=3, type = 'ol')
points(fit, col=alpha("red",0.9), lwd=1.5, type='ol')
