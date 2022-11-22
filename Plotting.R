library(ggplot2)
library(hrbrthemes)
library(gridExtra)

df <- read.csv('data/difference_based_anomaly_prediction_user_1050725.csv')
head(df)
df['index'] <- 1:nrow(df)
df <- df[complete.cases(df),]
df$Is.Anomaly[df$Is.Anomaly=='True'] <- 1
df$Is.Anomaly[df$Is.Anomaly=='False'] <- 0

p1 <- ggplot(df, aes(x=index, y=Register.reading)) +  geom_line( color="#69b3a2", size=0.7, alpha=0.9) + theme_bw() + theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())  

p2 <- ggplot(df, aes(x=index, y=Register.Gap)) +  geom_line( color="#69b3a2", size=0.7, alpha=0.9) + theme_bw() + theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())


p3 <- ggplot(df, aes(x=index, y=Incremental.Reading.Sum)) +  geom_line(color="#69b3a2", size=0.7, alpha=0.9) + theme_bw() + theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())  

p4 <- ggplot(df, aes(x=index, y=Difference)) +  geom_line( color="#69b3a2", size=0.7, alpha=0.9) + theme_bw() + theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())  

grid.arrange(p1, p2, p3, p4, ncol=2)
