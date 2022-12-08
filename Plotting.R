# ploting signals ---------------------------------------------------------
df_plot <- cbind.data.frame('InControl'=in_control$signal$V1, 'OutControl'= out_control$signal$V1)
df_plot['Time'] <- 1:nrow(df_plot)

# ggplot(data = df_plot,aes(x = Time, y = InControl)) + geom_line(color='#69b3a2') + geom_point(shape=21, color="black", fill="#69b3a2", size=1) +ggtitle("Signal Power Consumption")+ theme(panel.background = element_rect(fill = 'white', colour = 'gray'))+ geom_line(data = df_plot,aes(x = Time, y = OutControl), color='red', alpha=0.5)+ geom_point(data = df_plot,aes(x = Time, y = OutControl), shape=21, color="black", fill="red", size=1, alpha=0.5)
pdf('global.pdf')
ggplot(data = df_plot,aes(x = Time, y = InControl)) + geom_line(color='#69b3a2') + geom_line(data = df_plot,aes(x = Time, y = OutControl), color='red', alpha=0.5)+ggtitle("Signal Power Consumption")+ theme(panel.background = element_rect(fill = 'white', colour = 'gray'))
dev.off()

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

