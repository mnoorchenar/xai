library(DALEX)
library(caret)
library(dplyr)# alternatively, this also loads %>%
library(gridExtra)
library(modelStudio)

train_control <- trainControl(method = "cv", number = 5)

mydata$class <- as.factor(mydata$class)

#Neural network
hp_nn <- expand.grid(size = 2:10, decay = seq(0, 0.5, 0.05))
set.seed(2006)
fit_nn <- caret::train(
  form = class ~ .,
  data = mydata,
  trControl = train_control,
  tuneGrid = hp_nn,
  method = "nnet",
  metric = "Accuracy"
)

#Transform the variable to predict into numeric values
mydata <- transform(mydata, class=as.numeric(as.factor(mydata$class))-1) 

#nn model
explainer_nn <- DALEX::explain(fit_nn,  data = mydata[,-ncol(mydata)],y = mydata$class, label = "Neural network")

pdf('xai_global.pdf')
explainer_nn %>% model_parts() %>% plot(show_boxplots = FALSE) + ggtitle("Feature Importance ", "")
dev.off()

explainer_nn <- DALEX::explain(fit_nn, data = mydata[,-ncol(mydata)], y = mydata$class, label = "Neural network")

# simulating in control data
in_control <- signal_simulate(n_simulation = 10)
#simulating anomalies (out of control)
out_control <- signal_simulate(n_simulation = 10, anomaly = T)


test_data = 
  cbind.data.frame(in_control$features, 'class'=0)
test_data = rbind.data.frame(test_data, cbind.data.frame(out_control$features, 'class'=1))

signals <- cbind.data.frame(in_control$signal, out_control$signal)

modelStudio(explainer_nn, new_observation=test_data[95:105,])

pdf('Globalxai.pdf')
for (i in 1:(nrow(test_data))) {
p1 <- explainer_nn %>% predict_parts(new_observation = test_data[i,]) %>% plot(title='Predict Parts')

if (i>10) {
  df_plot <- signals[,c(1,i)]
  colnames(df_plot) <- c('InControl', 'OutControl')
  df_plot['Time'] <- 1:nrow(df_plot)
  
  p2 <- ggplot(data = df_plot,aes(x = Time, y = InControl)) + geom_line(color='#69b3a2') + geom_line(data = df_plot,aes(x = Time, y = OutControl), color='red', alpha=0.5)+ggtitle("Signal Power Consumption")+ theme(panel.background = element_rect(fill = 'white', colour = 'gray'))
  
}else{
  df_plot <- signals[i]
  df_plot['Time'] <- 1:nrow(df_plot)
  colnames(df_plot)[1] <- c('InControl')
  p2 <- ggplot() + geom_line(data = df_plot,aes(x = Time, y = InControl), color='#69b3a2')+ggtitle("Signal Power Consumption")+ theme(panel.background = element_rect(fill = 'white', colour = 'gray'))
  
}

grid.arrange(p1 , p2, nrow = 2)
}
dev.off()
