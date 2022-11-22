library(DALEX)
library(caret)
library(dplyr)    # alternatively, this also loads %>%
library(gridExtra)

train_control <- trainControl(method = "cv", number = 5)
metric <- "Accuracy"

mydata$class <- as.factor(mydata$class)

#Neural network
hp_nn <- expand.grid(size = 2:10,
                     decay = seq(0, 0.5, 0.05))
set.seed(2006)
fit_nn <- train(
  form = class ~ .,
  data = mydata,
  trControl = train_control,
  tuneGrid = hp_nn,
  method = "nnet",
  metric = metric
)

#Transform the variable to predict into numeric values
mydata <- transform(mydata, class=as.numeric(as.factor(mydata$class))-1) 

#nn model
explainer_nn <- DALEX::explain(fit_nn,
                               data = mydata[,-ncol(mydata)],
                               y = mydata$class, 
                               label = "Neural network")

explainer_nn %>% model_parts() %>% plot(show_boxplots = FALSE) + ggtitle("Feature Importance ", "")


# simulating in control data
in_control <- signal_simulate(n_simulation = 10, limit_mean = mean_in_control, limit_sd=sd_in_control)
#simulating anomalies (out of control)
out_control <- signal_simulate(n_simulation = 10, limit_mean = mean_out_control,limit_sd = sd_out_control)

test_data = 
  cbind.data.frame(in_control$features, 'class'=0)
test_data = rbind.data.frame(test_data, cbind.data.frame(out_control$features, 'class'=1))

plot(in_control$signal$V1, type = 'l', col="#69b3a2", lwd=1.5)
points(out_control$signal$V1, type = 'l', col=alpha("red",0.5), lwd=2)

explainer_nn <- DALEX::explain(fit_nn,
                               data = mydata[,-ncol(mydata)], y = mydata$class, label = "Neural network")

explainer_nn %>% predict_parts(new_observation = test_data[1,]) %>% plot()
