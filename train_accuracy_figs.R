library(dplyr)
library(neuralnet)
library(randomForest)
library(ggplot2)
library(tidyr)


L1 <- 12
L2 <- 9

nn_model <- readRDS('~/Downloads/nn_model.RDS')
rf_model.x.star <- readRDS('~/Downloads/rf_model_x.RDS')
rf_model.y.star <- readRDS('~/Downloads/rf_model_y.RDS')

#This script shows how training was done and how the accuracy figures were generated.
#note that running the script each time would give you results that are slightly different.

#       Neural Network Training Data (for Module 2 prediction) 

#Training set

theta_range_nn <- seq(0, pi/2, length.out = 50)
theta_combinations_nn <- expand.grid(theta1 = theta_range_nn, theta2 = theta_range_nn) %>%
  mutate(x = L1 * cos(theta1) + L2 * cos(theta1 + theta2),
         y = L1 * sin(theta1) + L2 * sin(theta1 + theta2))

nn_model <- neuralnet(theta2 ~ x + y + theta1, data = theta_combinations_nn,
                      hidden = c(10, 5, 5), linear.output = TRUE)


#Testing set

theta_range_nn_test <- seq(0, pi/2, length.out = 30)
theta_combinations_nn_test <- expand.grid(theta1 = theta_range_nn_test, theta2 = theta_range_nn_test) %>%
  mutate(x = L1 * cos(theta1) + L2 * cos(theta1 + theta2),
         y = L1 * sin(theta1) + L2 * sin(theta1 + theta2))



nn_preds <- neuralnet::compute(nn_model, theta_combinations_nn_test[, c("x", "y", "theta1")])$net.result
theta_combinations_nn_test$theta2_pred <- as.numeric(nn_preds)




ggplot(theta_combinations_nn_test, aes(x = theta2, y = theta2_pred)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, col = "#D96098", linetype = "dashed") +
  labs(title = "NN: Predicted vs Actual theta2", x = "Actual theta2", y = "Predicted theta2")+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.title = element_text(hjust = 0.5, size = 20),
    axis.text = element_text(size = 15),
    text = element_text(size = 18)
  )


theta_combinations_nn_test <- theta_combinations_nn_test %>%
  mutate(residuals = theta2_pred - theta2)

mse_value <- round(mean(theta_combinations_nn_test$residuals^2), 8)

ggplot(theta_combinations_nn_test, aes(x = theta2_pred, y = residuals)) +
  geom_point(alpha = 0.6, shape=8, size=1.4, stroke=1.2, color="#8AB2A6") +
  geom_hline(yintercept = 0, col = "#D96098", linetype = "dashed", linewidth=1.5) +
  labs(title = "Neural Network Residuals", x = TeX("Predicted $\\theta_2$"), y = "Residuals", tag="A")+
  annotate("label", 
           x = Inf, y = Inf, 
           label = paste("MSE =", mse_value),
           hjust = 1.1, vjust = 1.1, 
           size = 6, fill = "white", color = "black") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.title = element_text(hjust = 0.5, size = 20),
    axis.text = element_text(size = 15),
    text = element_text(size = 18)
  )->plt.A


ggplot(theta_combinations_nn_test, aes(x = residuals)) +
  geom_histogram(bins = 30, fill = "#D96098", alpha = 0.6) +
  geom_density(size=1.2, color="#8AB2A6")+
  labs(title = "Residuals Histogram", x = "Residuals", y = "Frequency", tag="B")+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.title = element_text(hjust = 0.5, size = 20),
    axis.text = element_text(size = 15),
    text = element_text(size = 18)
  )->plt.B






theta_range_rf <- seq(0, pi/2, length.out = 60)
theta_combinations_rf <- expand.grid(theta1 = theta_range_rf, theta2 = theta_range_rf) %>%
  mutate(x = L1 * cos(theta1) + L2 * cos(theta1 + theta2),
         y = L1 * sin(theta1) + L2 * sin(theta1 + theta2))


rf_model.x.star <- randomForest(x ~ theta1 + theta2, data = theta_combinations_rf, ntree = 500)
rf_model.y.star <- randomForest(y ~ theta1 + theta2, data = theta_combinations_rf, ntree = 500)


#Testing set

theta_range_rf_test <- seq(0, pi/2, length.out = 30)
theta_combinations_rf_test <- expand.grid(theta1 = theta_range_rf_test, theta2 = theta_range_rf_test) %>%
  mutate(x = L1 * cos(theta1) + L2 * cos(theta1 + theta2),
         y = L1 * sin(theta1) + L2 * sin(theta1 + theta2))



theta_combinations_rf$x_pred <- predict(rf_model.x.star, theta_combinations_rf)
theta_combinations_rf$y_pred <- predict(rf_model.y.star, theta_combinations_rf)

rf_x_preds <- predict(rf_model.x.star, theta_combinations_rf)
mse_x <- mean((rf_x_preds - theta_combinations_rf$x)^2)

rf_y_preds <- predict(rf_model.y.star, theta_combinations_rf)
mse_y <- mean((rf_y_preds - theta_combinations_rf$y)^2)



ggplot(theta_combinations_rf, aes(x = x, y = x_pred)) +
  geom_point(alpha = 0.5, color="#8AB2A6") +
  geom_abline(intercept = 0, slope = 1, col = "#D96098", linetype = "dashed", size=1.5) +
  labs(title = "RF - Predicted vs Actual x", x = "Actual x", y = "Predicted x", tag="C")+
  annotate("label", 
           x = Inf, y = Inf, 
           label = paste("MSE =", round(mse_x, 3)),
           hjust = 1.5, vjust = 1.1, 
           size = 6, fill = "white", color = "black")+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.title = element_text(hjust = 0.5, size = 20),
    axis.text = element_text(size = 15),
    text = element_text(size = 18)
  )->plt.C




ggplot(theta_combinations_rf, aes(x = y, y = y_pred)) +
  geom_point(alpha = 0.5, color="#8AB2A6") +
  geom_abline(intercept = 0, slope = 1, col = "#D96098", linetype = "dashed", size=1.5) +
  labs(title = "RF - Predicted vs Actual y", x = "Actual y", y = "Predicted y", tag="D")+
  annotate("label", 
           x = Inf, y = Inf, 
           label = paste("MSE =", round(mse_y, 3)),
           hjust = 1.5, vjust = 1.1, 
           size = 6, fill = "white", color = "black")+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.title = element_text(hjust = 0.5, size = 20),
    axis.text = element_text(size = 15),
    text = element_text(size = 18)
  )->plt.D




grid.arrange(plt.A,plt.B, plt.C, plt.D, nrow=2)






