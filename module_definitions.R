#' Authors@R: person("Annice", "Najafi", email = "annicenajafi27@gmail.com")
#' Spring 2025
#' The following script defines modules in the DT model for the two link robotic arm
#'

library(dplyr)
library(tidyr)
library(matlib)



#' Module 1
#'
#' @param theta1 theta1 angle of the robotic arm
#' @param theta2 theta2 angle of the robotic arm
#' 
#' @return the analytical solutions for x and y
module.1 <- function(theta1, theta2){
  
  x = L1*cos(theta1)+L2*cos(theta1+theta2)
  y = L1*sin(theta1)+L2*sin(theta1+theta2)
  return(data.frame(x=x, y=y))
}



#' Module 2
#'
#' @param theta1 theta1 angle of the robotic arm
#' @param x x coordinate of the end effector
#' @param y y coordinate of the end effector
#'
#' @return the prediction for theta2
module.2 <- function(theta1, x, y){
  nn_preds <- neuralnet::compute(nn_model, data.frame(x = x, y = y, theta1 = theta1))
  return(as.numeric(nn_preds$net.result))
}


#' Module 3
#'
#' @param theta1 theta1 angle of the robotic arm
#' @param theta2_in theta2 angle input of the robotic arm
#'
#' @return the updated x and y values and theta2 with internal error
module.3 <- function(theta1, theta2_in) {
  
  
  internal_error  <- rnorm(1, mean = 0.01, sd = 0.004)
  theta2_internal <- theta2_in + internal_error
  
  
  x_star <- predict(rf_model.x.star , data.frame(theta1 = theta1, theta2 = theta2_internal))
  y_star <- predict(rf_model.y.star , data.frame(theta1 = theta1, theta2 = theta2_internal))
  
  x_star <- as.numeric(x_star)
  y_star <- as.numeric(y_star)
  
  
  star.df <- data.frame(x = x_star, y = y_star)
  
  
  return(
    list(
      star.df      = star.df,
      theta2_int   = theta2_internal  
    )
  )
}


#' Title
#'
#' @param star.df the output of module 3
#' @param x_actual the actual x value
#' @param y_actual the actual y value
#' @param L1 length of arm link 1
#' @param L2 length of arm link 2
#' @param theta2_present present theta 2 value
#' @param nn_model the neural network model
#'
#' @return theta2 star 
module.4 <- function(star.df, x_actual, y_actual, L1, L2, theta2_present, nn_model) {
  
  dx <- star.df$x - x_actual
  dy <- star.df$y - y_actual
  alignment <- abs(dx) < 0.1 && abs(dy) < 0.1
  
  theta1_new  <- NA
  theta2_corr <- NA
  
  if (alignment) {
    #message("[Module 4] Misalignment (", round(misalignment, 4), 
    #        ") below threshold; skipping correction.")
    theta1_new  <- NA
    theta2_corr <- theta2_present
  } else {
    
    x <- x_actual
    y <- y_actual
    
    
    term1 <- 2*L1*y
    term2 <- sqrt((- L1^2 + 2*L1*L2 - L2^2 + x^2 + y^2) * (L1^2 + 2*L1*L2 + L2^2 - x^2 - y^2))
    denom <- L1^2 + 2*L1*x - L2^2 + x^2 + y^2
    
    theta1_candidate1 <- 2*atan((term1 + term2)/denom)
    theta1_candidate2 <- 2*atan((term1 - term2)/denom)
    
    
    forward_kinematics <- function(theta1) {
      x_pred <- L1 * cos(theta1) + L2 * cos(theta1 + theta2_present)
      y_pred <- L1 * sin(theta1) + L2 * sin(theta1 + theta2_present)
      return((x_pred - x)^2 + (y_pred - y)^2)  
    }
    
    err1 <- forward_kinematics(theta1_candidate1)
    err2 <- forward_kinematics(theta1_candidate2)
    
    
    if (err1 < err2) {
      theta1_new <- theta1_candidate1
    } else {
      theta1_new <- theta1_candidate2
    }
    
    
    pred_df <- data.frame(x = star.df$x, y = star.df$y, theta1 = theta1_new)
    nn_preds <- neuralnet::compute(nn_model, pred_df)$net.result
    theta2_corr <- as.numeric(nn_preds)
    
    #message("[Module 4] Correction applied. New theta1 = ", round(theta1_new, 4),
    #        "; New theta2 = ", round(theta2_corr, 4))
  }
  
  return(theta2_corr = theta2_corr)
}




angular_velocity <- 0.01
time_step        <- 1
T_max            <- 30
num_runs         <- 5000   


df_mae  <- data.frame()
df_mse  <- data.frame()
df_rmse <- data.frame()


n_steps <- floor(T_max / time_step)

for (ext_err in seq(0, 0.3, by = 0.05)) {
  
  
  err1_list <- vector("list", num_runs)
  err2_list <- vector("list", num_runs)
  err3_list <- vector("list", num_runs)
  err4_list <- vector("list", num_runs)
  
  
  for (run in seq_len(num_runs)) {
    print(run)
    theta1       <- 0.2
    ideal_theta2 <- 0
    theta2_dt    <- ideal_theta2 + ext_err
    sim_time     <- 0
    
    e1 <- numeric(n_steps)
    e2 <- numeric(n_steps)
    e3 <- numeric(n_steps)
    e4 <- numeric(n_steps)
    
    for (t in seq_len(n_steps)) {
      
      ideal_theta2 <- ideal_theta2 + angular_velocity * time_step
      theta2_dt    <- theta2_dt    + angular_velocity * time_step
      
      
      m1_act <- module.1(theta1, theta2_dt)
      m1_id  <- module.1(theta1, ideal_theta2)
      e1[t]  <- sqrt((m1_act$x - m1_id$x)^2 +
                       (m1_act$y - m1_id$y)^2)
      
      
      theta2_M2 <- module.2(theta1, m1_act$x, m1_act$y)
      e2[t]     <- abs(theta2_M2 - ideal_theta2)
      
      
      m3_out <- module.3(theta1, theta2_M2)
      m3_id  <- module.3(theta1, ideal_theta2)
      e3[t]  <- sqrt((m3_out$star.df$x - m3_id$star.df$x)^2 +
                       (m3_out$star.df$y - m3_id$star.df$y)^2)
      
      
      theta2_M4 <- module.4(m3_out$star.df,
                            m1_id$x, m1_id$y,
                            L1, L2, theta2_M2, nn_model)
      e4[t]     <- abs(theta2_M4 - ideal_theta2)
      
      theta2_dt <- theta2_M4 + angular_velocity * time_step
    }
    
    err1_list[[run]] <- e1
    err2_list[[run]] <- e2
    err3_list[[run]] <- e3
    err4_list[[run]] <- e4
  }
  
  
  mat1 <- do.call(rbind, err1_list)
  mat2 <- do.call(rbind, err2_list)
  mat3 <- do.call(rbind, err3_list)
  mat4 <- do.call(rbind, err4_list)
  
  
  time_vec <- seq_len(n_steps)
  
  mae1  <- colMeans(abs(mat1), na.rm = TRUE)
  mae2  <- colMeans(abs(mat2), na.rm = TRUE)
  mae3  <- colMeans(abs(mat3), na.rm = TRUE)
  mae4  <- colMeans(abs(mat4), na.rm = TRUE)
  
  mse1  <- colMeans(mat1^2, na.rm = TRUE)
  mse2  <- colMeans(mat2^2, na.rm = TRUE)
  mse3  <- colMeans(mat3^2, na.rm = TRUE)
  mse4  <- colMeans(mat4^2, na.rm = TRUE)
  
  rmse1 <- sqrt(mse1)
  rmse2 <- sqrt(mse2)
  rmse3 <- sqrt(mse3)
  rmse4 <- sqrt(mse4)
  
  
  df_mae  <- rbind(df_mae, 
                   data.frame(time       = time_vec,
                              error_m1   = mae1,
                              error_m2   = mae2,
                              error_m3   = mae3,
                              error_m4   = mae4,
                              external_err = ext_err))
  
  df_mse  <- rbind(df_mse, 
                   data.frame(time       = time_vec,
                              error_m1   = mse1,
                              error_m2   = mse2,
                              error_m3   = mse3,
                              error_m4   = mse4,
                              external_err = ext_err))
  
  df_rmse <- rbind(df_rmse, 
                   data.frame(time       = time_vec,
                              error_m1   = rmse1,
                              error_m2   = rmse2,
                              error_m3   = rmse3,
                              error_m4   = rmse4,
                              external_err = ext_err))
}


write.csv(df_mae,  "~/Downloads/error_prop_mae.csv",  row.names = FALSE)
write.csv(df_mse,  "~/Downloads/error_prop_mse.csv",  row.names = FALSE)
write.csv(df_rmse, "~/Downloads/error_prop_rmse.csv", row.names = FALSE)
