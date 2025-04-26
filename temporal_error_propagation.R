#' Authors@R: person("Annice", "Najafi", email = "annicenajafi27@gmail.com")
#' Spring 2025
#' The following script can be used to track the temporal propagation of error in the DT model. 
#' You have you to first train the NN and RF models and run the modules code to be able to run this script.

#####CTMC case


#Define parameteres


#These parameters are taken from Matlab results
sigma     <- 0.1
epsilon_M1 <- 3
epsilon_M2 <- 0.12 * epsilon_M1 - 0.05 + sigma
epsilon_M3 <- 7.89 * epsilon_M2 + 0.18 + sigma
epsilon_M4 <- 0.12 * epsilon_M3 + 0.07 + sigma


angular_velocity <- 0.05
time_step        <- 0.1
T_max            <- 30     
num_runs         <- 1000  
ext_err          <- 0.2   


waiting_times <- list(
  M1 = rep(NA, num_runs),
  M2 = rep(NA, num_runs),
  M3 = rep(NA, num_runs),
  M4 = rep(NA, num_runs)
)


for(run in seq_len(num_runs)) {
  
  print(run)
  theta1       <- 0.2
  ideal_theta2 <- 0
  theta2_dt    <- ideal_theta2 + ext_err
  
  
  crossed <- c(M1=FALSE, M2=FALSE, M3=FALSE, M4=FALSE)
  
  first_passage <- c(M1=NA, M2=NA, M3=NA, M4=NA)
  
  t <- 0
  while(t < T_max && any(!crossed)) {
    
    ideal_theta2 <- ideal_theta2 + angular_velocity * time_step
    theta2_dt    <- theta2_dt    + angular_velocity * time_step
    
    
    m1_act   <- module.1(theta1,    theta2_dt)
    m1_ideal <- module.1(theta1,    ideal_theta2)
    err1 <- sqrt(((m1_act$x - m1_ideal$x)^2 + (m1_act$y - m1_ideal$y)^2))
    if(!crossed["M1"] && err1 > epsilon_M1) {
      crossed["M1"]        <- TRUE
      first_passage["M1"]  <- t
    }
    
    
    theta2_M2 <- module.2(theta1, m1_act$x, m1_act$y)
    err2      <- abs(theta2_M2 - ideal_theta2)
    if(!crossed["M2"] && err2 > epsilon_M2) {
      crossed["M2"]        <- TRUE
      first_passage["M2"]  <- t
    }
    
    
    m3_out   <- module.3(theta1, theta2_M2)
    star_df  <- m3_out$star.df
    m3_idl   <- module.3(theta1, ideal_theta2)$star.df
    err3 <- sqrt(((star_df$x - m3_idl$x)^2 + (star_df$y - m3_idl$y)^2))
    if(!crossed["M3"] && err3 > epsilon_M3) {
      crossed["M3"]        <- TRUE
      first_passage["M3"]  <- t
    }
    
    
    theta2_M4 <- module.4(star_df, m1_act$x, m1_act$y,
                          L1, L2, theta2_M2, nn_model)
    err4      <- abs(theta2_M4 - ideal_theta2)
    if(!crossed["M4"] && err4 > epsilon_M4) {
      crossed["M4"]        <- TRUE
      first_passage["M4"]  <- t
    }
    
    
    theta2_dt <- theta2_M4 + angular_velocity * time_step
    t <- t + time_step
  }
  
  waiting_times$M1[run] <- first_passage["M1"]
  waiting_times$M2[run] <- first_passage["M2"]
  waiting_times$M3[run] <- first_passage["M3"]
  waiting_times$M4[run] <- first_passage["M4"]
}


wait_M1 <- waiting_times$M1[!is.na(waiting_times$M1)]
wait_M2 <- waiting_times$M2[!is.na(waiting_times$M2)]
wait_M3 <- waiting_times$M3[!is.na(waiting_times$M3)]
wait_M4 <- waiting_times$M4[!is.na(waiting_times$M4)]




mean12 <- mean(wait_M1, na.rm=TRUE)
mean23 <- mean(wait_M2, na.rm=TRUE)
mean34 <- mean(wait_M3, na.rm=TRUE)
mean45 <- mean(wait_M4, na.rm=TRUE)

lambda12 <- 1/mean12  
lambda23 <- 1/mean23 
lambda34 <- 1/mean34  
lambda45 <- 1/mean45

Q <- matrix(0, nrow=5, ncol=5)

Q[1,1] <- -lambda12
Q[1,2] <-  lambda12

Q[2,2] <- -lambda23
Q[2,3] <-  lambda23

Q[3,3] <- -lambda34
Q[3,4] <-  lambda34

Q[4,4] <- -lambda34
Q[4,5] <-  lambda34


p0 <- c(1, 0, 0, 0, 0)  

t_vals <- seq(0, 100, by=0.1)  
df_plot <- data.frame()

for (t in t_vals) {
  
  P_t <- p0 %*% expm(Q * t)  
  df_plot <- rbind(df_plot, 
                   data.frame(time   = t,
                              p_M1   = P_t[1],
                              p_M2   = P_t[2],
                              p_M3   = P_t[3],
                              p_M4   = P_t[4],
                              p_M5   = P_t[5]))
}


df_long_continuous <- reshape2::melt(df_plot, id.vars="time", 
                                     variable.name="Module", value.name="Probability")

ggplot(df_long_continuous, aes(x=time, y=Probability, color=Module)) +
  geom_line(size=1.2) +
  labs(
    x     = "Time",
    y     = "Probability",
    color = "Failure State",
    title = "CTMC - State Probabilities Over Time"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title      = element_text(hjust = 0.5)
  )+xlim(c(0,10))+
  theme(
    panel.background   = element_blank(),
    panel.grid.major   = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.line          = element_line(colour = "black"),
    plot.title         = element_text(hjust = 0.5, size = 20),
    axis.text          = element_text(size = 15),
    text               = element_text(size = 15),
    legend.position    = "right"
  )+scale_color_manual(values = c('#210F37', '#48A6A7',
                                  '#6F826A', '#C95792', '#94B4C1'))+
  labs(tag='B')->plt.A







#####DTMC case



sigma     <- 0.1
eps1      <- 3
eps2      <- 0.12 * eps1 - 0.05 + sigma
eps3      <- 7.89 * eps2 + 0.18 + sigma
eps4      <- 0.12 * eps3 + 0.07 + sigma

omega     <- 0.05    
dt        <- 0.1     
T_max     <- 30      
T_ticks   <- T_max / dt
N_runs    <- 1000   
ext_err   <- 0.2     


normalize_rows <- function(mat) {
  for(i in seq_len(nrow(mat))) {
    s <- sum(mat[i, ])
    if(s > 0) {
      mat[i, ] <- mat[i, ] / s
    } else {
      
      mat[i, ] <- 0
      mat[i, i] <- 1
    }
  }
  mat
}


all_states <- matrix(NA_integer_, nrow=N_runs, ncol=T_ticks+1)

for(run in seq_len(N_runs)) {
  print(run)
  
  theta1       <- 0.2
  ideal_theta2 <- 0
  obs_theta2   <- ideal_theta2 + ext_err
  
  crossed <- c(M1=FALSE, M2=FALSE, M3=FALSE, M4=FALSE)
  
  all_states[run, 1] <- 1
  
  for(tick in 1:T_ticks) {
    
    ideal_theta2 <- ideal_theta2 + omega * dt
    obs_theta2   <- obs_theta2   + omega * dt
    
    
    m1_act   <- module.1(theta1,    obs_theta2)
    m1_idl   <- module.1(theta1,    ideal_theta2)
    err1     <- sqrt(((m1_act$x - m1_idl$x)^2 + (m1_act$y - m1_idl$y)^2))
    if(!crossed["M1"] && err1 > eps1) crossed["M1"] <- TRUE
    
    
    theta2_M2 <- module.2(theta1, m1_act$x, m1_act$y)
    err2      <- abs(theta2_M2 - ideal_theta2)
    if(!crossed["M2"] && err2 > eps2) crossed["M2"] <- TRUE
    
    
    m3_out  <- module.3(theta1, theta2_M2)
    star_df <- m3_out$star.df
    m3_idl  <- module.3(theta1, ideal_theta2)$star.df
    err3    <- sqrt(((star_df$x - m3_idl$x)^2 + (star_df$y - m3_idl$y)^2))
    if(!crossed["M3"] && err3 > eps3) crossed["M3"] <- TRUE
    
    
    theta2_M4 <- module.4(star_df, m1_act$x, m1_act$y,
                          L1, L2, theta2_M2, nn_model)
    err4      <- abs(theta2_M4 - ideal_theta2)
    if(!crossed["M4"] && err4 > eps4) crossed["M4"] <- TRUE
    
    
    all_states[run, tick+1] <- sum(crossed) + 1
    
    obs_theta2 <- theta2_M4 + omega * dt
  }
}



nS        <- 5
counts_hom <- matrix(0, nS, nS)
for(run in 1:N_runs) {
  for(t in 1:T_ticks) {
    i <- all_states[run, t]
    j <- all_states[run, t+1]
    counts_hom[i, j] <- counts_hom[i, j] + 1
  }
}
P_hom <- normalize_rows(counts_hom)


P_hom[5, ] <- c(0,0,0,0,1)



pi0   <- c(1, 0, 0, 0, 0)
pi_hom <- matrix(NA, nrow=T_ticks*10+1, ncol=nS)
pi_hom[1,] <- pi0


for(t in 1:T_ticks*10) {
  pi_hom[t+1, ] <- pi_hom[t, ] %*% P_hom
}






df_hom <- as.data.frame(pi_hom)
colnames(df_hom) <- c(
  "p_M1",
  "p_M2",
  "p_M3",
  "p_M4",
  "p_M5"
)
df_hom$time <- 0:T_ticks


df_long_disc <- df_hom %>%
  pivot_longer(
    cols = -time,
    names_to  = "state",
    values_to = "prob"
  )


ggplot(df_long_disc, aes(x = time, y = prob, color = state)) +
  geom_line(size = 1) +
  labs(
    x     = "Time",
    y     = "Probability",
    color = "Failure State",
    title = "DTMC - State Probabilities Over Time", tag='A'
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title      = element_text(hjust = 0.5)
  )+xlim(c(0,50))+
  theme(
    panel.background   = element_blank(),
    panel.grid.major   = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.line          = element_line(colour = "black"),
    plot.title         = element_text(hjust = 0.5, size = 20),
    axis.text          = element_text(size = 15),
    text               = element_text(size = 15),
    legend.position    = "right"
  )+scale_color_manual(values = c('#210F37', '#48A6A7', '#6F826A', '#C95792', '#94B4C1'))->plt.B






#####Importance measure


compute_mfpt <- function(Q) {

  QT <- Q[1:(nrow(Q)-1), 1:(ncol(Q)-1)]
  QT_inv <- solve(QT)
  mfpt <- -rowSums(QT_inv)
  return(mfpt[1])  
}


mfpt_base <- compute_mfpt(Q)


importance_measures <- numeric(4)  


delta <- 1e-5


for (i in 1:4) {
  Q_perturbed <- Q
  
 
  j <- which(Q[i,] > 0)[1] 
  perturb_amount <- Q[i, j] * delta
  
  Q_perturbed[i, j] <- Q[i, j] + perturb_amount
  Q_perturbed[i, i] <- -sum(Q_perturbed[i, -i])
  

  mfpt_perturbed <- compute_mfpt(Q_perturbed)
  
  importance_measures[i] <- (mfpt_perturbed - mfpt_base) / mfpt_base
}


names(importance_measures) <- paste0("Module_", 1:4)
print(importance_measures)


importance_df <- data.frame(
  Module = names(importance_measures),
  Importance = importance_measures
)


ggplot(importance_df, aes(x = Module, y = Importance, fill=Module)) +
  geom_bar(stat = "identity") +
  labs(title = "Importance Measures by Module",
       x = "Module",
       y = "Importance", tag='A') +
  theme(
    panel.background   = element_blank(),
    panel.grid.major   = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.line          = element_line(colour = "black"),
    plot.title         = element_text(hjust = 0.5, size = 20),
    axis.text          = element_text(size = 15),
    text               = element_text(size = 15),
    legend.position    = "right"
  )+scale_fill_manual(values = c('#210F37', '#48A6A7',  '#C95792', '#94B4C1'))->plt.1






compute_importance <- function(Q, module_index) {
  delta_fraction <- 0.01 
  Q_perturbed <- Q
  
  j <- which(Q[module_index,] > 0)[1]  
  
  perturb_amount <- Q[module_index, j] * delta_fraction
  Q_perturbed[module_index, j] <- Q[module_index, j] + perturb_amount
  Q_perturbed[module_index, module_index] <- -sum(Q_perturbed[module_index, -module_index])
  
  mfpt_base <- compute_mfpt(Q)
  mfpt_perturbed <- compute_mfpt(Q_perturbed)
  
  return((mfpt_perturbed - mfpt_base) / mfpt_base)
}


multipliers <- seq(0.5, 1.5, length.out = 20)  
modules <- 1:4


sensitivity_results <- list()

for (m in modules) {
  importances <- numeric(length(multipliers))
  for (k in seq_along(multipliers)) {
    Q_mod <- Q
    j <- which(Q[m,] > 0)[1]
    original_rate <- Q[m, j]
    
    Q_mod[m, j] <- original_rate * multipliers[k]
    Q_mod[m, m] <- -sum(Q_mod[m, -m])  
    

    importances[k] <- compute_importance(Q_mod, m)
  }
  sensitivity_results[[paste0("Module_", m)]] <- data.frame(Multiplier = multipliers, Importance = importances)
}



plot_data <- sensitivity_results$Module_1

ggplot(plot_data, aes(x = Multiplier, y = Importance)) +
  geom_line(size=1.5) +
  geom_point(shape=8, stroke=1.5) +
  labs(title = "Sensitivity of Importance for Module 1",
       x = "Transition Rate Multiplier",
       y = "Relative Importance Change", tag='B')+
  theme(
    panel.background   = element_blank(),
    panel.grid.major   = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.line          = element_line(colour = "black"),
    plot.title         = element_text(hjust = 0.5, size = 20),
    axis.text          = element_text(size = 15),
    text               = element_text(size = 15),
    legend.position    = "right"
  )->plt.2





