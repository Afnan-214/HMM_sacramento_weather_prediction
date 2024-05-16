# read the data
library("xlsx")
data <- read.xlsx("D:/FCDS/fall2023_24/stochastic/Sac_data.xlsx", sheetIndex=1, header=TRUE)
data

#Data preprocessing
plot(data$parks_mobility_percent)
abline(h = c(-10, 10), col = "red")
median.default(data$Avg_Temp)
mean(data$Avg_Temp)
plot(data$Avg_Temp)
abline(h = 63, col = "red")
data$rain <- ifelse(data$Precipitation > 0, 1, 0)
data$weather_states <- ifelse(data$rain == 1, "rain",
                              ifelse(data$rain == 0 & data$Avg_Temp < 63, "clear", "warm"))
data$mobility_obs <- ifelse(data$parks_mobility_percent < -10, "low",
                            ifelse(data$parks_mobility_percent > 10, "high","med"))
data$new_mobility <- ifelse(data$mobility_obs == "low", "L",
                            ifelse(data$mobility_obs == "med", "M", "H"))
data
#length of the data
n <- nrow(data)
n
#Transition Matrix
clear_count <- table(data$weather_states)["clear"]
clear_count
rain_count <- table(data$weather_states)["rain"]
rain_count
warm_count <- table(data$weather_states)["warm"]
warm_count
calculate_transition_counts <- function(data) {
  count_cc <- 0
  count_cr <- 0
  count_cw <- 0
  count_rc <- 0
  count_rr <- 0
  count_rw <- 0
  count_wc <- 0
  count_wr <- 0
  count_ww <- 0
  for (i in 1:(nrow(data) - 1)) {
    current_state <- data$weather_states[i]
    next_state <- data$weather_states[i + 1]
    if (current_state == "clear" && next_state == "clear") {
      count_cc <- count_cc + 1
    } else if (current_state == "clear" && next_state == "rain") {
      count_cr <- count_cr + 1
    } else if (current_state == "clear" && next_state == "warm") {
      count_cw <- count_cw + 1
    } else if (current_state == "rain" && next_state == "clear") {
      count_rc <- count_rc + 1
    } else if (current_state == "rain" && next_state == "rain") {
      count_rr <- count_rr + 1
    } else if (current_state == "rain" && next_state == "warm") {
      count_rw <- count_rw + 1
    } else if (current_state == "warm" && next_state == "clear") {
      count_wc <- count_wc + 1
    } else if (current_state == "warm" && next_state == "rain") {
      count_wr <- count_wr + 1
    } else if (current_state == "warm" && next_state == "warm") {
      count_ww <- count_ww + 1
    }
  }
  result <- list(count_cc = count_cc,
                 count_cr = count_cr,
                 count_cw = count_cw,
                 count_rc = count_rc,
                 count_rr = count_rr,
                 count_rw = count_rw,
                 count_wc = count_wc,
                 count_wr = count_wr,
                 count_ww = count_ww)
  return(result)
}
transition_counts <- calculate_transition_counts(data)
print(transition_counts)
Pcc <- transition_counts$count_cc / clear_count
Pcr <- transition_counts$count_cr / clear_count
Pcw <- transition_counts$count_cw / clear_count
Prc <- transition_counts$count_rc / rain_count
Prr <- transition_counts$count_rr / rain_count
Prw <- transition_counts$count_rw / rain_count
Pwc <- transition_counts$count_wc / (warm_count-1)
Pwr <- transition_counts$count_wr / (warm_count-1)
Pww <- transition_counts$count_ww / (warm_count-1)
Transition_Matrix <- matrix(c(Pcc, Pcr, Pcw, Prc, Prr, Prw, Pwc, Pwr, Pww), nrow = 3, ncol = 3, byrow = TRUE,
                            dimnames = list(c("clear", "rain", "warm"), c("clear", "rain", "warm")))
Transition_Matrix
#Emission Matrix
clear_low_count <- sum(data$weather_states == "clear" & data$mobility_obs == "low")
clear_med_count <- sum(data$weather_states == "clear" & data$mobility_obs == "med")
clear_high_count <- sum(data$weather_states == "clear" & data$mobility_obs == "high")
rain_low_count <- sum(data$weather_states == "rain" & data$mobility_obs == "low")
rain_med_count <- sum(data$weather_states == "rain" & data$mobility_obs == "med")
rain_high_count <- sum(data$weather_states == "rain" & data$mobility_obs == "high")
warm_low_count <- sum(data$weather_states == "warm" & data$mobility_obs == "low")
warm_med_count <- sum(data$weather_states == "warm" & data$mobility_obs == "med")
warm_high_count <- sum(data$weather_states == "warm" & data$mobility_obs == "high")
P_L_C <- clear_low_count / clear_count
P_M_C <- clear_med_count / clear_count
P_H_C <- clear_high_count / clear_count
P_L_R <- rain_low_count / rain_count
P_M_R <- rain_med_count / rain_count
P_H_R <- rain_high_count / rain_count
P_L_W <- warm_low_count / warm_count
P_M_W <- warm_med_count / warm_count
P_H_W <- warm_high_count / warm_count
Emission_Matrix <- matrix(c(P_L_C, P_M_C, P_H_C, P_L_R, P_M_R, P_H_R, P_L_W, P_M_W, P_H_W), nrow = 3, ncol 
                          = 3, byrow = TRUE,
                          dimnames = list(c("clear", "rain", "warm"), c("L", "M", "H")))
Emission_Matrix
#Intial Distribution
pi <- c((clear_count/n),(rain_count/n),(warm_count/n))
pi
#Forward Algorithm
a <- Transition_Matrix
b <- Emission_Matrix
initial_distribution <- pi
b
forward = function(v, a, b, initial_distribution){
  T = length(v)
  m = nrow(a)
  alpha = matrix(0, T, m)
  alpha[1, ] = initial_distribution*b[, v[1]]
  for(t in 2:T){
    tmp = alpha[t-1, ] %*% a
    alpha[t, ] = tmp * b[, v[t]]
  }
  return(alpha)
}
Forward = forward(data$new_mobility,a,b,initial_distribution)
Forward
Total_prob_forward <- sum(Forward[92,])
Total_prob_forward
#Backward Algorithm
backward = function(V, A, B){
  T = length(V)
  m = nrow(A)
  beta = matrix(1, T, m)
  for(t in (T-1):1){
    tmp = as.matrix(beta[t+1, ] * B[, V[t+1]])
    beta[t, ] = t(A %*% tmp)
  }
  return(beta)
}
Backward = backward(data$new_mobility,a,b)
Backward
Total_prob_backward <- sum(Backward[1,])
Total_prob_backward
#Viterbi Algorithm 
viterbi = function(P, E, pi, S, seq){
  T = length(seq)
  n = length(S)
  V = matrix(0, n, T)
  colnames(V) = seq
  rownames(V) = S
  opt_path = rep(0, T)
  
  V[, 1] = pi*E[, seq[1]]
  opt_path[1] = S[which.max(V[, 1])]
  
  for(t in 2:T){
    
    for(s in 1:n){
      probs = V[, t-1]*P[, s]*E[s, seq[t]]
      V[s, t] = max(probs)
    }
    
    opt_path[t] = S[which.max(V[, t])]
  }
  
  viterbi_output = list("V matrix" = V , "optimal path" = opt_path)
  return(viterbi_output)
}
O = c("low", "med", "high")
S = c("clear", "rain", "warm")
colnames(Emission_Matrix) = O
rownames(Emission_Matrix) = S
colnames(Transition_Matrix) = S
rownames(Transition_Matrix) = S
Transition_Matrix
Emission_Matrix
#Viterbi Algorithm for decoding
viterbi(Transition_Matrix, Emission_Matrix, pi, S, unlist(data["mobility_obs"]))
#Baum Welch
Baum_welch <- function(P,E,pi,alpha,beta,seq,n_iter) {
  itrations = n_iter
  T = length(seq)
  n = length(S)
  n_obs = length(O)
  expi <- array(0,dim = c(n,n,T-1))
 
  for (i in 1:itrations){
    # exp(ij) matrix
    for(t in 1:T-1){
      sum_ij_t <- ((alpha[t,] %*% P) * E[,seq[t+1]]) %*% matrix(beta[t+1,])
      for(s in 1:n){
        exp_ij_t = alpha[t,s] * P[s,] * E[,seq[t+1]] * beta[t+1,]
        expi[s,,t]=exp_ij_t/as.vector(sum_ij_t)
      }
    }
    # gamma matrix
    gamma = apply(expi, c(1, 3), sum)
    gamma = cbind(gamma, colSums(expi[, , T-1]))
    
    # estimated lambda
    # 1. estimated initial dist: exp freq. of being in (i) at t=1
    pi <- gamma[,1]
    
    # 2. estimated transition matrix
    expi_t <- rowSums(expi, dims = 2) # exp trans from (i) to (j)
    for(r in 1:n){
      P[r,] <- expi_t[r,] / as.vector(sum(gamma[r,1:T-1]))
    }
    
    # 3. estimated emission matrix
    colnames(gamma) <- seq
    rownames(gamma) <- S
    for (obs in seq){
      E[,obs] <- rowSums(as.matrix(gamma[, which(seq==obs)])) # exp be in (j)|O=k
    }
    for (e in 1:n){
      E[e,] <- E[e,] / as.vector(sum(gamma[e,]))
    }
  }
  
BW_output <- list("Expected ij" = expi,"Gamma matrix" = gamma,
                    "initial dist" = pi,"Transition matrix" = P,"Emition matrix" =E)
  return(BW_output)
}

BM = Baum_welch(Transition_Matrix, Emission_Matrix, pi, Forward, Backward, data$mobility_obs, 100)
BM
