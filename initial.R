#INITIAL VALUE

#state_matrix_x0
initial_state_matrix_value_x0 <- function(sys_length) {
  #make 0 vector
  ans <- matrix(data = 0,
                nrow = sys_length,
                ncol = 1)
  
  
  return(ans)
  
}

#state_matrix_error_p0
initial_state_matrix_dispersion_sd_p0 <-
  function(sys_length, initial_sd) {
    #make error matrix
    p0 <-  matrix(data = initial_sd,
                  nrow = sys_length,
                  ncol = 1)
    
    return(p0)
    
  }

#system_noise_q
system_noise_sd <- function(parameter, sys_length) {
  error_matrix <- matrix(data = 0.0,
                         nrow = sys_length,
                         ncol = 1)
  
  #change
  error_matrix[1, 1] <- exp(parameter[1])
  error_matrix[2, 1] <- exp(parameter[2])
  error_matrix[3, 1] <- exp(parameter[3])
  error_matrix[9, 1] <- exp(parameter[length(parameter) - 1])
  #####
  
  
  return(error_matrix)
  
}

#observation_error_r
observation_error_sd <- function(parameter, obs_length) {
  error_matrix <- matrix(data = 0.0,
                         nrow = obs_length,
                         ncol = 1)
  
  #change
  error_matrix[1, 1] <- exp(parameter[length(parameter)])
  
  
  return(error_matrix)
  
}


#itereta_ar
iter_ar_distrubtion <- function(parameter) {
  #AR
  ar_coefficient <- AR_prepare(para = parameter)
  R_ar <- (exp(parameter[length(parameter) - 1])) ^ 2
  temp_ar_mat <- c(0, ar_coefficient)
  #temp_ar_mat <- c(0:5)
  
  #Yule Walker
  #-1
  equation_matrix <-
    matrix(
      data = 0,
      nrow = (AR_degree + 1),
      ncol = (AR_degree + 1)
    ) - diag(AR_degree + 1)
  
  num <- c(1:(AR_degree + 1))
  
  temp_n = 0
  
  #arrangement
  for (loop_i in 1:(AR_degree + 1)) {
    temp_n <- temp_n + 1
    temP_in <- num[temp_n:(AR_degree + 1)]
    
    
    if (length(temP_in) != 1) {
      for (loop_j in 1:length(temP_in)) {
        equation_matrix[loop_i, loop_j] <-
          equation_matrix[loop_i, loop_j] + temp_ar_mat[temP_in[loop_j]]
      }
    }
    
    temP_in <- num[temp_n:1]
    if (length(temP_in) != 1) {
      for (loop_j in 2:length(temP_in)) {
        equation_matrix[loop_i, loop_j] <-
          equation_matrix[loop_i, loop_j] + temp_ar_mat[temP_in[loop_j]]
      }
    }
  }
  
  #Yule Walker
  b <- matrix(0, nrow = (AR_degree + 1), ncol = 1)
  b[1, 1] <- -R_ar
  covariance_ar <- solve(equation_matrix, b)
  
  
  #result
  V <- matrix(0, nrow = AR_degree, ncol = AR_degree)
  
  V[1, 1] <- covariance_ar[1, 1]
  for (loop_i in 2:AR_degree) {
    temp_cell <- 0
    for (loop_j in loop_i:AR_degree) {
      temp_cell <-
        temp_cell + ar_coefficient[loop_j] * covariance_ar[abs(loop_j + 1 - loop_i) +
                                                             1, 1]
    }
    V[1, loop_i] <- temp_cell
    V[loop_i, 1] <- temp_cell
  }
  
  for (loop_i in 2:AR_degree) {
    for (loop_j in 2:AR_degree) {
      #Calculation
      temp_cell <- 0
      for (loop_p in loop_i:AR_degree) {
        for (loop_q in loop_j:AR_degree) {
          temp_cell <-
            temp_cell + ar_coefficient[loop_p] * ar_coefficient[loop_q] * covariance_ar[abs(loop_q -
                                                                                              loop_j - loop_p + loop_i) + 1, 1]
          
        }
      }
      V[loop_i, loop_j] <- temp_cell
    }
  }
  
  
  return(V)
  
  
}
