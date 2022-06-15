source("optim.R")
source("function.R")
source("initial.R")
source("structure.R")
source("kalman_filter.R")
source("data_assimilation_func.R")
source("kalman_smoothing.R")
source("kalman_smoothing_function.R")
source("AR_parcor.R")
#source("other_estimate_parameter_function.R")

set.seed(1)

AR <- 2
file_name_s <- c("newly_confirmed_cases_daily")

#kalman filter
#No external force
print(file_name_s)

#DEFINITION

# x0:initial state variable vector
# p0:initial state error variable standard deviation vector
# P0:initial state error variance-covariance matrix
# q:system noise variance vector
# Q:system noise variance-covariance matrix
# r:observation noise variance vector
# R:observation noise variance-covariance matrix
# M:system design matrix
# H:observation design matrix
# x_t:state variable vector
# T:estimate number in time series
# x_f:estimate state predicted distribution(before assimilation)


#PREPARATION

#change

#ARmodel
AR_TorF <- TRUE
AR_degree <- AR

#hyper parameter
#estimate parameter theta
estimate_parameters_num <- 2 + 1 + AR_degree + 1 + 1

#system state vector length
system_state_vector_length <- 2 + 6 + AR_degree

#observation error vector length
observation_error_vector_length <- 1

#initial standard deviation
initial_state_standard_deviation <- 10 ^ 7

#read csv file
name <- paste0("input/", file_name_s, ".csv")
data_csv <- read.csv(name, header = TRUE)
#caution
#The label of the observed value should be "y".

#estimate number (ex.day,hour...)
estimate_number <- length(data_csv[, 1])

#estimate_initial_matrix
estimate_parameters_matrix <- numeric(estimate_parameters_num)
estimate_parameters_matrix[7] <- 9
estimate_parameters_matrix[1] <- 7
estimate_parameters_matrix[2] <- 6
estimate_parameters_matrix[3] <- 8
estimate_parameters_matrix[6] <- 10

result_estimate_parameter <-
  estimate_parameter_function(kf_function_parameter, estimate_parameters_matrix)

name <- paste0("output/estmate_", AR, "_text", ".txt")

#hyper_parameter
print(result_estimate_parameter$estimate$par)
if (AR_TorF == TRUE) {
  print(ar_judgement_stationarity(AR_prepare(para = result_estimate_parameter$estimate$par)))
  print(AR_prepare(para = result_estimate_parameter$estimate$par))
}

result_ans <-
  kf_function(result_estimate_parameter$estimate$par)


result_smoothing <-
  kalman_smooth_RTS(
    x_a = result_ans$x_a,
    x_f = result_ans$x_f,
    P_a = result_ans$P_a,
    P_f = result_ans$P_f,
    para = result_estimate_parameter$estimate$par
  )


#OUTPUT
name <-
  paste0("output/estimaete_",
         AR,
         ".csv")
write.csv(result_ans$x_f, name)
name <- paste0("output/filter_", AR,  ".csv")
write.csv(result_ans$x_a, name)
name <- paste0("output/variance_", AR, ".csv")
write.csv(result_ans$predictive_variance, name)
name <- paste0("output/iter_", AR, ".csv")
write.csv(result_ans$P_a[, , 2] , name)
name <- paste0("output/smoother_", AR, ".csv")
write.csv(result_smoothing$x_a_T , name)
name <- paste0("output/dS_iter_", AR,  ".csv")
write.csv(result_smoothing$P_a_T[, , 1] , name)
name <-
  paste0("output/trend_var_smooth_", AR, ".csv")
write.csv(result_smoothing$P_a_T[1, 1 ,] , name)
