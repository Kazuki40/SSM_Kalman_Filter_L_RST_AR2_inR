AR_parcor <- function(para1) {
  #temp
  temp_matrix <- diag(para1)#Diagonalization
  
  for (m in 1:AR_degree) {
    for (j in 1:m) {
      if (j != m) {
        #update_parcor
        temp_matrix[m, j] = temp_matrix[m - 1 , j] - temp_matrix[m, m] * temp_matrix[m - 1, m - j]
      }
    }
  }
  
  #write_ans
  return (temp_matrix[AR_degree, ])
  
}


AR_all_only <- function(para) {
  temp_vector <- numeric(AR_degree)
  for (i in 1:AR_degree) {
    #ar last
    temp <- estimate_parameters_num - (AR_degree + 2)
    
    #degree in value
    temp_vector[i] <- para[temp + i]
    
  }
  return(temp_vector)
  
}

AR_parcor_prepare <- function(all_para) {
  temp_vector <- numeric(AR_degree)
  ans_vector <- numeric(AR_degree)
  
  #transrate
  temp_vector <- AR_all_only(all_para)
  
  #Convert
  ans_vector =  ((exp(temp_vector) - 1) / (exp(temp_vector) + 1)) * 0.99
  
  return (ans_vector)
  
  
}

ar_judgement_stationarity <- function(para1) {
  temp_num <- AR_degree + 1
  temp <- numeric(temp_num)
  j = 0
  for (i in AR_degree:1) {
    j = j + 1
    temp[j] = -para1[i]
  }
  temp[temp_num] = 1
  #0 ,x^1,x^2...
  characteristic_equ <- polyroot(temp)
  
  #length
  ans <- abs(characteristic_equ)
  
  return (ans)
  
}

AR_prepare <- function(para) {
  #No limit and  Overall
  #Always Stationarity
  new_para <- AR_parcor_prepare(all_para = para)
  
  #parcor Stationarity
  ar_coerricient <- AR_parcor(para1 = new_para)
  
  return (ar_coerricient)
  
}
