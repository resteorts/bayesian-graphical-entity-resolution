# Full conditional of beta_l
rbeta_l_dist <- function(num_obs = 1, l, a_vec, b_vec, z_list_of_mat) {
  # this function returns a random sample from the full conditional of beta_l
  
  # obtain values of a_l and b_l
  a_l <- a_vec[l]
  b_l <- b_vec[l]
  # obtain values of z_l
  z_l <- vector(mode = "numeric", length = 0)
  for (iter in 1:length(z_list_of_mat)) {
    z_l <- c(z_l, z_list_of_mat[[iter]][, l])
  }
  alpha <- a_l + sum(z_l)
  beta <- b_l + sum(1 - z_l)
  return( rbeta(n = num_obs,shape1 = alpha,shape2 = beta) )
}



# Full conditional of theta_l
rtheta_l_dist <- function(num_obs = 1, l, mu_list_of_vec, nv,
                          levels_list_of_vec, D, z_list_of_mat, y_matrix){
  params <- vector(mode = "numeric", length = length(mu_list_of_vec[[l]]) )
  
  for (m in 1:length(mu_list_of_vec[[l]])) {
    mu_lm <- mu_list_of_vec[[l]][m]
    
    sum_indicator_y_l <- 0
    for (j_prime in 1:nrow(y_matrix)) {
      if (y_matrix[j_prime, l] == levels_list_of_vec[[l]][m]) {
        sum_indicator_y_l <- sum_indicator_y_l + 1
      }
    }
    
    sum_z_l_times_indicator_x_l <- 0
    for (i in 1:length(nv)) {
      for (j in 1:nv[i]) {
        if (D[[i]][j, l] == levels_list_of_vec[[l]][m]) {
          sum_z_l_times_indicator_x_l <- sum_z_l_times_indicator_x_l + ( z_list_of_mat[[i]][j, l] * 1 )
        }
      }
    }
    
    big_sum <- sum_indicator_y_l + sum_z_l_times_indicator_x_l
    
    params[m] <- mu_lm + big_sum
    
  }
  return(sample_dirichlet(n = num_obs, alpha = params)) # `sample_dirichlet` needs package `igraph`
  
}



# Full conditional of y_j'l
ry_jprimel_dist <- function(num_samp = 1, j_prime, l, num_file, Lambda_list_of_vec,
                            z_list_of_mat, D, theta_list_of_vec, levels_list_of_vec){
  
  for (i in 1:num_file) {
    j_in_R <- FALSE
    z_ijl_is_zero <- FALSE
    j <- 0
    if(j_prime %in% Lambda_list_of_vec[[i]]){
      j_in_R <- TRUE
      j <- which(Lambda_list_of_vec[[i]] %in% j_prime)[1]
    }
    if(j_in_R == TRUE){
      if(z_list_of_mat[[i]][j, l] == 0){
        z_ijl_is_zero <- TRUE
      }
    }
    if(j_in_R && z_ijl_is_zero){
      return(D[[i]][j, l])
    }
  }
  
  multinorm_res <- rmultinom(n = num_samp, size = 1, prob = theta_list_of_vec[[l]])
  # transpose multinorm_res
  multinorm_res <- t(multinorm_res)
  # get field value corresponding to the position
  multinorm_val <- vector(mode = "numeric", length = 0)
  for (samp in 1:nrow(multinorm_res)) {
    position <- which(multinorm_res[samp,] %in% 1)
    multinorm_val <- c(multinorm_val, levels_list_of_vec[[l]][position])
  }
  return(multinorm_val)
}



# Full conditional of z_ijl
rz_ijl_dist <- function(num_samp = 1, i, j, l, Lambda_list_of_vec, D, y_matrix,
                        levels_list_of_vec, theta_list_of_vec, beta_vec){
  lambda_ij <- Lambda_list_of_vec[[i]][j]
  if (D[[i]][j, l] != y_matrix[lambda_ij, l]) {
    return(1)
  }
  else{
    big_prod <- 1
    for (m in 1:length(levels_list_of_vec[[l]])) {
      if(D[[i]][j, l] == levels_list_of_vec[[l]][m]){
        big_prod <- big_prod * theta_list_of_vec[[l]][m]
      }
    }
    prob <- beta_vec[l] * big_prod / (beta_vec[l] * big_prod + (1 - beta_vec[l]))
    return( rbern(n = num_samp, p = prob) ) # function `rbern` needs package `Rlab`
  }
}



# Full conditional of lambda_i
rlambda_i_dist <- function(i, y_matrix, nv, num_field, z_list_of_mat, D, Lambda_list_of_vec){
  rlambda_iter_count <- 0
  while (TRUE) {
    rlambda_iter_count <- rlambda_iter_count + 1
    lambda_i_vals <- sample(seq(1:nrow(y_matrix)), size = nv[i])
    z_ijl_is_zero <- FALSE
    x_ijl_not_equal_y_cjl <- FALSE
    
    for (j in 1:length(nv[i])) {
      for (l in 1:num_field) {
        if (z_list_of_mat[[i]][j,l] == 0
            && D[[i]][j, l] != y_matrix[lambda_i_vals[j], l]) {
          z_ijl_is_zero <- TRUE
          x_ijl_not_equal_y_cjl <- TRUE
        }
      }
    }
    
    if(rlambda_iter_count > 10000){
      # cat("unable to update lambda", i)
      return(Lambda_list_of_vec[[i]])
    }
    
    if (!z_ijl_is_zero && !x_ijl_not_equal_y_cjl) {
      # print("rlambda iterations")
      # print(rlambda_iter_count)
      return(lambda_i_vals)
    }
  }
}







