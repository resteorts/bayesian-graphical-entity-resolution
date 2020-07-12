sampleGibbs <- function (D, n.iter = 1000, a_vec, b_vec, mu_list_of_vec, beta_vec, theta_list_of_vec, y_matrix,
                         z_list_of_mat, Lambda_list_of_vec, 
                         nt, nv, num_field, num_file, levels_list_of_vec){
  # some initializaiton
  timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  outputfilename <- paste("results/Lambda-",timestamp,".txt",sep="")
  # outputfilename_y_length <- paste("results/y_length",timestamp,".txt",sep="")
  outputfilename_gibbs_var <- paste("results/gibbs-var-",timestamp,".RData",sep="")
  res_Lambda <- matrix(data = NA, nrow = n.iter, ncol = nt)
  for (overall_iter in 1:n.iter) {
    print(paste("iteration", overall_iter, "of", n.iter))
    # update beta
    # print("update beta")
    for (l in 1:num_field) {
      beta_vec[l] <- rbeta_l_dist(num_obs = 1, l, a_vec, b_vec, z_list_of_mat)
    }
    
    # update theta
    # print("update theta")
    for (l in 1:num_field) {
      theta_list_of_vec[[l]] <- rtheta_l_dist(num_obs = 1, l, mu_list_of_vec, nv,
                                              levels_list_of_vec, D, z_list_of_mat, y_matrix)
    }
    
    # update y
    # print("update y")
    for (j_prime in 1:nrow(y_matrix)) {
      for (l in 1:num_field) {
        y_matrix[j_prime, l] <- ry_jprimel_dist(num_samp = 1, j_prime, l, num_file, Lambda_list_of_vec,
                                                z_list_of_mat, D, theta_list_of_vec, levels_list_of_vec)
      }
    }
    # remove duplicate entries in y and update Lambda accordingly
    # if(if_remove_dup_y == TRUE){
    #   y_matrix_new <- unique(y_matrix)
    #   Lambda_list_of_vec <- get_updated_Lambda_after_y(y_matrix_old = y_matrix,
    #                                                    y_matrix_new = y_matrix_new,
    #                                                    Lambda_list_of_vec,
    #                                                    num_file,
    #                                                    nv)
    #   y_matrix <- y_matrix_new
    # }
    
    # update z
    # print("update z")
    for (i in 1:length(nv)) {
      for (j in 1:nv[i]) {
        for (l in num_field) {
          z_list_of_mat[[i]][j, l] <- rz_ijl_dist(num_samp = 1, i, j, l, Lambda_list_of_vec, D, y_matrix,
                                                  levels_list_of_vec, theta_list_of_vec, beta_vec)
        }
      }
    }
    
    # update Lambda
    # print("update Lambda")
    for (i in 1:length(nv)) {
      Lambda_list_of_vec[[i]] <- rlambda_i_dist(i, y_matrix, nv, num_field, z_list_of_mat, D, Lambda_list_of_vec)
    }
    
    # res[overall_iter, 1] <- beta_vec
    # res[overall_iter, 2] <- theta_list_of_vec
    # res[overall_iter, 3] <- y_matrix
    # res[overall_iter, 4] <- z_list_of_mat
    # res[overall_iter, 5] <- Lambda_list_of_vec
    
    res_Lambda[overall_iter,] <- c(Lambda_list_of_vec[[1]], Lambda_list_of_vec[[2]], Lambda_list_of_vec[[3]])
    write.table(matrix(res_Lambda[overall_iter,], nrow=1), outputfilename, append=TRUE,
                row.names=FALSE, col.names=FALSE)
    # save all variables relevant to this iteration as most recent to guard against crashes
    save(a_vec, b_vec, mu_list_of_vec,
         beta_vec, theta_list_of_vec, y_matrix, z_list_of_mat, Lambda_list_of_vec, 
         nt, nv, num_field, num_file, levels_list_of_vec, overall_iter,
         file = "results/latest-gibbs-vars-in-loop.RData")
    flush.console()
  }
  # save all variables other than Lambda relevant to this gibbs session into a time-stamped RData file
  save(a_vec, b_vec, mu_list_of_vec,
       beta_vec, theta_list_of_vec, y_matrix, z_list_of_mat, Lambda_list_of_vec, 
       nt, nv, num_field, num_file, levels_list_of_vec,
       file = outputfilename_gibbs_var)
  # save all variables other than Lambda as most recent
  save(a_vec, b_vec, mu_list_of_vec,
       beta_vec, theta_list_of_vec, y_matrix, z_list_of_mat, Lambda_list_of_vec, 
       nt, nv, num_field, num_file, levels_list_of_vec,
       file = "results/latest-gibbs-vars.RData")
  # delete file of in-loop most recent gibbs variables
  file.remove("results/latest-gibbs-vars-in-loop.RData")
  
  print(paste("Lambda's timestamp is", timestamp))
  
  # return(res_Lambda)
}