get_updated_Lambda_after_y <- function(y_matrix_old, y_matrix_new, Lambda_list_of_vec, num_file, nv){
  for (i in 1:num_file) {
    for (j in 1:nv[i]) {
      lambda_ij <- Lambda_list_of_vec[[i]][j]
      y_matrix_old_val <- y_matrix_old[lambda_ij, ]
      for (index in 1:nrow(y_matrix_new)) {
        equals <- all(y_matrix_old_val == y_matrix_new[index, ])
        if(equals){
          Lambda_list_of_vec[[i]][j] <- index
        }
      }
    }
  }
  return(Lambda_list_of_vec)
}