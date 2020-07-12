compute_file_lengths_vec <- function(data) {
  # Computes the number of observations in each file in provided data
  # 
  # Args: 
  #   data: a list of data frames, each data frame representing a file
  #
  # Returns:
  #   a vector, each entry being the number observations in the corresponding file
  return(sapply(data, function(x)dim(x)[1]))
}

compute_num_file <- function(data) {
  # Computes the number of files in the provided data
  #
  # Args:
  #   data: a list of data frames, each data frame representing a file
  #
  # Returns:
  #   a scalar, the number of files in the provided data
  return(length(data))
}

compute_total_num_obs <- function(data) {
  # Computes the total number of observations in the provided data
  #
  # Args:
  #   data: a list of data frames, each data frame representing a file
  #
  # Returns:
  #   a scalar, the total number of observations in the provided data
  return(sum(compute_file_lengths_vec(data)))
}

compute_num_population <- function(id_data) {
  # Computes the size of the population, i.e., the number of latent entities
  # 
  # Args:
  #   id_data: a vector of unique identifiers associated with each observation in data
  # 
  # Returns:
  #   a scalar, the size of the population
  return(length(unique(id_data)))
}

compute_num_field <- function(data) {
  # Computes the number of fields/features of the input data
  # 
  # Args:
  #   data: a list of data frames, each data frame representing a file
  # 
  # Returns:
  #   a scalar, the number of fields/features of the unput data
  return(ncol(data[[1]]) - 1)
}

compute_m_l_vec <- function(data) {
  # Computes the number of levels for each field/feature
  #
  # Args:
  #   data: a list of data frames, each data frame representing a file
  #
  # Returns:
  #   a vector, each entry being the number of levels of the corresponding field
  num_field <- compute_num_field(data)
  num_file <- compute_num_file(data)
  m_l_vec <- vector(mode = "numeric", length = num_field)
  for (iter_field in 1:num_field) {
    all_field_vals <- vector(mode = "numeric", length = 0)
    for (iter_file in 1:num_file) {
      all_field_vals <- c(all_field_vals, data[[iter_file]][, iter_field])
    }
    m_l_vec[iter_field] <- length(unique(all_field_vals))
  }
  return(m_l_vec)
}

compute_levels_list_of_vec <- function(data) {
  # Computes unique field values for each field
  # 
  # Args:
  #   data: a list of data frames, each data frame representing a file
  #
  # Returns:
  #   a list of vectors, each vector contains unique field values of the corresponding field
  num_field <- compute_num_field(data)
  num_file <- compute_num_file(data)
  levels_list_of_vec <- list()
  for (iter_field in 1:num_field) {
    all_field_vals <- vector(mode = "numeric", length = 0)
    for (iter_file in 1:num_file) {
      all_field_vals <- c(all_field_vals, data[[iter_file]][, iter_field])
    }
    levels_list_of_vec[[iter_field]] <- sort(unique(all_field_vals))
  }
  return(levels_list_of_vec)
}


