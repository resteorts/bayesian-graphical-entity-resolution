#######################################################
# Obtains relevant information from cleaned data,     #
# set hyperparamaters, and                            #
# initialize variables to be updated in gibbs sampler #
# Saves what's needed in Gibbs sampler                #
#######################################################

library(dplyr) # package for data manipulation

set.seed(42) # set random state

# source functions needed in this script-------------------------------------------------------------------------
source("scripts-and-reports/functions_data_attribute.R")

# Load data---------------------------------------------------------------------------------------------
D <- readRDS(file = "data/processed-data/processed_nltcs_sampled.rds")



# Obtain information from data--------------------------------------------------------------------------
num_file <- compute_num_file(D) # number of files
nv <- compute_file_lengths_vec(D) # vector of the length of each file
nt <- compute_total_num_obs(D) # total number of observations
num_field <- compute_num_field(D) # number of fields
m_l_vec <- compute_m_l_vec(D) # vector of number of levels for each cateogry
levels_list_of_vec <- compute_levels_list_of_vec(D) # list of vectors of unique field values of a field
id_data <- c(D[[1]]$V1, D[[2]]$V1, D[[3]]$V1) # unique identifiers
num_population <- compute_num_population(id_data) # size of the population



# Hyperparameters---------------------------------------------------------------------------------------
a_vec <- rep(1, num_field)
b_vec <- rep(99, num_field)

mu_list_of_vec <- list()
for (iter_field in 1:num_field) {
  mu_list_of_vec[[iter_field]] <- rep(x = 1/m_l_vec[iter_field], times = m_l_vec[iter_field])
}



# Initialization----------------------------------------------------------------------------------------
# Initialization of variables to be updated in the Gibbs sampler
# We call these variables gibbs variables

# intialize beta vector
beta_vec <- vector(mode = "numeric", length = num_field)
for (iter in 1:length(beta_vec)) {
  beta_vec[iter] <- rbeta(1, shape1 = a_vec[iter], shape2 = b_vec[iter])
}

# intialize list of theta vectors
theta_list_of_vec <- list()
for (iter_field in 1:num_field) {
  theta_list_of_vec[[iter_field]] <- rep(x = 1/m_l_vec[iter_field], times = m_l_vec[iter_field])
}

# initialize y matrix
y_matrix <- matrix(data = NA, nrow = nt, ncol = num_field)
j_prime <- 1
for (i in 1:length(nv)){
  for (j in 1:nv[i]) {
    for (l in 1:num_field) {
      y_matrix[j_prime, l] <- D[[i]][j,l]
    }
    j_prime <- j_prime + 1
  }
}

# initialize z, list of matrices
z_list_of_mat <- list()
for (iter in 1:num_file) {
  z_list_of_mat[[iter]] <- matrix(data = NA, nrow = nv[iter], ncol = num_field)
  z_list_of_mat[[iter]] <- apply(z_list_of_mat[[iter]], c(1, 2),function(x){
    sample(c(0,1), size = 1)
  })
}

# initialize Lambda, list of vectors
Lambda_list_of_vec <- list()
for (iter in 1:num_file) {
  Lambda_list_of_vec[[iter]] <- vector(mode="numeric", length = nv[iter])
  Lambda_list_of_vec[[iter]] <- sapply(Lambda_list_of_vec[[iter]], function(x){
    sample(seq(1:nt), size = 1)
  })
}



# Save all that's needed for Gibbs sampler----------------------------------------------------------------
# Into a file specified for initial variables

outfilename_init_vars <- "results/init-vars.RData"
save(a_vec, b_vec, mu_list_of_vec,
     beta_vec, theta_list_of_vec, y_matrix, z_list_of_mat, Lambda_list_of_vec, 
     nt, nv, num_field, num_file, levels_list_of_vec,
     file = outfilename_init_vars)



