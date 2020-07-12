list_of_packages <- c("dplyr", "igraph", "Rlab", "blink")
packages_needs_installing <- list_of_packages[!(list_of_packages %in% installed.packages()[, "Package"])]
if (length(packages_needs_installing) > 0) {
   installed.packages(packages_needs_installing)
}

library(dplyr) # package for data manipulation
library(igraph) # package supporting `sample_dirichlet` function
library(Rlab) # package supporting `rbern` function
library(blink) # record linkage package

set.seed(42) # set random state

# source functions needed in this script-------------------------------------------------------------------------
source("scripts-and-reports/function_sampleGibbs.R")
source("scripts-and-reports/functions_data_attribute.R")
source("scripts-and-reports/functions_full_conditional.R")



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



# modifying and saving some variables for testing purposes
# D_test <- list()
# D_test[[1]] <- D[[1]][1:2,]
# levels_list_of_vec_test <- levels_list_of_vec
# num_field_test <- num_field
# a_vec_test <- a_vec
# b_vec_test <- b_vec
# mu_list_of_vec_test <- mu_list_of_vec
# beta_vec_test <- beta_vec
# theta_list_of_vec_test <- theta_list_of_vec
# y_matrix_test <- y_matrix[1:2,]
# z_list_of_mat_test <- list()
# z_list_of_mat_test[[1]] <- z_list_of_mat[[1]][1:2,]
# save(D_test,
#      levels_list_of_vec_test,
#      num_field_test,
#      a_vec_test,
#      b_vec_test,
#      mu_list_of_vec_test,
#      beta_vec_test,
#      theta_list_of_vec_test,
#      y_matrix_test,
#      z_list_of_mat_test,
#      file = "test/test_nltcs_two_points.RData"
#      )


# beka debug

lam.test <- sampleGibbs(D = D, n.iter = 5, a_vec = a_vec, b_vec = b_vec, mu_list_of_vec = mu_list_of_vec,
                        beta_vec = beta_vec, theta_list_of_vec = theta_list_of_vec,
                        y_matrix = y_matrix, z_list_of_mat = z_list_of_mat, Lambda_list_of_vec = Lambda_list_of_vec, 
                        nt = nt, nv = nv, num_field = num_field)



# Run Gibbs sampler--------------------------------------------------------------------------------------
# res_Lambda <- sampleGibbs(D, n.iter = 4000, a_vec, b_vec, mu_list_of_vec, beta_vec, theta_list_of_vec,
#                           y_matrix, z_list_of_mat, Lambda_list_of_vec, nt, nv, num_field)

# Let's run 2000 more iterations
# load("results/gibbs-var-20190630-200417.RData")
# res_Lambda_2 <- sampleGibbs(D, n.iter = 2000, a_vec, b_vec, mu_list_of_vec, beta_vec, theta_list_of_vec,
#                             y_matrix, z_list_of_mat, Lambda_list_of_vec, nt, nv, num_field)

# Let's run 1000 more iterations
# load("results/gibbs-var-20190701-012253.RData")
# res_Lambda_3 <- sampleGibbs(D, n.iter = 1000, a_vec, b_vec, mu_list_of_vec, beta_vec, theta_list_of_vec,
#                             y_matrix, z_list_of_mat, Lambda_list_of_vec, nt, nv, num_field)

# Running averages plot----------------------------------------------------------------------------------


# calculates shared most probable maximal matching set.
mos_prob_mms <- links(res_Lambda[(nrow(res_Lambda)/2):nrow(res_Lambda),])
mos_prob_mms_2 <- links(res_Lambda[(nrow(res_Lambda_2)/2):nrow(res_Lambda_2),])
mos_prob_mms_3 <- links(res_Lambda[(nrow(res_Lambda_3)/2):nrow(res_Lambda_3),])

truth <- c(D[[1]]$V1, D[[2]]$V1, D[[3]]$V1)

# get estimated pairs
est.links.pair <- pairwise(mos_prob_mms_3)
# get true pairs
true.links.pair <- pairwise(links(matrix(truth, nrow = 1)))
# compare estimated links and true links
links.compare(est.links.pair, true.links.pair)


