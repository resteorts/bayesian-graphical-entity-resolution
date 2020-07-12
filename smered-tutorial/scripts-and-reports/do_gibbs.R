#####################
# Runs gibbs sampler#
#####################

library(igraph) # package supporting `sample_dirichlet` function
library(Rlab) # package supporting `rbern` function

set.seed(42) # set random state

# source functions needed in this script----------------------------------------------------------------
source("scripts-and-reports/function_sampleGibbs.R")
source("scripts-and-reports/functions_full_conditional.R")

# Read arguments from command line----------------------------------------------------------------------
# number of desired gibbs iterations, file name for gibbs variables to load
args <- commandArgs(trailingOnly = TRUE)

# Arguments for Gibbs sampler---------------------------------------------------------------------------
n.iter <- as.numeric(args[1])
gibbs_vars_filename <- args[2] # "results/init-vars.RData"
load(gibbs_vars_filename)

# Check if gibbs vars are successfully loaded----------------------------------------------------------
# print(paste("n.iter and nt", n.iter, nt))
# print(is.numeric(n.iter))
# print(is.numeric(nt))

# Load data---------------------------------------------------------------------------------------------
D <- readRDS(file = "data/processed-data/processed_nltcs_sampled.rds")

# Runs Gibbs sampler-----------------------------------------------------------------------------------
sampleGibbs(D = D, n.iter = n.iter, a_vec = a_vec, b_vec = b_vec, mu_list_of_vec = mu_list_of_vec,
            beta_vec = beta_vec, theta_list_of_vec = theta_list_of_vec,
            y_matrix = y_matrix, z_list_of_mat = z_list_of_mat, Lambda_list_of_vec = Lambda_list_of_vec, 
            nt = nt, nv = nv, num_field = num_field, num_file = num_file, levels_list_of_vec = levels_list_of_vec)

print("Just finished running Gibbs")
















