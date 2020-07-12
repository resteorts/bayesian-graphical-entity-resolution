set.seed(42) # set random state
file_names <- c("data/raw-data/proc_nltcs_82.txt", "data/raw-data/proc_nltcs_89.txt", "data/raw-data/proc_nltcs_94.txt") # paths of files to read in
D <- lapply(file_names, function(files){
  read.table(files, header = FALSE)
}) # load data
source("scripts-and-reports/functions_data_attribute.R")
nv <- compute_file_lengths_vec(D) # vector of the length of each file
id_data <- c(D[[1]]$V1, D[[2]]$V1, D[[3]]$V1) # unique identifiers
n_occur <- data.frame(table(id_data)) # occurance of each unique individual in observations
duplicate_id_data <- n_occur[n_occur$Freq > 1,]$id_data # Find id of duplicate entries
sample_duplicate_id_data <- sample(duplicate_id_data, size = 0.01*length(duplicate_id_data)) # randomly sample a portion of duplicate id
index_duplicate_sample_in_data <- which(id_data %in% sample_duplicate_id_data) # locate indices of the sample of duplicate entries in id_data
unique_id_data <- n_occur[n_occur$Freq == 1,]$id_data # Find id of unique entries
sample_unique_id_data <- sample(unique_id_data, size = 0.03*length(unique_id_data)) # randomly sample a portion of unique id
index_unique_sample_in_data <- which(id_data %in% sample_unique_id_data) # locate indices of sample of unique entries in id_data
index_sample_data <- c(index_duplicate_sample_in_data, index_unique_sample_in_data) # merge indices of sample of duplicate entries in id_data and indices of sample of unique entries in id_data
# create a sample of original dataset-----------------------------------------------------------------------------------------------------
D[[1]] <- D[[1]][index_sample_data[index_sample_data <= nv[1]],]
D[[2]] <- D[[2]][index_sample_data[index_sample_data > nv[1] & index_sample_data <= (nv[1] + nv[2])] - nv[1],]
D[[3]] <- D[[3]][index_sample_data[index_sample_data > (nv[1] + nv[2]) & index_sample_data <= (nv[1] + nv[2] + nv[3])] - (nv[1] + nv[2]),]
D <- lapply(D, function(x){
  x[, c(2:ncol(x), 1 ) ]
}) # move id numbers to last column for future use
# save data into a file-------------------------------------------------------------------------------------------------------------------
saveRDS(D, file = "data/processed-data/processed_nltcs_sampled.rds")