set.seed(42) # set random state

# Read arguments from command line--------------------------------------------------------------------
# file name for Lambdas
# args <- commandArgs(trailingOnly = TRUE)

# File name of the file of Lambdas to be read in------------------------------------------------------
# file_name_Lambda <- args[1] # "results/Lambda-20190819-211151.txt"
file_name_Lambda <- "server-version/results/Lambda-20190822-211417.txt"
# Load data-------------------------------------------------------------------------------------------
D <- readRDS(file = "data/processed-data/processed_nltcs_sampled.rds")

# Read in Lambda values from results folder-----------------------------------------------------------
res_Lambda <- read.table(file = file_name_Lambda, header = FALSE)

# Groundtruth---------------------------------------------------------------------------------------
truth <- c(D[[1]]$V1, D[[2]]$V1, D[[3]]$V1)
truth <- matrix(truth, nrow = 1, ncol = length(truth))
true_pop_size <- length(unique(truth))

# Estimated population size--------------------------------------------------------------------------

estLink <- res_Lambda
estPopSize <- apply(estLink , 1, function(x) {length(unique(x))})

# Graph of estimated population size vs iteration----------------------------------------------------

graphics.off()
jpeg(filename = "figures/obs-entity-vs-iter.jpeg")
plot(x = 1:length(estPopSize), y = estPopSize, type = "l", ylim = c(600, 1000), xlab = "iteration", ylab = "estimated population size")
abline(h=true_pop_size,col="red")
dev.off()




