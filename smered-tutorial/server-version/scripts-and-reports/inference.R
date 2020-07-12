###################################################
# Inference and summary of Gibbs sampler's output #
###################################################

.libPaths(c("~/.rlib", .libPaths()))

library(blink) # record linkage package

set.seed(42) # set random state

# Read arguments from command line--------------------------------------------------------------------
# file name for Lambdas
args <- commandArgs(trailingOnly = TRUE)

# File name of the file of Lambdas to be read in------------------------------------------------------
file_name_Lambda <- args[1] # "results/Lambda-20190819-211151.txt"
# Load data-------------------------------------------------------------------------------------------
D <- readRDS(file = "data/processed-data/processed_nltcs_sampled.rds")

# Read in Lambda values from results folder-----------------------------------------------------------
res_Lambda <- read.table(file = file_name_Lambda, header = FALSE)
# burn_in: number of iteration to discard
burn_in <- nrow(res_Lambda) - 1000
# Discard the first half, and change variable name to estLink
estLink <- res_Lambda[burn_in:nrow(res_Lambda), ]

estLink <- as.matrix(estLink)

# Groundtruth---------------------------------------------------------------------------------------
truth <- c(D[[1]]$V1, D[[2]]$V1, D[[3]]$V1)
truth <- matrix(truth, nrow = 1, ncol = length(truth))
true_pop_size <- length(unique(truth))

# Estimated population size--------------------------------------------------------------------------
estPopSize <- apply(estLink , 1, function(x) {length(unique(x))})
# Plot of estimated population size
graphics.off()
jpeg(filename = "figures/obs-population-size.jpeg")
plot(density(estPopSize), main="",lty=1, "Observed Population Size", xlim = c(200, 900),ylim= c(0,1))
abline(v=true_pop_size,col="red")
abline(v=mean(estPopSize),col="black",lty=2)
dev.off()

# Inference----------------------------------------------------------------------------------------
# calculates estimated pairwise links
est.links <- links(estLink)
est.links.pair <- pairwise(est.links)
# calculates true pairwise links
true.links <- links(truth)
true.links.pair <- pairwise(true.links)
# Correct, incorrect, and missing links
comparison <- links.compare(est.links.pair,true.links.pair,counts.only=TRUE)
comparison

missing.links <- comparison$missing
true.links<-comparison$correct 
false.links <- comparison$incorrect

truth.links <- true.links+false.links
fpr = false.links/truth.links
fnr = missing.links/truth.links
fdr = false.links/(true.links+false.links)
c(fpr,fnr,fdr)









