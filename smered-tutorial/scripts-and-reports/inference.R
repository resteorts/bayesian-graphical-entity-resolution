###################################################
# Inference and summary of Gibbs sampler's output #
###################################################

library(blink) # record linkage package

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
# burn_in: number of iteration to discard
burn_in <- nrow(res_Lambda) - 1000
# Discard the first half, and change variable name to estLink
estLink <- res_Lambda[burn_in:nrow(res_Lambda), ]
# Save results with burn-in removed
write.table(estLink, "server-version/results/tail.txt", row.names = FALSE, col.names = FALSE)

estLink <- as.matrix(estLink)

# Groundtruth---------------------------------------------------------------------------------------
truth <- c(D[[1]]$V1, D[[2]]$V1, D[[3]]$V1)
truth <- matrix(truth, nrow = 1)
true_pop_size <- length(unique(truth[1, ]))

# Estimated population size--------------------------------------------------------------------------
estPopSize <- apply(estLink , 1, function(x) {length(unique(x))})
# Plot of estimated population size
graphics.off()
jpeg(filename = "figures/obs-population-size.jpeg")
plot(density(estPopSize), main="",lty=1, "Observed Population Size", xlim = c(200, 1000),ylim= c(0,1))
abline(v=true_pop_size,col="red")
abline(v=mean(estPopSize),col="black",lty=2)
dev.off()

# Inference----------------------------------------------------------------------------------------
# calculates estimated pairwise links
est.links <- links(estLink, include.singles = TRUE)
est.links.pair <- pairwise(est.links)

# Some trying
est.links.pair.new <- list()
for (i in 1:(length(est.links.pair)/3)) {
  est.links.pair.new[[i]] <- est.links.pair[[3*i - 1]]
}
est.links.pair <- est.links.pair.new

# calculates true pairwise links
true.links <- links(truth)
true.links.pair <- pairwise(true.links)

not.singles <- vector(mode = "numeric", length = 0)
for (i in 1:length(true.links)) {
  not.singles <- c(not.singles, true.links[[i]])
}

# Some trying
true.links.new <- links(truth, include.singles = TRUE)
true.links.pair.new <- pairwise(true.links.new)
# remove NA, length 1, null values
true.links.pair.cleaned <- list()
for (i in 1:length(true.links.pair.new)) {
  NA_not_in_ele <- ! NA %in% true.links.pair.new[[i]]
  length_not_one <- length(true.links.pair.new[[i]]) != 1
  is_not_null <- !is.null(true.links.pair.new[[i]])
  if(NA_not_in_ele & length_not_one & is_not_null){
    true.links.pair.cleaned[[i]] <- true.links.pair.new[[i]]
  }
}
# remove possible false singletons
true.links.pair.cleaned.2 <- list()
for (i in 1:length(true.links.pair.cleaned)) {
  if(!is.null(true.links.pair.cleaned[[i]])){
    if( !(true.links.pair.cleaned[[i]][1] == true.links.pair.cleaned[[i]][2] & (true.links.pair.cleaned[[i]][1] %in% not.singles)) ){
      print(paste("i = ", i))
      true.links.pair.cleaned.2[[i]] <- true.links.pair.cleaned[[i]]
    }
  }
}
# There were acutally no false singletons
for (i in 1:length(true.links.pair.cleaned)) {
  count <- 0
  if(!is.null(true.links.pair.cleaned[[i]])){
    if(true.links.pair.cleaned[[i]][1] == true.links.pair.cleaned[[i]][2] & (true.links.pair.cleaned[[i]][1] %in% not.singles)){
      count <- count + 1
    }
  }
}
# check duplicate elements in true.links.pair.cleaned.2
true.links.pair.cleaned.2.matrix <- matrix(data = NA, nrow = length(true.links.pair.cleaned.2), ncol = 2)
for (i in 1:length(true.links.pair.cleaned.2)) {
  if(!is.null(true.links.pair.cleaned.2[[i]])){
    print(paste("i = ", i))
    true.links.pair.cleaned.2.matrix[i, ] <- true.links.pair.cleaned.2[[i]]
  }
}
# remve NA's
true.links.pair.cleaned.2.matrix <- remove.na(true.links.pair.cleaned.2.matrix)
# check duplicate elements
true.links.pair.cleaned.2.matrix.sorted <- true.links.pair.cleaned.2.matrix
for (i in 1:nrow(true.links.pair.cleaned.2.matrix)) {
  true.links.pair.cleaned.2.matrix.sorted[i, ] <- sort(true.links.pair.cleaned.2.matrix[i, na.last = TRUE])
}

true.links.pair <- true.links.pair.cleaned.2

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









