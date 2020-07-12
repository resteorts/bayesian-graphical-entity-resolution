# Summary statistics of gibbs sampler's output, with removing duplicate entries in y-------------------------------

library(blink) # record linkage package

estLink <- read.table("results/Lambda-20190701-041230.txt", header = FALSE)
estLink <- as.matrix(estLink)
dim(estLink)
estPopSize <- apply(estLink , 1, function(x) {length(unique(x))})
plot(density(estPopSize), main="",lty=1, "Observed Population Size", xlim = c(200, 900),ylim= c(0,1))
abline(v=727,col="red")
abline(v=mean(estPopSize),col="black",lty=2)
mean(estPopSize)
sd(estPopSize)
# let's calculated the estimated pairwise links
# using the blink method 
est.links.pair <- links(estLink)

# I think we would need to apply pairwise to links(estLink)
# Regarding potential bugs in sampling: samling lambda's (the last full conditionals)
# might be problematic

# let's calulated the true links using the 
# unique identifiers that are stored
true.links <- links(matrix(truth,nrow=1))
true.links.pair <- pairwise(true.links)

comparison <- links.compare(est.links.pair,true.links.pair,counts.only=TRUE)



# Summary statistics of gibbs sampler's output, WITHOUT removing duplicate entries in y-------------------------------



