#################################################
# Traceplot and running averages plot of Lambda #
#################################################

# Read arguments from command line--------------------------------------------------------------------
# file_name_Lambda, folder_name Lambda
args <- commandArgs(trailingOnly = TRUE)

# file name of the file of Lambdas to be read in------------------------------------------------------
file_name_Lambda <- args[1] # "results/Lambda-20190819-211151.txt"
folder_name_Lambda <- args[2] # "Lambda-20190819-211151"

# Read in Lambda values from results folder-----------------------------------------------------------
res_Lambda <- read.table(file = file_name_Lambda, header = FALSE)
# burn_in: number of iteration to discard
burn_in <- 0
# Discard the first half
res_Lambda <- res_Lambda[burn_in:nrow(res_Lambda), ]

# Create folders to save plots in---------------------------------------------------------------------
# Create a foldder in "results/diagnostics/" with the same name as the Lambda file we read in
folder_path_Lambda <- paste("results/diagnostics/", folder_name_Lambda, sep = "")
dir.create(folder_path_Lambda)
# Create two subfolders named "trace" and "run_avg" in the folder we just created
folder_path_trace <- paste(folder_path_Lambda, "/trace", sep = "")
dir.create(folder_path_trace)
folder_path_run_avg <- paste(folder_path_Lambda, "/run_avg", sep = "")
dir.create(folder_path_run_avg)

# Make traceplots-----------------------------------------------------------------------------------------

# Clear all existing graphics deivices to provide a clean environment for plotting
graphics.off()

# Create and save traceplots
for (iter in 1:ncol(res_Lambda)) {
  plot_name <- paste(iter, "_trace", ".jpeg", sep = "")
  plot_path <- paste(folder_path_trace, "/", plot_name, sep = "")
  
  jpeg(filename = plot_path)
  
  x.lab <- bquote(.(iter))
  plot(x = 1:nrow(res_Lambda), y = res_Lambda[, iter], xlim = c(0, nrow(res_Lambda)), type = "l", cex = 0.5,
       xlab = "iteration", ylab = bquote(paste("Traceplot of ", .(x.lab))),
       main = bquote(paste("Traceplot plot of ", .(x.lab))))
  
  dev.off()
}

# Make running averages plots-----------------------------------------------------------------------------------------

# Clear again all existing graphics deivices to provide a clean environment for plotting
graphics.off()

# get running averages of Lambdas
run.avg <- apply(res_Lambda, MARGIN = 2,FUN = function(x){
  cumsum(x)
}) / (1:nrow(res_Lambda))

# Create and save running averages plots
for (iter in 1:dim(run.avg)[2]) {
  plot_name <- paste(iter, "_run_avg", ".jpeg", sep = "")
  plot_path <- paste(folder_path_run_avg, "/", plot_name, sep = "")
  
  jpeg(filename = plot_path)
  
  x.lab <- bquote(.(iter))
  plot(1:nrow(res_Lambda), run.avg[, iter], type = "l", cex = 0.5, xlim = c(0, nrow(res_Lambda)),
       xlab = "iteration", ylab = bquote(paste("running average of ", .(x.lab))),
       main = bquote(paste("Running average plot of ", .(x.lab))))
  
  dev.off()
}

# Cleanup graphics devices
graphics.off()


