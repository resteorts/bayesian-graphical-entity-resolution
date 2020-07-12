# Installs packages needed for the rest of analysis

.libPaths(c("~/.rlib", .libPaths()))

list_of_packages <- c("dplyr", "igraph", "Rlab", "blink")
packages_needs_installing <- list_of_packages[!(list_of_packages %in% installed.packages()[, "Package"])]
if (length(packages_needs_installing) > 0) {
  install.packages(packages_needs_installing, repos = "http://cran.us.r-project.org")
}
