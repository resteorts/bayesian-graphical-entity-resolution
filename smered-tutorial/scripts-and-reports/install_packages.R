# Installs packages needed for the rest of analysis

list_of_packages <- c("dplyr", "igraph", "Rlab", "blink")
packages_needs_installing <- list_of_packages[!(list_of_packages %in% installed.packages()[, "Package"])]
if (length(packages_needs_installing) > 0) {
  installed.packages(packages_needs_installing)
}