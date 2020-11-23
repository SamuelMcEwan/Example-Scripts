# My typical toolkit of packages
packages <- c("xlsx", "exact2x2", "DescTools", "rcompanion", "Rmisc", "ggplot2", "lmerTest", "magrittr", "dplyr", "tidyr", "foreign", "stringr")

# Check these Packages are installed. Will report the names of the packages not installed or "character(0)" if all packages are installed
packages[!packages %in% row.names(installed.packages())]

# Load each package
invisible(lapply(packages, library, character.only = TRUE))

# Clear the environment 
rm(list = ls())

# Source another script containing various useful functions. "Useful_Functions" is a placeholder name here and does not refer to any scripts in the repository
source("Useful_Functions.R")
