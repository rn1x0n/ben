rm(list = ls())
# Change this path to the location of this file
root <- "/Users/Richard/Documents/R/ben"

# Set up global variables
gv <- list()
gv$root <- root
gv$data <- file.path(gv$root, "data")
gv$functions <- file.path(gv$root, "functions")
gv$scripts <- file.path(gv$root, "scripts")

# Install packages if needed then load
pkg.needed <- c("psych", "ggplot2", "GGally",
                "grid",
                "reshape2",
                "plyr",
                "xtable",
                "knitr")

pkgs <- installed.packages()[,"Package"]

for(pkg in pkg.needed){
if(is.na(match(pkg, pkgs))) install.packages(pkg)
library(pkg, character.only = TRUE)
}

# Source functions
for(file in list.files(gv$functions)){source(file.path(gv$functions, file))}
