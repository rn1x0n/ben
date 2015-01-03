# Set the working director to the location of this file
setwd("~/Documents/R/ben")

# Set up paths and load libraries
source("_setup.R")

# Make R data set, and save to /data/data2.dput
source(file.path(gv$scripts, "data_setup2.R"))

# Run scripts
setwd(gv$scripts)
knit("plots2.Rmd")

unlink(file.path(gv$root, "figure"), recursive = TRUE)
unlink(file.path(gv$scripts, "figure"), recursive = TRUE)
unlink(file.path(gv$root, "*.md"))
unlink(file.path(gv$scripts, "*.md"))
