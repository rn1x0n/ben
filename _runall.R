
#####################################################################
source("_setup.R")

# Run scripts

setwd(gv$scripts)
knit("plots.Rmd")

unlink("*.Rmd")
unlink("*.md")
unlink(file.path(gv$script, "figure"), recursive = TRUE)
