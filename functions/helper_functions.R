test.med <- function(
  dependent,
  independent,
  mediator,
  covar = NULL, # Other covariates to include in the model
  data
  ){
  
#  Build three models
#  Test "independent" is significant in mod1  dependent ~ independent (+ covar)
#  Test "independent" is significant in mod2  mediator ~ independent (+ covar)
#  Test "mediator" is significant in mod3  dependent ~ independent + mediator (+ covar)
  
covar.formula <- ifelse(
  is.null(covar),
  "",
  paste( "+", paste(covar, collapse = " + "))
)

mod1 <- lm(as.formula(paste(
  dependent, "~", independent, covar.formula)), 
  data = data)

mod2 <- lm(as.formula(paste(
  mediator, "~", independent, covar.formula)), 
  data = data)

mod3 <- lm(as.formula(paste(
  dependent, "~", independent, "+", mediator, covar.formula)), 
  data = data)

test <- list()
test[[1]] <- list()
test[[1]]$Model       <- paste(dependent, "~", independent)
test[[1]]$Coefficient <- independent
test[[1]]$Stats       <- data.frame(t(summary(mod1)$coeff[independent,]))
test[[1]]$Test        <- c(ifelse(test[[1]]$Stats[4] < 0.05, "PASSED", "FAILED"))

test[[2]] <- list()
test[[2]]$Model       <- paste(mediator, "~", independent)
test[[2]]$Coefficient <- independent
test[[2]]$Stats       <- data.frame(t(summary(mod2)$coeff[independent,]))
test[[2]]$Test        <- c(ifelse(test[[2]]$Stats[4] < 0.05, "PASSED", "FAILED"))

test[[3]] <- list()
test[[3]]$Model       <- paste(dependent, "~", independent, "+", mediator)
test[[3]]$Coefficient <- mediator
test[[3]]$Stats       <- data.frame(t(summary(mod3)$coeff[mediator,]))
test[[3]]$Test        <- c(ifelse(test[[3]]$Stats[4] < 0.05, "PASSED", "FAILED"))

test[[4]] <- list()
test[[4]]$Model       <- paste(dependent, "~", independent, "+", mediator)
test[[4]]$Coefficient <- independent
test[[4]]$Stats       <- data.frame(t(summary(mod3)$coeff[independent,]))
if(all(test[[1]]$Test == "PASSED",
       test[[2]]$Test == "PASSED",
       test[[3]]$Test == "PASSED"
       )){
  test[[4]]$Test        <- c(ifelse(test[[4]]$Stats[4] < 0.05, "COMPLETE", "PARTIAL"))
  
} else {
  test[[4]]$Test <- "NA"
}

results <- rbind(
data.frame(test[[1]]),
data.frame(test[[2]]),
data.frame(test[[3]]),
data.frame(test[[4]])
)

names(results)[3:6] <- colnames(summary(mod1)$coeff)

return(results)

}


print.test.med <- function(
  results,
  digits = 3){
  
  print(xtable(results, align = c("l", "l", "l", "r", "r", "r", "r", "l"), digits = digits),
        type = "html",
        include.rownames = FALSE)
  
}

#####################################################################
ggplotRegression <- function (mod) {
  ggplot(mod$model, aes_string(x = names(mod$model)[2], y = names(mod$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    xlab(labels[names(mod$model)[2]]) + ylab(labels[names(mod$model)[1]])
}

#####################################################################
ggplotCtsInteraction <- function (mod, num.levels = 4, modify.vals = NULL, alpha = 0.15, size = TRUE) {
  # Levels of the modifing variable and pred var range
  if(is.null(modify.vals)){
    #prob <- seq(1/(num.levels+1),num.levels/(num.levels+1), length = num.levels)  #seq(0, 1, length = num.levels)
    prob <- seq(0, 1, length = num.levels)
    modify.vals <- quantile(mod$model[,3], prob = prob)
  }
  x.vals <- seq(min(mod$model[,2]), max(mod$model[,2]), length = 100)
  
  # Make prediction
  line.data <- expand.grid(x.vals, modify.vals)
  names(line.data) <- names(mod$model)[2:3]
  
  if(ncol(mod$model)-3 > 0){
  base.val <- data.frame(matrix(NA, nrow = nrow(line.data), ncol = ncol(mod$model)-3,
                                dimnames = list(NULL, names(mod$model[,4:ncol(mod$model)]))))

  for(i in colnames(base.val)){
    base.val[,i] <- mod$xlevels[[i]][1]   
  }
  line.data <- cbind(line.data, base.val)
  }
  
  pred <- predict(mod, line.data, interval = "confidence", level = 0.8)
  
  # Add pred to lines data
  line.data <- cbind(line.data, pred)
  line.data[,2] <- as.factor(line.data[,2])
  
  # Make plot
  p <- ggplot(mod$model, aes_string(x = names(mod$model)[2])) + 
    geom_point(aes_string(y = names(mod$model)[1])) +
    geom_path(aes_string(y = "fit", colour = names(mod$model)[3]), data = line.data, size = 1) +
    geom_ribbon(aes_string(ymax = "upr", ymin = "lwr", fill = names(mod$model)[3]), 
                alpha = alpha, data = line.data) +
    xlab(labels[names(mod$model)[2]]) + ylab(labels[names(mod$model)[1]]) +
    scale_colour_discrete(name = labels[names(mod$model)[3]]) +
    guides(fill = FALSE) 
 
  if(size){
  p <- p + 
  geom_point(aes_string(y = names(mod$model)[1], size = names(mod$model)[3])) +
    scale_size_continuous(
      name = labels[names(mod$model)[3]],
      breaks = as.numeric(modify.vals))
  }
    
  return(p)
}
#####################################################################

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
