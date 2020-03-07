library(ggplot2)
library(data.table)
spam.dt <- data.table::fread("spam.data")
label.col.i <- ncol(spam.dt)
X.mat <- as.matrix(spam.dt[, -label.col.i, with=FALSE])
yt.vec <- ifelse(spam.dt[[label.col.i]]==1, 1, -1)
X.sc <- scale(X.mat)

NNetOneSplit <-function(X.mat, y.vec, max.epochs, step.size, n.hidden.units, is.subtrain)
{
  X.mat <- as.matrix(spam.dt[, -label.col.i, with=FALSE])
  X.sc <- scale(X.mat)
  y.vec <- ifelse(spam.dt[[label.col.i]]==1, 1, -1)
  max.epochs <- 1
  step.size <- 0.05
  n.hidden.units <- c(ncol(X.sc), 20, 1)
  n.folds <- 5
  set.seed(1)
  fold.vec <- sample(rep(1:n.folds, l=length(yt.vec)))
  
  validation.fold <- 1
  is.validation <- fold.vec == validation.fold
  is.train <- !is.validation
  table(is.train)  

  epoch <- 0
  yt.train <- yt.vec[is.train]
  X.train <- X.sc[is.train,]
  loss.dt.list <- list()
  
  set.seed(1)
  weight.mat.list <- list()
  for(layer.i in 1:(length(n.hidden.units)-1)){
    mat.nrow <- n.hidden.units[[layer.i+1]]
    mat.ncol <- n.hidden.units[[layer.i]]
    weight.mat.list[[layer.i]] <- matrix(
      rnorm(mat.nrow*mat.ncol), mat.nrow, mat.ncol)
  }
  str(weight.mat.list)
  ##create a for loop over epochs
}

LogLoss <- function(prediction, label)
{
  log(1+exp(-label*prediction))
}
