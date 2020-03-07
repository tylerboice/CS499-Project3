NNetOneSplit <-function(X.mat, y.vec, max.epochs, step.size, n.hidden.units, is.subtrain)
{
  n.folds <- 5
  set.seed(1)
  fold.vec <- sample(rep(1:n.folds, l=length(y.vec)))
  
  # lines 7-11 were copied from Hocking's R script. I believe these are false, and should be using is.subtrain
  validation.fold <- 1
  is.validation <- fold.vec == validation.fold
  is.train <- !is.validation
  table(is.train)  

  # is.subtrain should be defining y.train and X.train
  yt.train <- y.vec[is.train]
  X.train <- X.mat[is.train,]
  loss.dt.list <- list()
  
  set.seed(1)
  weight.mat.list <- list()
  for(layer.i in 1:(length(n.hidden.units)-1)){
    mat.nrow <- n.hidden.units[[layer.i+1]]
    mat.ncol <- n.hidden.units[[layer.i]]
    weight.mat.list[[layer.i]] <- matrix(
      rnorm(mat.nrow*mat.ncol), mat.nrow, mat.ncol)
  }
  
  ##create a for loop over epochs
  # Set returned values
  for(epoch in 0:max.epochs)
  {
  	epoch
    
  }
  return(list(loss.values, v.mat, w.vec))
}

LogLoss <- function(prediction, label)
{
  log(1+exp(-label*prediction))
}
