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
  yt.train <- y.vec[is.subtrain]
  X.train <- X.mat[is.subtrain]
  
  set.seed(1)
  weight.mat.list <- list()
  for(layer.i in 1:(length(n.hidden.units)-1)){
    mat.nrow <- n.hidden.units[[layer.i+1]]
    mat.ncol <- n.hidden.units[[layer.i]]
    weight.mat.list[[layer.i]] <- matrix(
      rnorm(mat.nrow*mat.ncol), mat.nrow, mat.ncol)
  }
  
  ##create a for loop over epochs
  for(layer.i in 1:(length(n.hidden.units)-1))
  {
    obs.i <- is.subtrain[[layer.i]]
    v <- X.train[obs.i]
    w <- yt.train[obs.i]
    w.list <- 1/(1+exp(-w*v))
    grad.list <- list()
    for(layer.i in length(weight.mat.list):1)
    {
      loss.values <- if(layer.i==length(weight.mat.list))
      {
        LogLoss(w.list, w)
      }
      else
      {
        # grad.w <- t(weight.mat.list[[layer.i+1]]) %*% loss.values
        grad.w <- t(weight.mat.list[[layer.i+1]]) * loss.values
        w.vec <- w.list[[layer.i]]
        grad.w * w.vec * (1-w.vec)
        grad.list[[layer.i]] <- loss.values %*% t(grad.w)
      }
      #grad.list[[layer.i]] <- loss.values %*% t(w.list[[layer.i]])
      
    }
    
    v.mat <- weight.mat.list
    for(epoch in 1:max.epochs)
    {
      # v.mat[[layer.i]] <- v.mat[[layer.i]] - step.size * grad.list[[layer.i]]
      current.v.mat <- v.mat[[layer.i]]
      for( row in nrow(v.mat[[layer.i]]) )
      {
      	current.v.mat <- v.mat[[layer.i]][row]
      	for( col in length(current.v.mat) )
      	{
      		replace <- current.v.mat[col] - step.size * grad.list[[layer.i]][row]
      		v.mat[[layer.i]][row][col] <- replace
      	}
      }
    }
  }
  return(list(loss.values, v.mat, w.vec, layer.i))
}

LogLoss <- function(prediction, label)
{
  log(1+exp(-label*prediction))
}
