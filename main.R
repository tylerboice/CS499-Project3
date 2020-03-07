# install packages if it is not already
if(!require("data.table"))
{
	install.packages("data.table")
}
if(!require("ggplot2"))
{
  install.packages("ggplot2")
}
if(!require("caTools"))
{
  install.packages("caTools")
}

# attach all functions provided by these packages
library(data.table)
library(ggplot2)
library(caTools)

# source NNetOneSplit function
source('NNetOneSplit.R')

#download spam data set to local directory, if it is not present
if(!file.exists("spam.data"))
{
	download.file("https://web.stanford.edu/~hastie/ElemStatLearn/datasets/spam.data", "spam.data")
}

# Read spam data set and convert to input matrix
spam.dt <- data.table::fread("spam.data")
N.obs <- nrow(spam.dt)
X.raw <- as.matrix(spam.dt[, -ncol(spam.dt), with=FALSE])
y.vec <- spam.dt[[ncol(spam.dt)]]
X.mat <- scale(X.raw)

max.epochs <- 10
step.size <- 0.05
n.hidden.units <- c(ncol(X.mat), 20, 1)

# Create vector with size = num observations in the whole data set
# elements are TRUE if observation is in train set, FALSE otherwise. Should be 80T-20F
# is.train contains rows which are in the train set. All other rows not in is.train are in the test set
is.train <- sample.int(n = nrow(X.mat), size = floor(.8*nrow(X.mat)), replace = F)

# create vector with size = num observations in the whole data set
# elements are TRUE if observation is in train set, FALSE otherwise. Should be 60T-40F 
# is.subtrain contains rows which are in the train set. All other rows not in is.subtrain are in test set
is.subtrain <- sample.int(n = nrow(X.mat), size = floor(.6*nrow(X.mat)), replace = F)

# Call NNetOne with x.mat, y.vec, with is.subtrain, and large max.epochs
stuff <- NNetOneSplit(X.mat, y.vec, max.epochs, step.size, n.hidden.units, is.subtrain)

# Plot subtrain/validation loss as fxn of num epochs, draw point to emphasize min validation loss
#loss.dt <- do.call(rbind, loss.dt.list)
#ggplot()+
#geom_line(aes(
#		x=epoch, y=mean.loss, color=set),
#		data=loss.dt)

# best_epochs = epochs to minimize validation loss
best.epochs <- 10

# Call NNetONe with x.mat =, y.vec, is.subtrain = TRUE for all, max.epochs = best_epochs
stuff <- NNetOneSplit(X.mat, y.vec, best.epochs, step.size, n.hidden.units, is.subtrain)

# Use learned V.mat, w.vec, to make predictions on test set
#predictions <- ComputePredictions()

ComputePredictions <- function(X_train, y_train, X_new) class::knn(X_train, y_train, X_new)

