library("class")
library("caret")
library("MASS")
library("rpart")
library("rpart.plot")
library("rpart.utils")

## Read data from file
inputPath <- file.choose()
df <- read.csv(inputPath, header=TRUE, stringsAsFactors=FALSE)

## Combine wingers and centers to forwards
# df$Pos[df$Pos == "W" | df$Pos == "C"] <- "F"

## Normalize data to interval of [0, 1]
minmaxNormalize <- function(x, range=c(0, 1)) {
  return((x - min(x))/(max(x) - min(x)) * (range[2] - range[1]) + range[1])
}

df[, -ncol(df)] <- apply(df[, -ncol(df)], MARGIN=2, FUN=minmaxNormalize)

## features to be used
vars <- c("Pos", "G", "A", "PM", "PIM", "S", "ATOI", "BLK", "HIT", "FO")

## Divide data into training (90%) and testing (10%) sets
set.seed(2018)
dfParts <- createDataPartition(df$Pos, times=1, p=0.9, list=FALSE)
dfTrain <- df[dfParts, vars]
dfTest <- df[-dfParts, vars]

## k nearest neighbour (knn) function
knnFun <- function(train, test) {
  
  kvals <- seq(1, 25, 2) #  Different k values
  
  ## The first part is to find the best choice for k value from above
  ## alternatives by using classification accuracy
  knn <- list()
  knnConf <- list()
  knnAcc <- list()
  
  for (i in 1:length(kvals)) {
    set.seed(2018)
    knn[[i]] <- knn(train[, -1], test[, -1], cl=train[, 1], k=kvals[i])
    knnConf[[i]] <- table(test[, 1], knn[[i]]) # Confusion matrix
    knnAcc[[i]] <- 100*sum(diag(knnConf[[i]]))/sum(knnConf[[i]]) # Accuracy
  }
  
  ##  Pick the k value with highest accuracy
  k <- kvals[which.max(unlist(knnAcc))]
  
  ## Go through knn with the chosen k
  knnPred <- knn(train[, -1], test[, -1], train[, 1], k=k)
  knnConf <- table(test[, 1], knnPred)
  
  ## Compute the final classification accuracy and sensitivities
  knnAcc <- 100*sum(diag(knnConf))/sum(knnConf)
  knnSens <- c()
  
  for (i in 1:length(unique(train[, 1]))) {
    knnSens[i] <- 100*knnConf[i, i]/sum(knnConf[, i])
  }
  
  print(knnConf)
  print(paste("Accuracy for knn is:", round(knnAcc, digits=1), "%"))
  print(paste("Sensitivities for positions are:", round(knnSens, digits=1),"%"))

}


## Quadratic discriminant analysis (qda) function
qdaFun <- function(train, test) {
  
  qda <- qda(train[, -1], train[, 1], tol=1e-4, method="moment")
  qdaPred <- predict(qda, test[, -1])$class
  qdaConf <- table(test[, 1], qdaPred)
  
  ## Compute the final classification accuracy and sensitivities
  qdaAcc <- 100*sum(diag(qdaConf))/sum(qdaConf)
  qdaSens <- c()
  
  for (i in 1:length(unique(train[, 1]))) {
    qdaSens[i] <- 100*qdaConf[i, i]/sum(qdaConf[, i])
  }
  
  print(qdaConf)
  print(paste("Accuracy for qda is:", round(qdaAcc, digits=1), "%"))
  print(paste("Sensitivities for positions are:", round(qdaSens, digits=1),"%"))

}

## Decision tree (CART) function
cartFun <- function(train, test) {
  
  cart <- rpart(Pos~G+A+PM+PIM+S+ATOI+BLK+HIT+FO,
                data=train, method="class",
                parms=list(split="information"),
                control=rpart.control(minsplit=5, minbucket=1))

  par(mar=c(1, 1, 1, 1))
  plot(cart) #  Plot the decision tree
  text(cart) #  Add feature names

  cartPred <- predict(cart, test[, -1], type="class")
  cartConf <- table(test[, 1], cartPred)
  
  ## Compute the final classification accuracy and sensitivities
  cartAcc <- 100*sum(diag(cartConf))/sum(cartConf)
  cartSens <- c()
  
  for (i in 1:length(unique(train[, 1]))) {
    cartSens[i] <- 100*cartConf[i, i]/sum(cartConf[, i])
  }
  
  print(cartConf)
  print(paste("Accuracy for CART is:", round(cartAcc, digits=1), "%"))
  print(paste("Sensitivities for positions are:", round(cartSens, digits=1),"%"))

}

knnFun(dfTrain, dfTest)
qdaFun(dfTrain, dfTest)
cartFun(dfTrain, dfTest)