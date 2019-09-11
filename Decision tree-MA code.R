# Performance Evaluation Function 
perf_eval <- function(cm){
  
  # True positive rate: TPR (Recall)
  TPR <- cm[2,2]/sum(cm[2,])
  # Precision
  PRE <- cm[2,2]/sum(cm[,2])
  # True negative rate: TNR
  TNR <- cm[1,1]/sum(cm[1,])
  # Simple Accuracy
  ACC <- (cm[1,1]+cm[2,2])/sum(cm)
  # Balanced Correction Rate
  BCR <- sqrt(TPR*TNR)
  # F1-Measure
  F1 <- 2*TPR*PRE/(TPR+PRE)
  
  return(c(TPR, PRE, TNR, ACC, BCR, F1))
}

# Performance table
Perf.Table <- matrix(0, nrow = 6, ncol = 6)
rownames(Perf.Table) <- c("tree.full","tree.prune","rpart.full","rpart.gini","rpart.info","party")
colnames(Perf.Table) <- c("TPR", "Precision", "TNR", "Accuracy", "BCR", "F1-Measure")

# Load the data & Preprocessing
data <- read.csv("heart.csv")
input.idx <- c(1,2,3,4,5,6,7,8,9,10,11,12,13)
target.idx <- 14

data.input <- data[,input.idx]
data.target <- as.factor(data[,target.idx])

heart.data <- data.frame(data.input, data.target)

# Training set, Validation set 나누기

set.seed(1234)
idx=sample(1:nrow(heart.data),0.663*nrow(heart.data))
train = heart.data[idx,]
val = heart.data[-idx,]

# Classification Tree -------------------------------
library(tree)

# Training the tree
CART.model <- tree(train$data.target ~ ., train)
summary(CART.model)

# Plot the tree
plot(CART.model)
text(CART.model, pretty = 1)

# Prediction
CART.prey.full <- predict(CART.model, val, type = "class")
CART.cfm.full <- table(val$data.target, CART.prey.full)
CART.cfm.full

Perf.Table[1,] <- perf_eval(CART.cfm.full)
Perf.Table

# Find the best tree
set.seed(12345)
CART.model.cv <- cv.tree(CART.model, FUN = prune.misclass)

# Plot the pruning result
plot(CART.model.cv$size, CART.model.cv$dev, type = "b")
CART.model.cv

# Select the final model
CART.model.pruned <- prune.misclass(CART.model, best = 9)
plot(CART.model.pruned)
text(CART.model.pruned, pretty = 1)
summary(CART.model.pruned)

# Prediction
CART.prey <- predict(CART.model.pruned, val, type = "class")
CART.cfm <- table(val$data.target, CART.prey)
CART.cfm

Perf.Table[2,] <- perf_eval(CART.cfm)
Perf.Table
  
#다른 패키지 사용
#rpart
library(rpart)

rpart.model <- rpart(data.target ~. , data=train, method="class")
plot(rpart.model)
text(rpart.model)


# Prediction
rpart.prey <- predict(rpart.model, val, type = "class")
rpart.cfm <- table(val$data.target, rpart.prey)
rpart.cfm

Perf.Table[3,] <- perf_eval(rpart.cfm)
Perf.Table

# pruning
printcp(rpart.model)
plotcp(rpart.model)
ptree<-prune(rpart.model, cp= rpart.model$cptable[which.min(rpart.model$cptable[,"xerror"]),"CP"])
plot(ptree)
text(ptree)

# Prediction
rpart.prey.prune <- predict(ptree, val, type = "class")
rpart.cfm.prune <- table(val$data.target, rpart.prey.prune)
rpart.cfm.prune

Perf.Table[6,] <- perf_eval(rpart.cfm.prune)
Perf.Table


rpart_md_gini <- rpart(data.target~., data=train, parms = list(split = "gini"), method="class")
plot(rpart_md_gini)
text(rpart_md_gini, use.n= TRUE)
rpartpredgini <- predict(rpart_md_gini, val, type='class')
rpart.gini.cfm <- table(val$data.target, rpartpredgini)

Perf.Table[4,] <- perf_eval(rpart.gini.cfm)
Perf.Table

rpart_md_info <- rpart(data.target~., data=train, parms = list(split = "information"), method="class")
plot(rpart_md_info)
text(rpart_md_info, use.n= TRUE)
rpartpredinfo <- predict(rpart_md_info, val, type='class')
rpart.info.cfm <- table(val$data.target, rpartpredinfo)

Perf.Table[5,] <- perf_eval(rpart.info.cfm)
Perf.Table

#party
library(party)
party.model <- ctree(data.target ~. , data=train)
plot(party.model)

# Prediction
party.prey <- predict(party.model, val)
party.cfm <- table(val$data.target, party.prey)
party.cfm

Perf.Table[6,] <- perf_eval(party.cfm)
Perf.Table

library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(rpart_md_info)

  