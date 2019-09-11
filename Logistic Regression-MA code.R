#데이터 불러오기
data <- read.csv("Admission_Predict.csv", header=TRUE)

#serial no는 불필요하므로 제거
data <- data[,-1]

# 통계량 계산 함수 정의
library(moments)
statistic <- function(variabl){
  
  m <- mean(variabl)
  std <- sqrt(var(variabl))
  skew <- skewness(variabl)
  kur <- kurtosis(variabl)
  return(c(m,std, skew, kur))
  
}

# research 변수는 binary이므로 통계량 계산에서 제외 + NA값 제거
data_1 <- data[,-7]
data_1 <- na.omit(data_1)

# 통계량 계산 값을 matrix에 담기
stat_mat <- matrix(0, nrow = 7, ncol = 4)

rownames(stat_mat) <- c("GRE.Score", "TOEFL.Score","University.Rating","SOP","LOR","CGPA","Chance.of.Admit")
colnames(stat_mat) <- c("mean", "std", "skewness","kurtosis")
stat_mat

for(i in 1:7){
  stat_mat[i,] <- statistic(data_1[,i])
}

# boxplot 도시하기
boxplot(data$GRE.Score, data$TOEFL.Score,data$University.Rating,data$SOP,data$LOR,data$CGPA)

# histogram 그려보기 (정규성 확인)
for(i in 1:7){
  hist(data_1[,i],density=50, breaks=30,prob=TRUE,xlab="x-variable", main="normal curve over histogram")
  m <- mean(data_1[,i])
  std <- sqrt(var(data_1[,i]))
  curve(dnorm(x, mean=m, sd=std), 
        col="darkblue", lwd=2, add=TRUE, yaxt="n")
}

# qqplot 그려보기 (정규성 확인)
for(i in 1:7){
  qqnorm(data_1[,i])
  qqline(data_1[,i])
}

# 1,2, 6, 7 번째 변수 정도 -> 정규성

#이상치 제거하기, 이를 통한 불필요 변수 제거

for (i in 1:7){
  out.val <- boxplot.stats(data_1[,i])$out
  data_1[,i][data_1[,i] %in% out.val] = NA
  pure_data <- data_1[complete.cases(data_1),]
}

#scatter plot, correlation plot
plot(pure_data[,-7] ,main="scatter plot")

c <- cor(pure_data[,-7])
library(corrplot)
corrplot(c,method="number")

# cut-off value 0.8
library(dplyr)
data_ynew <- pure_data %>% mutate(Admit=ifelse(Chance.of.Admit>0.8,1,0))

data_ynew <- data_ynew[,-8]
data_ynew$Research <- as.factor(data_ynew$Research)

ynew_input <- data_ynew[,c(1,2,3,4,5,6)]
ynew_input <- scale(ynew_input, center = TRUE, scale = TRUE)
ynew_target <- data_ynew[,c(7,8)]
ynew_data <- data.frame(ynew_input, ynew_target)

# training set, test set
set.seed(356)
idx=sample(1:nrow(ynew_data),0.7*nrow(ynew_data))
data_ynew_train = ynew_data[idx,]
data_ynew_test = ynew_data[-idx,]

# Train the Logistic Regression Model
full_lr <- glm(Admit ~ ., family="binomial", data_ynew_train)
summary(full_lr)

lr_response <- predict(full_lr, type = "response", newdata = data_ynew_test)
lr_target <- data_ynew_test$Admit
lr_predicted <- rep(0, length(lr_target))
lr_predicted[which(lr_response > 0.8)] <- 1

# confusion matrix
cm_full <- table(lr_target, lr_predicted)
cm_full


# Performance Evaluation Function -----------------------------------------
perf_eval2 <- function(cm){
  
  # True positive rate: TPR (Recall)
  TPR <- cm[2,2]/sum(cm[2,])
  # False positive rate
  FPR <- cm[1,2]/sum(cm[1,])
  # Precision
  PRE <- cm[2,2]/sum(cm[,2])
  # True negative rate: TNR
  TNR <- cm[1,1]/sum(cm[1,])
  # False negative rate
  FNR <- cm[2,1]/sum(cm[2,])
  # Simple Accuracy
  ACC <- (cm[1,1]+cm[2,2])/sum(cm)
  # Balanced Correction Rate
  BCR <- sqrt(TPR*TNR)
  # F1-Measure
  F1 <- 2*TPR*PRE/(TPR+PRE)
  
  return(c(TPR,FPR, PRE, TNR,FNR, ACC, BCR, F1))
}

# Initialize the performance matrix
perf_mat <- matrix(0, 1, 8)
colnames(perf_mat) <- c("TPR (Recall)","FPR", "Precision", "TNR","FNR", "ACC", "BCR", "F1")
rownames(perf_mat) <- "Logstic Regression"

# result
perf_mat[1,] <- perf_eval2(cm_full)
perf_mat


#roc curve
library(ROCR)
p <- predict(full_lr, newdata= data_ynew_test, type= "response")
pr <- prediction(p, data_ynew_test$Admit)
prf <- performance(pr, measure="tpr", x.measure="fpr")
plot(prf)
auc <- performance(pr, measure="auc")
auc <- auc@y.values[[1]]
auc
# seed 5번 변경 결과
# 0.9912605 , 0.9898702, 0.9737259, 0.9887566, 0.9785086

-------------------------------
#EXTRA QUESTION
# heart disease dataset

data_EX <- read.csv("heart.csv", header=TRUE)
str(data_EX)
data_EX$sex <- as.factor(data_EX$sex)
data_EX$cp <- as.factor(data_EX$cp)
data_EX$fbs <- as.factor(data_EX$fbs)
data_EX$restecg <- as.factor(data_EX$restecg)
data_EX$exang <- as.factor(data_EX$exang)
data_EX$slope <- as.factor(data_EX$slope)
data_EX$ca <- as.factor(data_EX$ca)
data_EX$thal <- as.factor(data_EX$thal)

# 이상치 제거
for (i in 1:7){
  out.val_ex <- boxplot.stats(data_EX[,i])$out
  data_EX[,i][data_EX[,i] %in% out.val_ex] = NA
  pure_data_EX <- data_EX[complete.cases(data_EX),]
}

# training set, test set 분할
set.seed(1234)
idx=sample(1:nrow(pure_data_EX),0.7*nrow(pure_data_EX))
EX_train = pure_data_EX[idx,]
EX_test = pure_data_EX[-idx,]

# Logistic regression 모델 학습 
EX_full_lr <- glm(target ~ ., family="binomial", EX_train)
summary(EX_full_lr)

EX_lr_response <- predict(EX_full_lr, type = "response", newdata = EX_test)
EX_lr_target <- EX_test$target
EX_lr_predicted <- rep(0, length(EX_lr_target))

# cut-off value 0.2
EX_lr_predicted[which(EX_lr_response > 0.2)] <- 1

# confusion matrix
EX_cm_full <- table(EX_lr_target, EX_lr_predicted)
EX_cm_full

# matrix 초기화 
EX_perf_mat <- matrix(0, 1, 8)
colnames(EX_perf_mat) <- c("TPR (Recall)","FPR", "Precision", "TNR","FNR", "ACC", "BCR", "F1")
rownames(EX_perf_mat) <- "Logstic Regression"

# result
EX_perf_mat[1,] <- perf_eval2(EX_cm_full)
EX_perf_mat
