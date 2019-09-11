# 데이터 불러오기, 불필요 변수 제거
data <- read.csv("kc_house_data.csv", header=TRUE)
data <- data[,-c(1,2,17,18,19)]
data <- data[,-c(4,5)]
str(data)
data$waterfront <- as.factor(data$waterfront)

# 입력변수 통계량 확인
library(moments)
statistic <- function(variabl){
  
  m <- mean(variabl)
  std <- sqrt(var(variabl))
  skew <- skewness(variabl)
  kur <- kurtosis(variabl)
  return(c(m,std, skew, kur))
  
}


statistic(data$bedrooms)
statistic(data$bathrooms)
statistic(data$floors)
statistic(data$condition)
statistic(data$grade)
statistic(data$sqft_above)
statistic(data$sqft_basement)
statistic(data$yr_built)
statistic(data$sqft_living15)
statistic(data$sqft_lot15)

#boxplot 도시하기
boxplot(data$bedrooms,data$bathrooms,data$floors,data$waterfront, data$view,data$condition,data$grade,data$sqft_above,data$sqft_basement,data$yr_built,data$yr_renovated,data$sqft_living15,data$sqft_lot15,  main="boxplot of input variables")

# 정규성 확인을 위해 numerical 변수 아닌 것은 잠시 제외함
data2 <- data[,-c(5,6,12)]
str(data2)
# 정규성 확인
for(i in 2:11){
  hist(data2[,i],density=50, breaks=30,prob=TRUE,xlab="x-variable", main="normal curve over histogram")
  m <- mean(data2[,i])
  std <- sqrt(var(data2[,i]))
  curve(dnorm(x, mean=m, sd=std), 
        col="darkblue", lwd=2, add=TRUE, yaxt="n")
}

for(i in 2:11){
qqnorm(data2[,i])
qqline(data2[,i])
}

#이상치 제거하기  (임재인 학우의 도움) + 이를 통한 불필요 변수 제거

for (i in 2:(nvar+1)){
  out.val <- boxplot.stats(data[,i])$out
  data[,i][data[,i] %in% out.val] = NA
  pure_data <- data[complete.cases(data),]
}
pure_data_filt <- pure_data[,-c(5,6)]


# scatterplot & corrplot 그리기 
plot(x=pure_data_filt$sqft_living15,y=pure_data_filt$sqft_lot15 ,main="scatter plot - sqft_living15&sqft_lot15")

data_filt2 <- data_filt[,-c(5,6,12)]
data_filt_x <- data_filt2[,-c(1)]
str(data_filt2)
#correlation & corrplot
c <- cor(data_filt_x)
install.packages("corrplot")
library(corrplot)
corrplot(c,method="number")

#강한 상관관계 bathrooms&sqft_above , bathrooms&grade, yr_built&bathrooms, floors&sqft_above, sqft_above&grade, sqft_above&sqft_living15, grade&sqft_living15, sqft_living15&bathrooms

#training, test set 분할 
library(dplyr)

pure_data_filt_new <- pure_data_filt[,-c(10)]

nhouse <- nrow(pure_data_filt_new)
nvar <- ncol(pure_data_filt_new)

house_trn_idx <- sample(1:nhouse, round(0.7*nhouse))
house_trn_data <- pure_data_filt_new[house_trn_idx,]
house_test_data <- pure_data_filt_new[-house_trn_idx,]
str(house_trn_data)

# training MLR
mlr_house <- lm(price ~ ., data = house_trn_data)
mlr_house
summary(mlr_house)
plot(mlr_house)

# Performance evaluation function for regression --------------------------
perf_eval_reg <- function(tgt_y, pre_y){
  
  # RMSE
  rmse <- sqrt(mean((tgt_y - pre_y)^2))
  # MAE
  mae <- mean(abs(tgt_y - pre_y))
  # MAPE
  mape <- 100*mean(abs((tgt_y - pre_y)/tgt_y))
  
  return(c(rmse, mae, mape))
  
}

#결과 입력받을 테이블 생성 
perf_mat <- matrix(0, nrow = 1, ncol = 3)

rownames(perf_mat) <- c("House price")
colnames(perf_mat) <- c("RMSE", "MAE", "MAPE")
perf_mat

# Performance Measure
mlr_house_haty <- predict(mlr_house, newdata = house_test_data)

perf_mat[1,] <- perf_eval_reg(house_test_data$price, mlr_house_haty)
perf_mat

# 7개만 남기고 제거해야 한다면 -> corr 상관계수 높고, 중요도 떨어지는 변수부터 제거
# bathrooms, grade, sqft_above 제거

# 다시 모델 세우기 
#training, test set 분할 
pure_data_filt_7 <- pure_data_filt_new[,-c(3,6,7)]
str(pure_data_filt_7)
nhouse7 <- nrow(pure_data_filt_7)
nvar7 <- ncol(pure_data_filt_7)

# Split the data into the training/validation sets
house_trn_idx7 <- sample(1:nhouse7, round(0.7*nhouse7))
house_trn_data7 <- pure_data_filt_7[house_trn_idx7,]
house_test_data7 <- pure_data_filt_7[-house_trn_idx7,]

# training MLR
mlr_house7 <- lm(price ~ ., data = house_trn_data7)
mlr_house7
summary(mlr_house7)
plot(mlr_house7)


# 결과 입력받을 테이블 생성
perf_mat2 <- matrix(0, nrow = 1, ncol = 3)

rownames(perf_mat2) <- c("House price_7")
colnames(perf_mat2) <- c("RMSE", "MAE", "MAPE")
perf_mat2

# Performance Measure
mlr_house_haty7 <- predict(mlr_house7, newdata = house_test_data7)

perf_mat2[1,] <- perf_eval_reg(house_test_data7$price, mlr_house_haty7)
perf_mat2



# extra question
# target variable에 log를 씌우면?

mlr_houselog7 <- lm(log(price) ~ ., data = house_trn_data7)
mlr_houselog
summary(mlr_house7)
plot(mlr_houselog)

# 변수 선별하는 방법
fit.con <- lm(price~1,data=pure_data_filt_new)
fit.forward <- step(fit.con,scope=list(lower=fit.con,upper=mlr_house),
                    direction = "forward") 
fit.backward <- step(mlr_house,scope=list(lower=fit.con,upper=mlr_house),
                     direction = "backward")
fit.both <- step(fit.con,scope=list(lower=fit.con,upper=mlr_house), direction = "both")
