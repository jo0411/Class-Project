# library 불러오기, 함수 정의---------------------------------
install.packages("glmnet")
install.packages("GA")
library(moments)
library(glmnet)
library(GA)
install.packages("tree")

# Performance Evaluation function
perf_eval_reg <- function(tgt_y, pre_y){
  
  # RMSE
  rmse <- sqrt(mean((tgt_y - pre_y)^2))
  # MAE
  mae <- mean(abs(tgt_y - pre_y))
  # MAPE
  mape <- 100*mean(abs((tgt_y - pre_y)/tgt_y))
  
  return(c(rmse, mae, mape))
  
}

perf_mat <- matrix(0, nrow = 6, ncol = 3)

# Initialize a performance summary
rownames(perf_mat) <- c("All", "Exhaustive search","Forward Selection","Backward Elimination","Stepwise Selection","Genetic Algorithm")
colnames(perf_mat) <- c("RMSE", "MAE", "MAPE")
perf_mat

#-------------------------------------------------------------

# Data 불러오기
data <- read.csv("Weather_Ankara.csv", header=TRUE)

# Training set, Validation set 나누기

set.seed(1234)
idx=sample(1:nrow(data),0.78*nrow(data))
trainset = data[idx,]
testset = data[-idx,]

trn <- trainset
tst <- testset

# Train the MLR
mlr_trn <- lm(Mean_temperature ~ ., data = trn)
summary(mlr_trn)
plot(mlr_trn)

# Performance measure
mlr_trn_haty <- predict(mlr_trn, newdata = tst)

perf_mat[1,] <- perf_eval_reg(tst$Mean_temperature, mlr_trn_haty)
perf_mat

#------------------------------------------------------------
# Exhaustive Search (박종범 학우의 도움을 받음)

variable_x <- as.vector(colnames(trn)[-10])
output <- data.frame(c(0),c(0))
result <- data.frame(c(0),c(0))
colnames(output) <- c("Formula","Adjusted R-squared")
colnames(result) <- c("Formula","Adjusted R-squared")

start_time <- proc.time()

EX_search_func <- function(x_var_count, x_names){
for (i in 1:x_var_count){
  
  combination <- combn(x_names,i)
  
  for (j in 1:length(combination[1,])){
    
    comb_x_vari <- paste(combination[,j], collapse = " + ")
    formula <- paste("Mean_temperature ~ ", comb_x_vari, collapse="")
    EX_model <- lm(formula, data = trn)
    result[,1] <- formula 
    result[,2] <- summary(EX_model)$adj.r.squared
    output <- rbind(output, result)
    
  }
}

best_comb <- which.max(output[,2])
best_formula <- output[best_comb,1]

EX_bestmodel <- lm(best_formula, data=trn)
}

EX_search_func(9, variable_x)

end_time <- proc.time()
end_time-start_time

combcheck <- combn(variable_x,2)
summary(EX_bestmodel)
mlr_EX_trn_haty <- predict(EX_bestmodel, newdata=tst)

perf_mat[2,] <- perf_eval_reg(tst$Mean_temperature, mlr_EX_trn_haty)
#-----------------------------------------------------------------------

# Variable selection method : Forward selection
tmp_x <- paste(colnames(trn)[-10], collapse=" + ")
tmp_xy <- paste("trn_target ~ ", tmp_x, collapse = "")
as.formula(tmp_xy)
start_time <- proc.time()
forward_model <- step(lm(Mean_temperature ~ 1, data = trn), 
                      scope = list(upper = as.formula(tmp_xy), lower = Mean_temperature ~ 1), 
                      direction="forward", trace = 1)
end_time <- proc.time()
end_time-start_time
summary(forward_model)

# Performance measure
mlr_forward_trn_haty <- predict(forward_model, newdata = tst)

perf_mat[3,] <- perf_eval_reg(tst$Mean_temperature, mlr_forward_trn_haty)
perf_mat

#-------------------------------------------------------------------

# Variable selection method : Backward Elimination
start_time <- proc.time()
backward_model <- step(mlr_trn, 
                       scope = list(upper = as.formula(tmp_xy), lower = Mean_temperature ~ 1),
                       direction = "backward", trace = 1)
end_time <- proc.time()
end_time-start_time
summary(backward_model)

# Performance measure
mlr_backward_trn_haty <- predict(backward_model, newdata = tst)

perf_mat[4,] <- perf_eval_reg(tst$Mean_temperature, mlr_backward_trn_haty)
perf_mat

#------------------------------------------------------------------

# Variable selection method : Stepwise selection
start_time <- proc.time()
stepwise_model <- step(lm(Mean_temperature ~ 1, data = trn), 
                       scope = list(upper = as.formula(tmp_xy), lower = Mean_temperature ~ 1), 
                       direction="both", trace = 1)
end_time <- proc.time()
end_time-start_time
summary(stepwise_model)

# Performance measure
mlr_stepwise_trn_haty <- predict(stepwise_model, newdata = tst)

perf_mat[5,] <- perf_eval_reg(tst$Mean_temperature, mlr_stepwise_trn_haty)
perf_mat

#-----------------------------------------------------------------

# Variable selection method : Genetic Algorithm
# Fitness function: Adjusted R2 for the training dataset
fit_r2 <- function(string){
  sel_var_idx <- which(string == 1)
  # Use variables whose gene value is 1
  sel_x <- x[, sel_var_idx]
  xy <- data.frame(sel_x, y)
  # Training the model
  GA_lr <- lm(y ~ ., data = xy)
  GA_perf <- summary(GA_lr)$adj.r.squared
  return(GA_perf)
}

x <- as.matrix(trn[,-10])
y <- trn[,10]

# Variable selection by Genetic Algorithm
start_time <- proc.time()
GA_r2 <- ga(type = "binary", fitness = fit_r2, nBits = ncol(x), 
            names = colnames(x), popSize = 50, pcrossover = 0.5, 
            pmutation = 0.1, maxiter = 20, elitism = 2, seed = 123)

end_time <- proc.time()
end_time - start_time

best_var_idx <- which(GA_r2@solution == 1)

# Model training based on the best variable subset
GA_trn_data <- trn[,c(best_var_idx, 10)]
GA_tst_data <- tst[,c(best_var_idx, 10)]

GA_model <- lm(Mean_temperature ~ ., data= GA_trn_data)
summary(GA_model)

# Performance measure
mlr_gen_trn_haty <- predict(GA_model, newdata = tst)

perf_mat[6,] <- perf_eval_reg(tst$Mean_temperature, mlr_gen_trn_haty)
perf_mat

