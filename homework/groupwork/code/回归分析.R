# 导包
library(tidyr)
library(GGally)
library(MASS)
library(nnet)
library(caret)
library(glmnet)
library(ModelMetrics)

# 读取数据
data <- read.csv("D:/Program/DataSpell/datafile/R-Class/homework/groupwork/data/filtered_data.csv")
table(data$ProductChoice)

# 生成密度图
data_long <- gather(data, key = "varname", value = "value")
ggplot(data_long) + theme_bw() + geom_density(aes(value), fill = "red", alpha = 0.5) +
  facet_wrap(.~varname, scales = "free") + theme(axis.text.x = element_text(angle = 90))


# 分割测试集
set.seed(7)
training_samples <- data$ProductChoice %>% createDataPartition(p=0.8, list = FALSE)
train_data <- data[training_samples, ]
test_data <- data[-training_samples, ]
test_label <- test_data$ProductChoice

# 多元回归分析
model_multi_reg <- lm(ProductChoice~., data = data)
summary(model_multi_reg)
confint(model_multi_reg)
predictions_multi_reg <- predict(model_multi_reg, newdata = test_data)
# 使用ifelse函数进行转换
arr_transformed <- ifelse(predictions_multi_reg >= 0.5 & predictions_multi_reg < 1.5, 1,
                          ifelse(predictions_multi_reg >= 1.5 & predictions_multi_reg < 2.5, 2,
                                 ifelse(predictions_multi_reg >= 2.5 & predictions_multi_reg < 3.5, 3,
                                        ifelse(predictions_multi_reg >= 3.5 & predictions_multi_reg < 4.5, 4, NA))))
t_multi_reg <- 0
f_multi_reg <- 0
for(i in seq_along(arr_transformed)){
  if(arr_transformed[i] == test_label[i]){
    t_multi_reg <- t_multi_reg + 1
  }else{
    f_multi_reg <- f_multi_reg + 1
  }
}
acc_multi_reg <- t_multi_reg/(length(arr_transformed))
sprintf("多元回归的准确度为：%f",acc_multi_reg)


# 多元Logistic回归分析
model_logi <- multinom(ProductChoice~., data = train_data)
summary(model_logi)
# 准确性分析
predictions_logi <- predict(model_logi, newdata = test_data)
t_logi <- 0
f_logi <- 0
for(i in seq_along(predictions_logi)){
  if(predictions_logi[i] == test_label[i]){
    t_logi <- t_logi + 1
  }else{
    f_logi <- f_logi + 1
  }
}
acc_logi <- t_logi/(length(predictions_logi))
sprintf("多元逻辑斯蒂回归的准确度为：%f",acc_logi)



# 数据标准化
scal<-preProcess(train_data,method=c("center","scale"))
train_ds <- predict(scal,train_data)
test_ds<- predict(scal,test_data)

# 岭回归 / Lasso回归
ProductChoice_index <- grep("ProductChoice", names(train_data))
x <- as.matrix(train_ds[, -ProductChoice_index])
y <- train_ds[, ProductChoice_index]
test <- as.matrix( test_ds[, 1:10])
test_ds_label <- test[, ProductChoice_index]
# 岭回归
model_ridge <- cv.glmnet(x, y, family = "gaussian", alpha = 0, nfolds = 3)
plot(model_ridge)
predictions_ridge <- predict(model_ridge, test)
test_pre_o<- as.vector(predictions_ridge[,1]* scal$std[11] + scal$mean[11])

sprintf("标准化后平均绝对误差为：%f",mae(test_ds_label,predictions_ridge))
sprintf("标准化前平均绝对误差为：%f",mae(test_label,test_pre_o))

# Lasso回归
model_lasso <- glmnet(x, y, family = "gaussian", alpha = 1, nfolds = 3)
plot(model_lasso)
predictions_lasso <- predict(model_lasso, test)
test_lasso_o<- as.vector(predictions_lasso[,1]* scal$std[11] + scal$mean[11])

sprintf("标准化后平均绝对误差为：%f",mae(test_ds_label,predictions_lasso))
sprintf("标准化前平均绝对误差为：%f",mae(test_label,test_lasso_o))