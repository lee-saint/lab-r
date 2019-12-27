rm(list = ls())

# Artificial Neural Network(인공 신경망)
# f(x) = 2x + 1
curve(expr = 2 * x + 1, from = -5, to = 5, )
# sigmoid 함수: f(x) = 1 / [1 + exp(-x)]
curve(expr = 1 / (1 + exp(-x)), from = -10, to = 10)
# hyperbolic tangent: f(x) = tanh(x)
curve(expr = tanh(x), from = -5, to = 5)


# 콘크리트의 강도 예측
concrete <- read.csv("mlwr/concrete.csv")
str(concrete)
summary(concrete)

# 정규화(Normalization): 실제값 -> 0 ~ 1
# 표준화(Standardization): z-score 표준화(평균, 표준편차)

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

concrete_norm <- as.data.frame(lapply(concrete, normalize))
summary(concrete_norm)

# 신경망 알고리즘 적용하기 위한 패키지: neuralnet
# 오차 역전파(backpropagation)
install.packages("neuralnet")
library(neuralnet)

# 3. 모델 생성, 학습
# 학습 데이터 세트(75%) / 테스트 데이터 세트(25%)
1030 * 0.75
concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]

summary(concrete_train$strength)
summary(concrete_test$strength)

# 신경망 모델 생성
set.seed(12345)
concrete_model <- neuralnet(formula = strength ~ ., data = concrete_train)

# 생성된 NN을 확인
plot(concrete_model)

# 4. 만들어진 NN을 평가 - 테스트 데이터 세트에 적용
model_result <- compute(concrete_model, concrete_test[-9])
head(model_result)  # 신경망 모델에 의해서 계산된 stength 예측값
summary(model_result)

predict_result <- model_result$net.result
# 예측 결과와 실제 값의 상관관계 - 상관계수
cor(predict_result, concrete_test$strength) # 0.81

tail(concrete_test$strength)
tail(predict_result)

# 모델 향상
model2 <- neuralnet(formula = strength ~ ., data = concrete_train, hidden = 2)
plot(model2)
model2_result <- compute(model2, concrete_test[-9])
model2_predict <- model2_result$net.result

model5 <- neuralnet(formula = strength ~ ., data = concrete_train, hidden = 5)
plot(model5)
model5_result <- compute(model5, concrete_test[-9])
model5_predict <- model5_result$net.result

# 각 모델(model2, model5)에서 예측 결과와 실제 strength간의 상관 계수를 계산
cor(model2_predict, concrete_test$strength) # 0.902
cor(model5_predict, concrete_test$strength) # 0.928

# 평균 절대 오차(MAE: Mean Absolute Error) 함수 작성 -> 각 모델의 MAE를 계산
MAE <- function(actual, predict) {
  return(mean(abs(actual - predict)))
}
MAE(predict_result, concrete_test$strength)  # 0.093
MAE(model2_predict, concrete_test$strength)  # 0.069
MAE(model5_predict, concrete_test$strength)  # 0.057

# 역정규화(정규화 -> 실제값) 함수 작성 -> 실제 데이터프레임(concrete)의 값들과 비교
denorm <- function(norm, denorm) {
  return(norm * (max(denorm) - min(denorm)) + min(denorm))
}

model1_pred_denorm <- denorm(predict_result, concrete$strength)
summary(model1_pred_denorm)
tail(model1_pred_denorm)
tail(concrete$strength)
MAE(model1_pred_denorm, concrete[774:1030, 9])  # 7.4927

model2_pred_denorm <- denorm(model2_predict, concrete$strength)
summary(model2_pred_denorm)
tail(model2_pred_denorm)
tail(concrete$strength)
MAE(model2_pred_denorm, concrete[774:1030, 9])  # 5.5039

model5_pred_denorm <- denorm(model5_predict, concrete$strength)
summary(model5_pred_denorm)
tail(model5_pred_denorm)
tail(concrete$strength)
MAE(model5_pred_denorm, concrete[774:1030, 9])  # 4.6050

actual_predict_df <- data.frame(actual = concrete[774:1030, 9],
                                predict1 = model1_pred_denorm,
                                predict2 = model2_pred_denorm,
                                predict5 = model5_pred_denorm)
head(actual_predict_df, 10)

cor(actual_predict_df$actual, actual_predict_df$predict1)
cor(actual_predict_df$actual, actual_predict_df$predict2)
cor(actual_predict_df$actual, actual_predict_df$predict5)
# 정규화된 값들의 상관계수 = 정규화되지 않은 값들의 상관계수

softplus <- function(x) log(exp(x) + 1)
1 + softplus(1)

relu <- function(x) ifelse(x > 0, x, 0)
relu(-1)
relu(3)

curve(expr = softplus, from = -10, to = 10)

model_custom <- neuralnet(formula = strength ~ ., data = concrete_train, hidden = c(5, 3), act.fct = softplus)
plot(model_custom)
model_c_result <- compute(model_custom, concrete_test[-9])
model_c_predict <- model_c_result$net.result

cor(model_c_predict, concrete_test$strength)  # (5,3) tanh: 0.9321 / softplus: 0.9317 / logistic: 0.9343 / (5, 3, 3) logistic: 0.9435
MAE(model_c_predict, concrete_test$strength)  # (5,3) tanh: 0.0543 / softplus: 0.0557 / logistic: 0.0544 / (5, 3, 3) logistic: 0.0506
