#모델 성능 평가

sms_results <- read.csv("mlwr/sms_results.csv")
head(sms_results)

# spam/ham 분류에서 예측 확률이 50% 근처일 경우 / 예측하기 애매한 경우 모델이 잘못 예측할 가능성이 크다
library(dplyr)
sms_results %>% filter(prob_spam > 0.4 & prob_spam < 0.6) %>% head(10)

# 실제 값과 예측값이 다른 경우
sms_results %>% filter(actual_type != predict_type) %>% head(10)

# 혼동 행렬(confusion matrix)
table(sms_results$actual_type, sms_results$predict_type)

library(gmodels)
CrossTable(sms_results$actual_type, sms_results$predict_type, prop.chisq = F)

# kappa 통계량 계산
# Pr(a) : 실제 일치(actual agreement) 비율 / TN + TP
pr_a <- 0.865 + 0.109

# PR(e): 예상 일치(expected agrrement) 비율
# 독립 사건이라는 가정 하에서 P(실제 스팸) X P(예측 스팸) + P(실제 햄) x P(예측 햄)
pr_e <- 0.132 * 0.112 + 0.868 * 0.888
kappa <- (pr_a - pr_e) / (1 - pr_e)

# caret 패키지: Classification And REgression Training
install.packages("caret")
library(caret)
CrossTable(sms_results$actual_type, sms_results$predict_type, prop.chisq = F)
confusionMatrix(data = sms_results$predict_type,
                reference = sms_results$actual_type,
                positive = "spam")
  # data = 예측 결과, reference = 실제 결과, positive = 관심 클래스
  # Positive Predictive Value -> 정밀도(precision)

# 민감도
sensitivity(data = sms_results$predict_type, reference = sms_results$actual_type, positive = "spam")
# 특이도
specificity(data = sms_results$predict_type, reference = sms_results$actual_type, negative = "ham")
# 정밀도
precision(data = sms_results$predict_type, reference = sms_results$actual_type, relevant = "spam")
# F-척도 = (2 * precision * recall) / (precision + recall)
F_meas(data = sms_results$predict_type, reference = sms_results$actual_type, relevant = "spam")
f <- (2 * 0.974359 * 0.8306011) / (0.974359 + 0.8306011)
f


# ROC(Receiver Operation Characteristic) 곡선
install.packages("pROC")
library(pROC)
sms_roc <- roc(response = sms_results$actual_type, predictor = sms_results$prob_spam)
plot(sms_roc, col = "blue", lwd = 3)

sms_knn <- read.csv("mlwr/sms_results_knn.csv")
head(sms_knn)
sms_knn_roc <- roc(response = sms_results$actual_type, predictor = sms_knn$p_spam)
plot(sms_knn_roc, col = "red", lwd = 3, add = T)

# k-fold CV(Cross Validation, 교차 검증)
# caret::createFolds()
credit <- read.csv("mlwr/credit.csv")

random_ids <- order(runif(1000))
credit_train <- credit[random_ids[1:500], ]
credit_validate <- credit[random_ids[501:750], ]
credit_test <- credit[random_ids[751:1000], ]

in_train <- createDataPartition(credit$default, p = 0.75, list = F)
credit_part_train <- credit[in_train, ]
credit_part_test <- credit[-in_train, ]

folds <- createFolds(credit$default, k = 10)
str(folds)

library(C50)
install.packages("irr")
library(irr)

set.seed(123)
folds <- createFolds(credit$default, k = 10)

cv_results <- lapply(folds, function(x) {
  credit_train <- credit[-x, ]
  credit_test <- credit[x, ]
  credit_model <- C5.0(default ~ ., data = credit_train)
  credit_pred <- predict(credit_model, credit_test)
  credit_actual <- credit_test$default
  kappa <- kappa2(data.frame(credit_actual, credit_pred))$value
  return(kappa)
})

str(cv_results)
mean(unlist(cv_results))
