# Regression Tree와 Model Tree

# 1. 데이터 준비
wine <- read.csv("mlwr/whitewines.csv")

# 2. 데이터 확인, 전처리
str(wine)
summary(wine)
# 종속변수(quality)의 분포
hist(wine$quality)

# regression tree를 사용하기 위한 패키지
# rpart: recursive partitioning
install.packages("rpart")
library(rpart)

# 3. 모델 학습
# 학습 데이터 세트(75%) / 테스트 데이터 세트(25%)
head(wine)
tail(wine)
4898 * 0.75
wine_train <- wine[1:3674, ]
wine_test <- wine[3675:4898, ]

# 학습 데이터를 rpart 패키지를 사용해 학습시킴
wine_rpart <- rpart(formula = quality ~ ., data = wine_train)
wine_rpart
summary(wine_rpart)

# rpart(회귀 트리) 결과를 시각적으로 보여주는 패키지: rpart.plot
install.packages("rpart.plot")
library(rpart.plot)

rpart.plot(x = wine_rpart, digits = 3)
rpart.plot(wine_rpart, fallen.leaves = F, type = 3, extra = 101)

# 4. 모델 평가: regression tree가 테스트 데이터를 얼마나 잘 설명하는가?
wine_predict <- predict(wine_rpart, wine_test)
summary(wine_predict)        # 예측 quality의 기술 통계량
summary(wine_test$quality)   # 실제 quality의 기술 통계량

# 모델 성능 평가
# 1) 상관 계수(correlation coefficient): -1 <= cor <= 1
cor(wine_predict, wine_test$quality)  # 0.54

# 2) MAE(Mean absolute Error): 평균 절대 오차
# 오차(실제값-예측값)들의 절대값 평균
MAE <- function(actual, predict) {
  return(mean(abs(actual - predict)))
}

# 함수 테스트
MAE(actual = c(1, 2, 3), predict = c(1.1, 1.9, 3.0))

MAE(wine_test$quality, wine_predict)  # 0.6

# 5. 모델 성능 향상
# 모델 트리(Model Tree): Regression Tree(분류) + Linear Regression Modeling(회귀 모델 적용)
# 교재 RWeka 패키지의 M5P 함수 사용
# Cubist 패키지: 규칙 학습 기반 분류 + M5P 알고리즘 회귀 모델 적용
install.packages("Cubist")
library(Cubist)

# cubist(x = 훈련데이터, y = 훈련 데이터의 결과)
wine_cubist <- cubist(x = wine_train[-12], y = wine_train$quality)
wine_cubist
summary(wine_cubist)

# 모델 트리의 성능 테스트
wine_predict2 <- predict(wine_cubist, wine_test)
head(wine_predict2)

# 기술 통계량
summary(wine_predict2)
summary(wine_test$quality)

# 상관 계수
cor(wine_predict2, wine_test$quality)  # 0.64

# MAE: 평균 절대 오차
MAE(wine_predict2, wine_test$quality)  # 0.54

# 상관계수는 커지고 오차는 작아짐 -> 회귀 트리에 비해 성능이 좋아졌다 할 수 있음


# REDWINE 데이터셋
redwine <- read.csv("mlwr/redwines.csv")
str(redwine)
head(redwine)
tail(redwine)
hist(redwine$quality)

1599 * 0.75
redwine_train = redwine[1:1199, ]
redwine_test = redwine[1120:1599, ]
prop.table(table(redwine_train$quality))
prop.table(table(redwine_test$quality))

# 회귀 트리(rpart)
redwine_rpart <- rpart(formula = quality ~ ., data = redwine_train)
redwine_rpart
summary(redwine_rpart)
rpart.plot(redwine_rpart, type = 5, extra = 101)

redwine_predict <- predict(redwine_rpart, redwine_test)
head(redwine_predict)

summary(redwine_predict)
summary(redwine_test$quality)

cor(redwine_predict, redwine_test$quality)  # 0.61
MAE(redwine_predict, redwine_test$quality)  # 0.53

# 모델 트리(cubist)
redwine_cubist <- cubist(redwine_train[-12], redwine_train$quality)
redwine_cubist
summary(redwine_cubist)

redwine_predict2 <- predict(redwine_cubist, redwine_test)
head(redwine_predict2)

summary(redwine_predict2)
summary(redwine_test$quality)

cor(redwine_predict2, redwine_test$quality)  # 0.67
MAE(redwine_predict2, redwine_test$quality)  # 0.45



summary(games$User.Rating.Count)
boxplot(games$User.Rating.Count)
games_many_rating <- games[games$User.Rating.Count > 10000, ]
summary(games_many_rating$User.Rating.Count)
hist(games_many_rating$User.Rating.Count)
