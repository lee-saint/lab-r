rm(list = ls())

# 미국 의료비 데이터
insurance <- read.csv("mlwr/insurance.csv")

# 데이터 확인
str(insurance)
summary(insurance)
head(insurance)

# 종속 변수 - expenses(의료비 지출)
boxplot(insurance$expenses)
hist(insurance$expenses)  # 오른쪽으로 꼬리가 긴 분포
  # 대다수 사람들의 의료비는 0 ~ 15,000 사이에 분포

# 상관 계수: cor(x, y)
cor(insurance$bmi, insurance$expenses)
# 상관 행렬: 상관 계수들로 만든 행렬
cor(insurance[c("age", "bmi", "children", "expenses")])

pairs(insurance[c("age", "bmi", "children", "expenses")])


install.packages("psych")
library(psych)
pairs.panels(insurance[c("age", "bmi", "children", "expenses")])
pairs.panels(insurance)

# 다중 선형 회귀(multiple linear regression)
# expenses ~ 나머지 모든 변수
ins_model <- lm(formula = expenses ~ ., data = insurance)
ins_model
summary(ins_model)

# 선형 회귀 모델을 수정해서 모델 성능 향상
# 나이의 비선형 항을 추가
insurance$age2 <- insurance$age ^ 2
head(insurance[c("age", "age2")])

# 수치형 변수를 이진화
# bmi의 값이 30 이상이면 1, 그렇지 않으면 0으로 변환
insurance$bmi30 <- ifelse(insurance$bmi > 30, 1, 0)
head(insurance[c("bmi", "bmi30")])

# 두 개 이상 변수의 상호작용을 선형 회귀 모델에 추가
# 흡연 + 비만

str(insurance)
ins_model2 <- lm(formula = expenses ~ age + age2 + sex + bmi + children + bmi30 + smoker + smoker*bmi30 + region, data = insurance)
summary(ins_model2)



# 앱스토어 게임 데이터
games <- read.csv("mlwr/appstore_games.csv", stringsAsFactors = F, encoding = "UTF-8")
str(games)
summary(games[c("Average.User.Rating", "User.Rating.Count", "Price", "Size")])
pairs.panels(games[c("Average.User.Rating", "User.Rating.Count", "Price", "Size")])

game_model <- lm(formula = Average.User.Rating ~ User.Rating.Count + Price + Size, data = games)
summary(game_model)

game_rating_over_100 <- games[games$User.Rating.Count > 100, ]
str(game_rating_over_100)
pairs.panels(game_rating_over_100[c("Average.User.Rating", "User.Rating.Count", "Price", "Size")])

# mpg
str(mpg)
pairs.panels(mpg[c("displ", "cyl", "drv", "cty", "hwy", "fl", "class")])

mpg_model <- lm(formula = cty ~ displ + cyl + drv + fl + class, data = mpg)
summary(mpg_model)
