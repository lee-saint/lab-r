# Decision Tree(의사결정 나무)

# csv 파일을 읽어서 데이터프레임 생성
credit <- read.csv("mlwr/credit.csv", encoding = "UTF-8")
str(credit)
head(credit)
# 대출 상환 능력과 관계가 있을 것 같은 변수들?
# 범주형 변수(특징) -> table(도수분포표)
# 수치형 변수(특징) -> summary(기술 통계량)
table(credit$checking_balance)
table(credit$savings_balance)
summary(credit$months_loan_duration)
summary(credit$amount)

# 채무불이행(default) 빈도수
table(credit$default)

head(credit$amount, 10)
tail(credit$amount,10)

# random sampling: 정렬되어 있는 데이터를 무작위 추출
sample(1000, 10)

# 학습 데이터 세트와 테스트 데이터 세트(9:1 비율)를 준비
rows <- sample(1000, 900)
train_set <- credit[rows, ]  # 학습 데이터 세트
test_set <- credit[-rows, ]  # 테스트 데이터 세트

# 학습/테스트 데이터 세터에서 default 비율 확인
prop.table(table(train_set$default))
prop.table(table(test_set$default))

# 의사결정 나무(decision tree)를 사용하기 위한 패키지
install.packages("C50")
library(C50)  # 라이브러리 로딩
search()

# C50::C5.0(train, class, trials = 1, costs = NULL)
# train: 학습 데이터 세트
# class: 학습 데이터의 레이블(분류)
# trials, costs: 옵션 -> 성능 개선
# 학습 데이터 세트에서는 데이터의 레이블(클래스)을 제거해야 함!
credit_model <- C5.0(train_set[-17], train_set$default)
credit_model
summary(credit_model)


# 모델을 테스트 데이터를 사용해서 평가
# stats::predict(모델, 테스트 데이터세트)
credit_predict <- predict(credit_model, test_set)
library(gmodels)
CrossTable(x = test_set$default, y = credit_predict, prop.chisq = F)


# 모델 성능 개선 1) 의사결정 나무의 개수를 변경
credit_boost <- C5.0(train_set[-17], train_set$default, trials = 10)
credit_boost
summary(credit_boost)


# Adaboost를 적용한 모델 성능 평가
credit_boost_predict <- predict(credit_boost, test_set[-17])
table(credit_boost_predict)

# 이원교차표
CrossTable(x = test_set$default, y = credit_boost_predict, prop.chisq = F)


# 모델 성능 개선 방법 2) 비용 행렬(cost matrix) 사용: 발생할 수 있는 오류에 패널티를 추가
# 비용 행렬: 패널티 값들로 이루어진 행렬
# 비용 행렬의 행/열이 이름
matrix_dimname <- list(predict = c("no", "yes"),
                       actual = c("no", "yes"))
matrix_dimname
#비용 행렬
cost_matrix <- matrix(data = c(0, 1, 4, 0),
                      nrow = 2,
                      dimnames = matrix_dimname)
cost_matrix

# 모델 훈련에 비용 행렬을 적용
credit_cost <- C5.0(train_set[-17], train_set$default, costs = cost_matrix)
credit_cost
summary(credit_cost)

credit_cost_predict <- predict(credit_cost, test_set[-17])
CrossTable(x = test_set$default, y = credit_cost_predict, prop.chisq = F, prop.c = F, prop.r = F)





# 규칙 학습자(rule learner) 분류기
mushroom <- read.csv("mlwr/mushrooms.csv", encoding = "UTF-8")
str(mushroom)
head(mushroom)
table(mushroom$type)

# veil-type 변수는 모든 행이 동일한 값: 분류 기준이 될 수 없음
mushroom$veil_type <- NULL
str(mushroom)

# 규칙 분류기 - One Rule 분류기
install.packages("OneR")
library(OneR)  # 패키지 로딩

# 모델 훈련
mushroom_1R <- OneR(type ~ ., data = mushroom)
mushroom_1R

mushroom_1R_cap <- OneR(type ~ cap_shape + cap_surface + cap_color, data = mushroom)
mushroom_1R_cap


summary(mushroom_1R)
CrossTable(x = mushroom$type, y = mushroom$odor, prop.r = F, prop.c = F, prop.chisq = F)

# 성능 개선을 위해 RIPPER 알고리즘을 사용
install.packages("RWeka")
library(RWeka)  # 패키지 로딩

mushroom_ripper <- JRip(type ~ ., mushroom)
mushroom_ripper
summary(mushroom_ripper)



# 버섯 데이터로 나이브 베이즈 알고리즘 활용
str(mushroom)
library(e1071)

sample_row <- sample(8124, 6093)
mushroom_train = mushroom[sample_row, ]
mushroom_test = mushroom[-sample_row, ]
prop.table(table(mushroom_train$type))
prop.table(table(mushroom_test$type))
mushroom_classifier <- naiveBayes(mushroom_train[-1], mushroom_train$type, laplace = 1)
str(mushroom_classifier)

mushroom_pred <- predict(mushroom_classifier, mushroom_test[-1])

CrossTable(mushroom_test$type, mushroom_pred, prop.r = F, prop.c = F, prop.chisq = F)



# 버섯 데이터로 의사결정 나무
mushroom_decision_tree <- C5.0(mushroom_train[-1], mushroom_train$type, trials = 5)
mushroom_dt_pred <- predict(mushroom_decision_tree, mushroom_test[-1])
CrossTable(mushroom_test$type, mushroom_dt_pred, prop.r = F, prop.c = F, prop.chisq = F)



# sms 데이터 의사결정 나무
sms_decision_tree <- C5.0(sms_train, sms_train_label, trials = 10, costs = sms_cost_matrix)
sms_dt_pred <- predict(sms_decision_tree, sms_test)
CrossTable(sms_test_label, sms_dt_pred, prop.r = F, prop.c = F, prop.chisq = F)

sms_matrix_dimname <- list(predict = c("ham", "spam"),
                       actual = c("ham", "spam"))
sms_matrix_dimname

sms_cost_matrix <- matrix(data = c(0, 4, 1, 0),
                      nrow = 2,
                      dimnames = sms_matrix_dimname)
sms_cost_matrix

# sms 데이터 규칙학습
sms_ripper 
