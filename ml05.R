# 버섯 분류 - 나이브 베이즈

# 1. 데이터 준비
mushroom <- read.csv("mlwr/mushrooms.csv", encoding = "UTF-8", stringsAsFactors = T)

# 2. 데이터 확인, 전처리
str(mushroom)

# veil_type 변수(특징)는 모든 관찰값에서 항상 같은 값("partial") -> 버섯 분류에 사용되지 않는 변수 -> 데이터프레임에서 제거
mushroom$veil_type <- NULL

# 버섯들의 클래스(분류 레이블) - type 변수
table(mushroom$type)
prop.table(table(mushroom$type))  # 식용 : 독버섯 = 52% : 48%

# 나이브 베이즈 방법을 사용할 학습 데이터 세트/테스트 데이터 세트
# 학습(75%) : 테스트(25%)
sample_count <- round(nrow(mushroom) * 0.75)
# nrow(데이터프레임): 데이터프레임의 행(row, observation) 개수
# round(): 반올림

set.seed(123) # 같은 순서의 난수를 발생시키기 위해서
sample_rows <- sample(nrow(mushroom), sample_count)
sample_rows

# 학습 데이터 세트
mushroom_train <- mushroom[sample_rows, ]
train_label <- mushroom_train$type
mushroom_train <- mushroom_train[-1]  # 첫번째 컬럼(type)을 제거

str(mushroom_train)
table(train_label)
prop.table(table(train_label))

# 테스트 데이터 세트
mushroom_test <- mushroom[-sample_rows, ]
test_label <- mushroom_test$type
mushroom_test <- mushroom_test[-1]

str(mushroom_test)
prop.table(table(test_label))

# 3. 모델 생성 - 나이브 베이즈
library(e1071)
classifier <- naiveBayes(mushroom_train, train_label)
summary(classifier)

# 4. 모델 평가
mushroom_predict <- predict(classifier, mushroom_test)

library(gmodels)
CrossTable(x = test_label, y = mushroom_predict, prop.chisq = F)


# 5. 모델 향상 - 라플라스 추정량 변경
classifier <- naiveBayes(mushroom_train, train_label, laplace = 0.1)
mushroom_predict <- predict(classifier, mushroom_test)
CrossTable(x = test_label, y = mushroom_predict, prop.chisq = F)
