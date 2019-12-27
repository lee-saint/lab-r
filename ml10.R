rm(list = ls())

# SVM(Support Vector Machine)을 이용한 분류

# 1. 데이터 준비
letters <- read.csv("mlwr/letterdata.csv")
str(letters)
head(letters, 10)
table(letters$letter)

# 학습 데이터(80%) / 테스트 데이터(20%) 세트
letters_train <- letters[1:16000, ]
letters_test <- letters[16001:20000, ]

table(letters_train$letter)
table(letters_test$letter)

# 3. 모델 생성 - SVM
# kernlab 패키지
install.packages("kernlab")
library(kernlab)
search()
detach("package:neuralnet")

# SVM 알고리즘 모델을 생성
letter_classifier <- ksvm(letter ~ ., data = letters_train, kernel = "vanilladot")

# 4. 모델 평가
letters_predict <- predict(letter_classifier, letters_test)
head(letters_predict)
table(letters_predict, letters_test$letter)

correct <- letters_predict == letters_test$letter
correct_count <- sum(correct)
correct_count
correct_ratio <- correct_count / 4000
correct_ratio

# 5. 모델 수정 -> 재평가 -> 성능 개선
classifier2 <- ksvm(letter ~ ., data = letters_train, kernel = "rbfdot")
predict2 <- predict(classifier2, letters_test)
head(predict2, 10)
head(letters_test$letter, 10)

table(predict2, letters_test$letter)
correct2 <- predict2 == letters_test$letter
prop.table(table(correct2))
