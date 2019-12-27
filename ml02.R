# k-NN 알고리즘을 이용한 Iris 품종 분류

# csv 파일에서 데이터프레임 생성
iris <- read.csv("data/Iris.csv", stringsAsFactors = F)
str(iris)
head(iris)
tail(iris)

# iris 품종 분류와는 관계없는 변수(특징)인 Id를 제거
iris <- iris[-1]
str(iris)

# Species 컬럼을 팩터로 만들기 - 레이블
iris$Species <- factor(iris$Species,
                       levels = c("Iris-setosa", "Iris-versicolor", "Iris-virginica"),
                       labels = c("Setosa", "Versicolor", "Virginica"))
str(iris)
table(iris$Species)

# 학습 데이터 세트, 테스트 데이터 세트를 준비
# 품종별로 구분되어 있는 데이터를 랜덤하게 섞은 후 데이터 세트를 나눠야 함
v <- c(1:10)
v
# sample(벡터): 벡터의 원소들을 랜덤하게 모두 추출
sample(v)
# sample(벡터, n): 벡터의 원소들 중에서 n개의 원소를 랜덤하게 추출
sample(v, 7)
# sample(n): 1 ~ n까지 n개의 정수를 랜덤하게 추출
sample(5)
sample(150)

# nrow(데이터프레임), ncol(데이터프레임): 데이터프레임의 행/열의 개수
nrow(iris)
iris_shuffled <- iris[sample(nrow(iris)), ]
head(iris_shuffled)
tail(iris_shuffled)
table(iris_shuffled$Species)

# 학습 데이터
train_set <- iris_shuffled[1:100, -5]
head(train_set)

# 학습 데이터 레이블
train_label <- iris_shuffled[1:100, 5]
head(train_label)

# 테스트 데이터
test_set <- iris_shuffled[101:150, -5]
head(test_set)

# 테스트 데이터 레이블
test_label <- iris_shuffled[101:150, 5]
head(test_label)

# 최소-최대 정규화 함수 정의
normalize <- function(x) {
  return( (x - min(x)) / ( max(x) - min(x)))
}

train_set <- as.data.frame(lapply(train_set, normalize))
summary(train_set)

test_set <- as.data.frame(lapply(test_set, normalize))
summary(test_set)

# knn 함수가 있는 패키지
library(class)
# CrossTable 함수가 있는 패키지
library(gmodels)
search()

# knn을 적용햇을 때 예측값
predict <- knn(train = train_set, test = test_set, cl = train_label, k  = 9)
CrossTable(x = test_label, y = predict, prop.chisq = F)
