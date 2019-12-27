# 모델 성능 개선

# 데이터 준비
credit <- read.csv("mlwr/credit.csv")
str(credit)

library(caret)
modelLookup("C5.0")
# C5.0 모델(의사결정 트리)에서 성능 향상을 위해 변경 가능한 파라미터 리스트를 보여줌

modelLookup("knn")

set.seed(1021)
m <- train(default ~ ., data = credit, method = "C5.0")
str(m)
m

p <- predict(m, credit)
table(p, credit$default)

# 튜닝 절차 자동화
ctrl <- trainControl(method = "cv", number = 10, selectionFunction = "oneSE")
ctrl  # caret::train  함수의 제어 조건들 생성

grid <- expand.grid(model = "tree", trials = c(1, 5, 10, 15, 20, 25, 30, 35), winnow = F)
grid

grid2 <- expand.grid(model = c("tree", "rules"), trials = c(1, 5, 10, 15, 20, 25), winnow = F)
grid2

set.seed(1021)
m <- train(default ~ ., data = credit, method = "C5.0", metric = "Kappa", trControl = ctrl, tuneGrid = grid)
m
