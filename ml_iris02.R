rm(list = ls())

iris <- read.csv("data/Iris.csv")
iris <- iris[sample(nrow(iris)), ]
head(iris)

iris <- iris[-1]
head(iris)
str(iris)

iris_n <- as.data.frame(lapply(iris[-5], normalize))
head(iris_n)
train_d <- iris_n[1:120, ]
test_d <- iris_n[121:150, ]
train_l <- iris[1:120, "Species"]
test_l <- iris[121:150, "Species"]

pred <- knn(train = train_d, test = test_d, cl = train_l, k = 11)
table(test_l)
table(pred)

CrossTable(test_l, pred, prop.chisq = F)