rm(list = ls())
iris <- read.csv("data/Iris.csv", stringsAsFactors = F)
str(iris)
iris <- iris[-1]
str(iris)
iris <- iris[1:100, ]
table(iris$Species)
iris$Species <- factor(iris$Species, levels = c("Iris-setosa", "Iris-versicolor"))
str(iris)
summary(iris)
iris_n <- as.data.frame(lapply(iris[-5], normalize))
summary(iris_n)

size <- sample(nrow(iris), 75)

train_data <- iris_n[size, ]
head(train_data)
train_label <- iris[size, "Species"]
train_label


test_data <- iris_n[!(rownames(iris_n) %in% size), ]
head(test_data)

test_size <- sample(rownames(test_data))
test_size

test_data <- test_data[test_size, ]
head(test_data)

test_label <- iris[test_size, "Species"]
test_label

sqrt(75)

predict <- knn(train = train_data, test = test_data, cl = train_label, k = 9)
table(test_label)
table(predict)

CrossTable(test_label, predict, prop.chisq = F)

# 표준화
iris_z <- as.data.frame(scale(iris[-5]))
head(iris_z)
summary(iris_z)

z_size <- sample(nrow(iris_z), 75)
z_train_data <- iris_z[z_size, ]
z_train_label <- iris[size, "Species"]
table(z_train_label)

z_test_data <- iris_z[!(rownames(iris_z) %in% z_size), ]
z_test_size <- sample(rownames(z_test_data))
z_test_data <- z_test_data[z_test_size, ]

z_test_label <- iris[z_test_size, "Species"]
table(z_test_label)

z_predict <- knn(train = z_train_data, test = z_test_data, cl = z_train_label, k = 7)
table(z_predict)

CrossTable(z_test_label, z_predict, prop.chisq = F)


n_train <- iris_n[z_size, ]
n_test <- iris_n[z_test_size, ]

n_predict <- knn(train = n_train, test = n_test, cl = z_train_label, k = 11)
table(n_predict)

CrossTable(z_test_label, n_predict, prop.chisq = F)
?dist
