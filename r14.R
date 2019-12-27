rm(list = ls())
exam <- read.csv("data/csv_exam.csv")
str(exam)
head(exam)
tail(exam)

exam_by_class <- exam %>% group_by(class) %>% summarise(math = mean(math), english = mean(english), science = mean(science))
exam_by_class

ggplot(data = exam_by_class) + geom_col(mapping = aes(x = class, y = math), position = "dodge") + geom_col(mapping = aes(x = class, y = english), position = "dodge")

install.packages("tidyr")
library(tidyr)
# tidyr::gather() 함수 - 엑셀의 pivotting 기능
df <- exam_by_class %>% gather(key = "subject", value = "mean", -class)
df

exam_by_class %>% gather(key = "subject", value = "mean", -class)
ggplot(data = df, mapping = aes(x = class, y = mean, fill = subject)) + geom_col(position = "dodge")
