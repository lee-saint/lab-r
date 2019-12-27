# 연관 규칙(Association Rules) 학습

# 1. 데이터 준비
groceries <- read.csv("mlwr/groceries.csv", header = F)
str(groceries)
head(groceries, 10)

# csv 파일의 각 행에는 영수증의 구매 아이템이 있음
# 그러나 영수증마다 구매 아이탬의 개수가 달라 컬럼의 개수가 일정하지 않음
# 해결방법: sparse matrix(희소행렬)를 사용

# arules 패키지: association rules(연관 규칙) 패키지
install.packages("arules")
library(arules)

# 장바구니 영수증 데이터(csv)를 희소 행렬로 만들기
groceries <- read.transactions(file = "mlwr/groceries.csv", header = F, sep = ",")
  # read. transactions 함수에서 header 파라미터의 기본값은 FALSE, sep 파라미터의 기본값은 ""이기 때문에 반드시 ","를 전달해야 함

summary(groceries)
inspect(groceries)
inspect(groceries[1:5])
inspect(groceries[9831:9835])
inspect(groceries[, 1:5])

# 영수증에 등장하는 아이템들의 빈도(frequency)
itemFrequency(groceries[, 1:5])
itemFrequency(groceries[, 165:169])

# 거래 아이탬들의 빈도 분포
itemFrequencyPlot(groceries, support = 0.1)
  # support: 영수증에 아이템이 나타나는 횟수 / 최소 10% 이상 나타나는 아이템만 그래프에 표시
itemFrequencyPlot(groceries, topN = 20)
  # topN: 그래프에 표시할 빈도 순위

# 희소 행렬(sparse matrix)를 그래프로 표시
image(groceries[1:100, ])
  # 데이터의 이상치나 어떤 경향을 파악할 수도 있음

# 3. 모델 학습 - 자율(비지도) 학습의 한 종류인 a priori 알고리즘
grocery_rules <- apriori(data = groceries)
summary(grocery_rules)
  # 만들어진 연관규칙이 없다?!
  # apriori 함수의 임계값 파라미터 기본값이 support = 0.10, confidence = 0.8로 되어 있는데,
  # 이 경우 수준이 너무 높아서 연관 규칙을 만들 수 없음

grocery_rules2 <- apriori(data = groceries, parameter = list(support = 0.03, confidence = 0.25, minlen = 2))
summary(grocery_rules2)
inspect(grocery_rules2)

grocery_rules3 <- apriori(data = groceries, parameter = list(support = 0.006, confidence = 0.25, minlen = 2))
summary(grocery_rules3)
inspect(grocery_rules3[1:10])
inspect(grocery_rules3[400:410])



inspect(sort(grocery_rules3, by = "lift")[1:10])
# lift(x -> y) = confidence(x -> y) / support(y)