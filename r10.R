# ggplot2 패키지를 사용한 그래프
# gg: grammar of graph(그래프 그리기 문법)
# install.packages("ggplot")
library(ggplot2)
search()

rm(list = ls())

# ggplot2 패키지의 mpg 데이터 프레임 구조 확인
str(mpg)

# 자동차 배기량(displ)과 시내주행 연비(cty) 사이의 관계
# 1) 그래프를 그릴 데이터(데이터프레임), 좌표축 설정
g <- ggplot(data = mpg, mapping = aes(displ, cty))

# 2) 그래프의 종류 선택
g <- g + geom_point()  # 좌표축 + 그래프
g

# 3) 옵션 추가
g <- g + xlim(3, 6)
g

ggplot(mpg, aes(displ, cty)) + geom_point() + ylim(10, 30)

# Q1. mpg 데이터의 cty(도시 연비)와 hwy(고속도로 연비) 간에 어떤 관계가 있는지 알아보려고 합니다. x축은 cty, y축은 hwy로 된 산점도를 만들어 보세요.
ggplot(mpg, aes(cty, hwy)) + geom_point()
# Q2. 미국 지역별 인구통계 정보를 담은 ggplot2 패키지의 midwest 데이터를 이용해서 전체 인구와 아시아인 인구 간에 어떤 관계가 있는지 알아보려고 합니다.
#   x축은 poptotal(전체 인구), y축은 popasian(아시아인 인구)으로 된 산점도를 만들어 보세요.
#   전체 인구는 50만 명 이하, 아시아인 인구는 1만 명 이하인 지역만 산점도에 표시되게 설정하세요.
ggplot(midwest, aes(poptotal, popasian)) + geom_point() + xlim(0, 500000) + ylim(0, 10000)


str(mpg)
table(mpg$cyl)
ggplot(mpg, aes(cyl, cty)) + geom_point()


ggplot(mpg, aes(displ, cty, color = as.factor(cyl), shape = as.factor(drv))) + geom_point()
