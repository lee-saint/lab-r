# 데이터 분석 기초

# Global Env.(메모리)에 있는 변수들 삭제
rm(list = ls())

# csv 파일을 읽어서 데이터프레임 생성
exam <- read.csv("data/csv_exam.csv")

# 데이터프레임의 모든 데이터 출력
exam
# Viewer 창에 데이터들을 출력
View(exam)

# 데이터프레임의 처음 일부 데이터만 출력
head(exam)
head(exam, n = 3)

# 데이터프레임에서 마지막에 나오는 일부 데이터만 출력
tail(exam)
tail(exam, n = 3)

# 데이터프레임의 차원(dimension): 행과 열의 개수
dim(exam)
dimension <- dim(exam)
dimension[1] #데이터프레임의 행 개수
dimension[2] #데이터프레임의 열 개수
dim(exam)[1]

# 데이터프레임의 구조(structure)를 확인
str(exam)

# 요약 통계량(기술 통계량, descriptive statistics) 출력
# min: 최솟값
# 1st Quartile: 1사분위값
# median: 중앙값
# mean: 평균
# 3rd Quartile: 3사분위값
# max: 최댓값
summary(exam)


# ggplot2 패키지에는 mpg 데이터프레임이 있음 - 분석하기!
# as.data.frame(변수):
# 변수가 데이터프레임이면 데이터프레임을 반환
# 변수가 데이터프레임이 아니고 데이터프레임으로 변환 가능하면, 데이터프레임을 생성해서 리턴
mpg <- as.data.frame(ggplot2::mpg)

# mpg 데이터프레임의 차원(행/열 개수)
dim(mpg)

# mpg 데이터프레임의 구조
str(mpg)

# mpg 데이터프레임에서 데이터 일부만 출력
head(mpg)
tail(mpg)

# 기술통계량
summary(mpg)


# 데이터 가공(조작)을 위한 패키지 설치
install.packages("dplyr")
# 설치한 패키지를 검색 경로(메모리)에 로드
library(dplyr)
#검색경로에 로딩된 패키지 확인
search()

# 변수 이름 바꾸기
df <- data.frame(v1 = c(1, 2, 3),
                 v2 = c(11, 22, 33))
df
# 변수의 이름을 의미가 있는 이름으로 바꿔서 나중에 데이터를 분석할 때 편리하게
# rename(데이터프레임, 바꿀변수이름 = 원래변수이름)
# dplyr::rename() 함수는 새로운 데이터 프레임을 생성해서 반환(리턴). 원본 데이터프레임을 변경하는 것은 아님
rename(df, id = v1, score = v2)

df   # rename() 호출 후에도 df는 원본 그대로 유지됨

#원본 데이터프레임을 변경하고 싶을 때
df <- rename(df, id = v1, score = v2)
df



# 새로운 변수(컬럼) 추가:
# 데이터프레임$추가할변수 <- 벡터(값)
df_score <- data.frame(id = c(1, 2, 3),
                       math = c(90, 80, 100),
                       kor = c(70, 80, 90))
df_score

# 총점 변수(컬럼)을 추가
df_score$total <- df_score$math + df_score$kor
df_score

# mpg 데이터프레임에서 시내 주행 연비(cty)와 고속도로 주행 연비(hwy)의 평균을 계산해서 avg_mpg 변수를 추가
mpg$avg_mpg <- (mpg$cty + mpg$hwy) / 2
head(mpg)
tail(mpg)

# 새로 작성된 평균 연비 변수(컬럼)의 기술 통계량
summary(mpg$avg_mpg)

# 조건 함수(ifelse) 이용, 위에서 계산된 평균 연비(avg_mpg)의 값이
# 30 이상이면 1등급
# 20 이상 ~ 30 미만이면 2등급
# 10 이상 ~ 20 미만이면 3등급
mpg$grade <- ifelse(mpg$avg_mpg >= 30, 1, 
                    ifelse(mpg$avg_mpg >= 20, 2, 3))
head(mpg, n = 20)
tail(mpg)

# 1, 2, 3등급 차량의 개수
table(mpg$grade)   # 도수분포표(frequency table)
hist(mpg$grade)   # 히스토그램
ggplot2::qplot(mpg$grade)

# ifelse(조건식, 조건식이 참일 때 반환할 값, 조건식이 거짓일 때 반환할 값)
x <- 0
result <- ifelse(x > 0, "positive", "negative")
result

result2 <- ifelse(x > 0, "positive",
                  ifelse(x == 0, "zero", "negative"))
result2

# ifelse() 함수는 스칼라와 벡터 모두 적용 가능
df <- data.frame(id = c(1, 2, 3, 4, 5),
                 score = c(100, 95, 62, 72, 86))
df

df$grade1 <- ifelse(df$score >= 80, "Pass", "Fail")
df

df$grade2 <- ifelse(df$score >= 90, "A",
                    ifelse(df$score >= 80, "B", 
                           ifelse(df$score >= 70, "C", 
                                  ifelse(df$score >= 60, "D", "F"))))
df

mpg$grade2 <- ifelse(mpg$avg_mpg >= 30, "A",
                     ifelse(mpg$avg_mpg >= 25, "B",
                            ifelse(mpg$avg_mpg >= 20, "C", "D")))
table(mpg$grade2)
ggplot2::qplot(mpg$grade2)
