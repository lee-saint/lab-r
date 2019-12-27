# 데이터 가공(전처리)

# csv 파일을 읽어서 데이터프레임 생성
exam <- read.csv("data/csv_exam.csv")
#데이터프레임을 출력
exam[1, 1]   # 데이터프레임[행번호, 열번호]
exam[1, 5]   # 1번 학생(row=1)의 과학 점수(column=5)
# 행 번호 또는 열 번호를 생략하면 행/열을 전체 출력
exam[1, ]    # 컬럼 인덱스를 생략하면 해당 행의 모든 컬럼을 출력
exam[, 1]    # 행 인덱스를 생략하면 해당 컬럼의 모든 행을 출력
# 모든 학생의 영어 점수를 출력
exam[, 4]    # select english
# 5번 학생의 모든 정보를 출력
exam[5, ]    # select from exam where id = 5
# 여러 개의 행/열을 한번에 추출
exam[1:4, ]
exam[, 3:5]
exam[seq(1, 20, 4), ]
exam[c(1, 10, 20), ]

# 논리값(TRUE/FALSE)으로 데이터 추출
exam$class == 1
exam[exam$class == 1, ]
# & (and), | (or)
exam[(exam$class == 1) | (exam$class == 2), ]
# %in% 연산자
exam[exam$class %in% c(3, 4), ]

# 수학 점수가 50점 이상인 학생들의 id, class, math를 출력
exam[exam$math >= 50, 1:3]

# 컬럼 이름이 있는 경우 컬럼 이름으로도 데이터 추출 가능
# 과학 점수가 60점 이상인 학생들의 id, class, science를 출력
exam[exam$science >= 60, c("id", "class", "science")]

# 영어 점수가 평균 이상인 학생들의 id, class, english를 출력
exam[exam$english >= mean(exam$english), c("id", "class", "english")]


# ggplot2::mpg 데이터프레임 사용
mpg <- as.data.frame(ggplot2::mpg)

# mpg 데이터프레임에서 cty, hwy 출력(1~6번 행만)
head(mpg[, c("cty", "hwy")])
mpg[1:6, c("cty", "hwy")]

# mpg 데이터프레임에서 시내주행 연비가 평균 이상이고, 고속도로 연비도 평균 이상인 자동차의 model, cty, hwy 출력
mpg[(mpg$cty >= mean(mpg$cty)) & (mpg$hwy >= mean(mpg$hwy)), c("model", "cty", "hwy")]


# exam 데이터프레임에서 1반 학생들의 수학점수 평균
mean(exam$math)  # 전체 학생 수학 평균
class1 <- exam[exam$class == 1, ]   # 1반 학생들로만 이루어진 데이터프레임
mean(class$math)  # 1반 학생들의 수학 점수 평균
mean(exam[exam$class == 1, ]$math)