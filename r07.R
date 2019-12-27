# dplyr 패키지를 사용한 데이터 전처리(가공)
# 패키지, 라이브러리: 여러 기능(함수) 또는 데이터를 모아둔 꾸러미

search() # 메모리에 로딩된 패키지 이름들을 확인 
library(dplyr) # 패키지(라이브러리)를 메모리에 로딩

# Global Env.에 있는 변수 모두 삭제
rm(list = ls())

# csv 파일을 읽어서 데이터프레임 생성
exam <- read.csv("data/csv_exam.csv")
# 생성된 데이터 프레임의 구조(structure)를 확인
str(exam)

# 1반(class == 1) 학생들의 모든 정보 출력
# filter(데이터프레임, 검색조건)
filter(exam, class == 1)

# dplyr 패키지의 모든 함수는 첫번째 파라미터가 데이터프레임
# 변수 이름은 데이터프레임 이름 생략하고 사용하면 됨

# 1반, 2반 학생들의 정보 출력
filter(exam, class == 1 | class == 2)
filter(exam, class %in% c(1, 2))

# 3반이 아닌 학생들의 정보 출력
filter(exam, class != 3)

# 수학점수가 60점 이상인 학생들 출력
filter(exam, math >= 60)


# dplyr 패키지의 함수들을 호출하는 방법
# 1) function(data_frame, ...)
# 2) data_frame %>% function(...)

# pipe 연산자를 사용한 filter() 함수 호출
exam %>% filter(class == 1)
# Ctrl + Shift + M: 파이프 연산자 작성
exam %>% filter(class %in% c(4, 5))

# 1반 학생들 중에서 수학 점수가 평균 이상인 학생을 출력
exam %>% filter(class == 1 & math >= mean(math))

# filter 함수의 결과를 변수에 저장 -> 데이터프레임
# 1반 학생들로만 이루어진 데이터프레임
class1 <- exam %>% filter(class == 1)
str(class1)
# 1반 학생들의 각 과목 평균 점수
mean(class1$math)
mean(class1$english)
mean(class1$science)

# mpg 데이터를 이용해 분석 문제를 해결해 보세요.
# Q1. 자동차 배기량에 따라 고속도로 연비가 다른지 알아보려고 합니다. 
#     displ(배기량)이 4 이하인 자동차와 5 이상인 자동차 중 어떤 자동차의 hwy(고속도로 연비)가 평균적으로 더 높은지 알아보세요.
mpg <- as.data.frame(ggplot2::mpg)
low_displ <- mpg %>% filter(displ <= 4)
high_displ <- mpg %>%  filter(displ >= 5)
mean(low_displ$hwy)
mean(high_displ$hwy)
# Q2. 자동차 제조 회사에 따라 도시 연비가 다른지 알아보려고 합니다.
#     "audi"와 "toyota" 중 어느 manufacturer(자동차 제조 회사)의 cty(도시 연비)가 평균적으로 더 높은지 알아보세요.
audi <- mpg %>% filter(manufacturer == "audi")
toyota <- mpg %>% filter(manufacturer == "toyota")
mean(audi$cty)
mean(toyota$cty)
# Q3. "chevrolet", "ford", "honda" 자동차의 고속도로 연비 평균을 알아보려고 합니다. 이 회사들의 자동차를 추출한 뒤 hwy 전체 평균을 구해보세요.
chev <- mpg %>% filter(manufacturer == "chevrolet")
ford <- mpg %>% filter(manufacturer == "ford")
honda <- mpg %>% filter(manufacturer == "honda")
mean(chev$hwy)
mean(ford$hwy)
mean(honda$hwy)


# 데이터프레임에서 변수를 선택하는 함수 - dplyr::select)(
# 1) select(data_frame, columns)
# 2) data_frame %>% select(columns)

# exam 데이터프레임에서 수학점수만 선택
exam %>% select(math)
# id와 math 변수를 선택
exam %>% select(id, math)
exam %>% select(c(id, math))

# class 변수를 제외한 모든 변수를 선택
exam %>% select(id, math, english, science)
exam %>% select(-class)

# 파이프 연산자( %>% )를 이용한 함수 연쇄 호출(chain call)
# 1반 학생들의 id, math를 출력
# select id, math from exam where class = 1;
# 1) 1반 학생들만 있는 데이터프레임
class1 <- exam %>% filter(class == 1)
class1
class1 %>% select(id, math)

exam %>% filter(class == 1) %>% select(id, math)

exam %>% select(id, math) %>% filter(class == 1)            # 오류: 중간에 class를 select하지 않았기 때문에 뒤에서 필터링이 안됨
exam %>% select(id, class, math) %>% filter(class == 1)     # it works: 그러나 대게는 filter를 먼저 하는 게 효율적임

# math, english, science를 출력, 앞에 있는 6건만 출력
exam %>% select(math, english, science) %>% head()
exam %>% head() %>% select(math, english, science)          # 데이터가 100만건이면 이쪽이 더 빠르지 않겠니?

# mpg 데이터를 이용해서 분석 문제를 해결해보세요.
# Q1. mpg 데이터는 11개 변수로 구성되어 있습니다. 이 중 일부만 추출해서 분석에 활용하려고 합니다.
#     mpg 데이터에서 class(자동차 종류), cty(도시 연비) 변수를 추출해 새로운 데이터를 만드세요. 새로 만든 데이터의 일부를 출력해서 두 변수로만 구성되어 있는지 확인하세요.
mpg2 <- mpg %>% select(class, cty)
head(mpg2)
# Q2. 자동차 종류에 따라 도시 연비가 다른지 알아보려고 합니다.
#     앞에서 추출한 데이터를 이용해서 class(자동차 종류)가 "suv"인 자동차와 "compact"인 자동차 중 어떤 자동차의 cty(도시 연비)가 더 높은지 알아보세요.
mean((mpg2 %>% filter(class == "suv"))$cty)
mean((mpg2 %>% filter(class == "compact"))$cty)


# 정렬: dplyr::arrange(data_frame, 정렬 기준 컬럼들)
exam %>% arrange(math)    # 정렬 기본 방식은 오름차순
# 내림차순 정렬을 할 때는 desc() 함수 이용
exam %>% arrange(desc(math))

exam %>% arrange(math) %>% head()
exam %>% arrange(desc(math)) %>% head()


# 2개 이상의 변수로 정렬
# class 순서 -> math 점수 순서
exam %>% arrange(class, math)
exam %>% arrange(class, desc(math))

# "audi"에서 생산한 자동차 중에 어떤 자동차 모델의 hwy(고속도로 연비)가 높은지 알아보려고 합니다.
#   "audi"에서 생산한 자동차 중 hwy가 1~5위에 해당하는 자동차의 데이터를 출력하세요.
mpg %>% filter(manufacturer == "audi") %>% arrange(desc(hwy)) %>% head(n = 5)


# 새로운 변수(컬럼) 만들기:
# data_frame$new_var = 식; --> 데이터프레임이 변경됨
exam2 <- exam
exam2
exam2$total <- exam2$math + exam2$english + exam2$science
exam2

# dplyr::mutate(data_frame, new_var = 식)
# 원본 데이터프레임은 수정되지 않고, 컬럼이 추가된 새로운 데이터프레임이 리턴(반환)
exam %>% mutate(total = math + science + english)
exam3 <- exam %>% mutate(total = math + science + english)
exam3

# exam 데이터 프레임에서 수학/영어/과학 점수의 합계를 total,
# 세 과목 점수의 평균을 average
# 변수를 추가한 결과를 출력(만)
exam %>% mutate(total = math + english + science, average = round(total / 3, 2))
# ceiling: 올림한 정수 -> 정수 반환
# floor: 내림한 정수 -> 정수 반환
# trunc: 소숫점 자름 -> 정수 반환
# round: 반올림 -> 소숫점 이하 자릿수를 지정

# 과학점수 60점 이상이면 "pass", 60점 미만이면 "fail" 값을 갖는 컬럼(변수) test를 추가
exam %>% mutate(test = ifelse(science >= 60, "pass", "fail"))


# 세 과목 모두 50점 이상 / 평균 60점 이상이어야 "pass", 그렇지 않으면 "fail"인 값으로 변수(컬럼) 추가
exam %>% mutate(test = ifelse(math < 50, "fail", 
                              ifelse(english < 50, "fail",
                                     ifelse(science < 50, "fail",
                                            ifelse((math + english + science) / 3 < 60, "fail", "pass")))))
exam %>% mutate(test = ifelse(math >= 50 & 
                                english >= 50 & 
                                science >= 50 & 
                                (math + english + science) / 3 >= 60, "pass", "fail"))

# mpg 데이터는 연비를 나타내는 변수가 hwy(고속도로 연비), cty(도시 연비) 두 종류로 분리되어 있습니다.
#   두 변수를 각각 활용하는 대신 하나의 통합 연비 변수를 만들어 분석하려고 합니다.
# Q1. mpg 데이터 복사본을 만들고, cty와 hwy를 더한 '합산 연비 변수'를 추가하세요.
mpg2 <- mpg %>% mutate(sum_mpg = cty + hwy)
mpg2 %>% head
# Q2. 앞에서 만든 '합산 연비 변수'를 2로 나눠 '평균 연비 변수'를 추가세요.
mpg2 <- mpg2 %>% mutate(avg_mpg = sum_mpg / 2)
mpg2 %>% head
# Q3. '평균 연비 변수'가 가장 높은 자동차 3종의 데이터를 출력하세요. 
mpg2 %>% arrange(desc(avg_mpg)) %>% head(n=3)
# Q4. 1~3번 문제를 해결할 수 있는 하나로 연결된 dplyr 구문을 만들어 출력하세요. 데이터는 복사본 대신 mpg 원본을 이용하세요.
mpg %>% mutate(avg_mpg = (cty + hwy) / 2) %>% arrange(desc(avg_mpg)) %>% head(n=3)



summary(exam)   # 기술 통계량을 출력
# dplyr::summarise(), dplyr::summarize()
# 벡터를 스칼라로 만들어줌 - 통계 함수를 적용하기 위해서
exam %>% summarise(m_math = mean(math))
mean(exam$math)

exam %>% summarise(mean = mean(math), sd = sd(math))
mean(exam$math)   # 평균
sd(exam$math)     # 표준편차(standard deviation)

# 반별로 수학 점수의 평균
class1 <- exam %>% filter(class == 1)
mean(class1$math)

exam %>% group_by(class) %>% summarise(mean_math = mean(math), mean_english = mean(english)) %>% arrange(mean_english)


mpg %>% group_by(manufacturer, drv) %>% summarise(mean = mean(cty)) %>% head(10)

mpg %>% group_by(drv, manufacturer) %>% summarise(mean = mean(cty)) %>% head(10)

str(mpg)

# Q1. mpg 데이터의 class는 "suv", "compact" 등 자동차를 특징에 따라 일곱 종류로 분류한 변수입니다. 어떤 차종의 연비가 높은지 비교해보려고 합니다. class별 cty 평균을 구해보세요. 
mpg %>% group_by(class) %>% summarise(mean = mean(cty))
# Q2. 앞 문제의 출력 결과는 class 값 알파벳 순으로 정렬되어 있습니다. 어떤 차종의 도시 연비가 높은지 쉽게 알아볼 수 있도록 cty 평균이 높은 순으로 정렬해 출력하세요.
mpg %>% group_by(class) %>% summarise(mean = mean(cty)) %>% arrange(desc(mean))
# Q3. 어떤 회사 자동차의 hwy(고속도로 연비)가 가장 높은지 알아보려고 합니다. hwy 평균이 가장 높은 회사 세 곳을 출력하세요. 
mpg %>% group_by(manufacturer) %>% summarise(mean = mean(hwy)) %>% arrange(desc(mean)) %>% head(3)
# Q4. 어떤 회사에서 "compact"(경차) 차종을 가장 많이 생산하는지 알아보려고 합니다. 각 회사별 "compact" 차종 수를 내림차순으로 정렬해 출력하세요.
mpg %>% filter(class == "compact") %>% group_by(manufacturer) %>% summarise(num = n()) %>% arrange(desc(num))


mpg <- as.data.frame(ggplot2::mpg)
# dyplr::n()
# summarise, filter, mutate 함수 안에서 사용
# 개수를 리턴하는 함수
# Oracle에서 count 함수와 비슷한 역할
# select count(*) from emp;

library(dplyr)
mpg %>% summarise(n())

# 제조사별 자동차 모델 개수
mpg %>% group_by(manufacturer) %>% summarise(n())

# 자동차 종류(class)별 자동차 모델 개수
mpg %>% group_by(class) %>% summarise(n())

mpg %>% group_by(manufacturer) %>% summarise(mean = mean(hwy)) %>% arrange(desc(mean)) %>% head(3)
