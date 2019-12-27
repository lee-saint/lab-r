# 데이터 합치기
# 1) 가로로 합치기 - 컬럼을 이어붙이기
#   관계형 데이터베이스(Oracle)처럼여러 테이블에 나뉘어 있는 데이터를 join 해서 하나의 데이터프레임으로 만드는 것
# 2) 세로로 합치기 - 행을 이어붙이기
#   어떤 데이터가 행의 개수가 너무 많아서 여러개의 파일로 쪼개져 있을 경우, 다시 하나의 데이터터프레임으로 합치는 것

# join
# 중간고사 성적
midterm <- data.frame(id = c(1, 2, 3),
                      mid = c(99, 88, 100))
midterm
 #기말고사 성적
final <-data.frame(id = c(1, 2, 3),
                   final = c(95, 95, 50))
final
# 두개의 데이터프레임을 join해서 하나의 데이터프레임으로 생성
total <- left_join(midterm, final, by="id")
total

# join의 종류
df1 <- data.frame(id = c(1, 2, 3, 4), var1 = c(11, 22, 33, 44))
df1
df2 <- data.frame(id = c(1, 2, 3, 5), var2 = c(50, 60, 70, 80))
df2
# inner join
inner_join(df1, df2, by="id")
# left join
left_join(df1, df2, by="id")
# right join
right_join(df1, df2, by="id")
# full join
full_join(df1, df2, by="id")
# NA(Not Available): 값이 없음

# csv 파일을 읽어 데이터프레임 생성
exam <- read.csv("data/csv_exam.csv")
head(exam)

# 각 반 선생님 이름을 저장하고 있는 데이터프레임
teachers <- data.frame(class = c(1:5), teacher = c("aa", "bb", "cc", "dd", "ee"))
teachers

# exam과 teachers를 합쳐서 하나의 데이터프레임을 생성
exam %>% left_join(teachers, by="class")


# 데이터프레임을 세로로 합치기(행 추가하기)
group1 <- data.frame(id = c(1:3), data = c(10, 20, 30))
group1
group2 <- data.frame(id = c(4:8), data = seq(40, 80, 10))
group2

# group1과 group2를 세로로 이어붙이기
group_total <- bind_rows(group1, group2)
group_total

g1 <- data.frame(id = c(1:5), var1 = seq(10, 50, 10), var2 = seq(11, 15))
g1
g2 <- data.frame(id = c(6:10), var2 = c(55, 66, 77, 88, 99), var3 = c(12, 34, 56, 78, 90))
g2

g1 %>% bind_rows(g2)

# Q1. mpg 데이터에는 연료 종류를 나타낸 fl 변수는 있지만 연료 가격을 나타낸 변수는 없습니다.
#   위에서 만든 fuel 데이터를 이용해서 mpg 데이터에 price_fl(연료 가격) 변수를 추가하세요.
fuel <- data.frame(fl = c("c", "d", "e", "p", "r"),
                   price_fl = c(2.35, 2.38, 2.11, 2.76, 2.22),
                   stringsAsFactors = F)
fuel
mpg %>% left_join(fuel, by="fl")
# Q2. 연료 가격 변수가 잘 추가됐는지 확인하기 위해서 model, fl, price_fl 변수를 추출해 앞부분 5행을 출력해 보세요.
mpg %>% left_join(fuel, by="fl") %>% select(model, fl, price_fl) %>% head(5)


# 문제1. popadults는 해당 지역의 성인 인구, poptotal은 전체 인구를 나타냅니다. midwest 데이터에 '전체 인구 대비 미성년 인구 백분율' 변수를 추가하세요.
midwest <- as.data.frame(ggplot2::midwest)
midwest <- midwest %>% mutate(minor_perc = (poptotal - popadults) / poptotal)
midwest %>% head()
# 문제2. 미성년 인구 백분율이 가장 높은 상위 5개 county(지역)의 미성년 인구 백분율을 출력하세요.
midwest %>% arrange(desc(minor_perc)) %>% head(5) %>% select(county, minor_perc)
# 문제3. 분류표의 기준에 따라 미성년 비율 등급 변수를 추가하고, 각 등급에 몇 개의 지역이 있는지 알아보세요.
midwest <- midwest %>% mutate(minor_perc_grade = ifelse(minor_perc >= 0.4, "large", 
                                                        ifelse(minor_perc >= 0.3, "middle", "small")))
table(midwest$minor_perc_grade)
# 문제4. popasian은 해당 지역의 아시아인 인구를 나타냅니다.
#   '전체 인구 대비 아시아인 인구 백분율' 변수를 추가하고, 하위 10개 지역의 state(주), county(지역명), 아시아인 인구 백분율을 출력하세요.
midwest <- midwest %>% mutate(asian_perc = popasian / poptotal)
midwest %>% arrange(asian_perc) %>% head(10) %>% select(state, county, asian_perc)
