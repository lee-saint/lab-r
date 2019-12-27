# 복지패널 데이터 분석

# 필요한 라이브러리 로드
library(dplyr)
library(ggplot2)
library(ggthemes)
search()

# 저장해둔 R 데이터 파일을 로드 -> 변수 자동 생성
load("data/welfare.rda")
# 데이터프레임 확인
str(welfare)
welfare %>% select(code_job, job) %>% head(10)

table(is.na(welfare$job))

# 직업별 평균 소득 분석
income_by_job <- welfare %>% filter(!is.na(job) & !is.na(income)) %>% 
  group_by(job) %>% summarise(count = n(), mean_income = mean(income))

income_by_job

# 평균 소득 상위 10개 직종
top10 <- income_by_job %>% arrange(desc(mean_income)) %>% head(10)
top10

# 평균 소득 하위 10개 직종
bottom10 <- income_by_job %>% arrange(mean_income) %>% head(10)
bottom10

# 직종별 평균 급여 상/하위 10개를 그래프
ggplot(data = top10, mapping = aes(x = reorder(job, mean_income), y = mean_income)) + geom_col() + coord_flip() + theme_economist() + xlab("job")

ggplot(data = bottom10, mapping = aes(x = reorder(job, -mean_income), y = mean_income)) + geom_col() + coord_flip() + theme_economist() + xlab("job")

# 직종별 인구수가 30명 이상인 직종에 대해서 평균 소득 상위 10개 직종을 찾고, 그래프 작성
over_30_top10 <- income_by_job %>% filter(count >= 30) %>% arrange(desc(mean_income)) %>% head(10)
over_30_top10

ggplot(data = over_30_top10, mapping = aes(x = reorder(job, mean_income), y = mean_income)) + geom_col() + coord_flip() + xlab("job") +theme_minimal()



# 지역별 연령대 비율
# 지역(1~7권역) 변수 확인 - NA 없음
table(welfare$code_region)
# 연령대(ageg, age_range)
table(welfare$ageg)
table(welfare$age_range)

# 지역 코드 - 해당 지역 이름 데이터프레임
regions <- data.frame(
  code_region = c(1:7),
  region = c("서울", "수도권/인천/경기", "부산/경남/울산", "대구/경북", "대전/충남", "강원/충북", "광주/전남/전북/제주")
)
regions

welfare <- left_join(welfare, regions, by = "code_region")
table(welfare$region)

# 지역별, 연령대별 인구수
region_ageg <- welfare %>% group_by(region, ageg) %>% summarise(count = n()) %>% mutate(total = sum(count), pct = (count / total) * 100)
region_ageg

ggplot(data = region_ageg, mapping = aes(x = region, y = pct, fill = ageg)) + geom_col() + coord_flip()

region_agerange <- welfare %>% group_by(region, age_range) %>% summarise(count = n()) %>% mutate(total = sum(count), pct = (count / total) * 100)
region_agerange

ggplot(data = region_agerange, mapping = aes(x = region, y = pct, fill = age_range)) + geom_col() + coord_flip() + scale_fill_solarized() + theme_solarized()



# 종교 여부에 따른 이혼율
class(welfare$religion)
table(welfare$religion)

# 종교 여부 팩터화
welfare$religion <- factor(welfare$religion, levels = 1:2, labels = c("yes", "no"))
table(welfare$religion)
ggplot(data = welfare, mapping = aes(x = religion)) + geom_bar()


# 혼인 상태 변수: marriage
class(welfare$marrige)
table(welfare$marrige)

# 이혼 여부 변수
welfare$group_marriage <- ifelse(welfare$marrige == 1, "married", 
                                 ifelse(welfare$marrige == 3, "divorced", NA))
table(welfare$group_marriage)
table(is.na(welfare$group_marriage))
ggplot(data = welfare, mapping = aes(x = group_marriage)) + geom_bar()

# 종교 유무에 따른 이혼율 표 만들기
religion_marriage <- welfare %>% filter(!is.na(group_marriage)) %>%
  group_by(religion,group_marriage) %>% summarise(count = n()) %>% mutate(total = sum(count), pct = count/total * 100)
religion_marriage

divorce <- religion_marriage %>% filter(group_marriage == "divorced") %>% select(religion, pct)
divorce
ggplot(data = divorce, mapping = aes(x = religion, y = pct)) + geom_col()


# 연령대별 이혼율
ageg_marriage = welfare %>% filter(!is.na(group_marriage)) %>% group_by(ageg, group_marriage) %>% summarise(count = n()) %>% mutate(total = sum(count), pct = count/total * 100)
ageg_marriage

ageg_divorce <- ageg_marriage %>% filter(group_marriage == "divorced") %>% select(ageg, pct)
ageg_divorce
ggplot(data = ageg_divorce, mapping = aes(x = ageg, y = pct)) + geom_col()

# 연령대별 종교별 이혼율
ageg_religion_marriage <- welfare %>% filter(!is.na(group_marriage)) %>% group_by(ageg, religion, group_marriage) %>% summarise(count = n()) %>% mutate(total = sum(count), pct = count/total * 100)
ageg_religion_marriage

ageg_religion_divorce <- ageg_religion_marriage %>% filter(group_marriage == "divorced") %>% select(ageg, religion, pct)
ageg_religion_divorce
ggplot(data = ageg_religion_divorce, mapping = aes(x = ageg, y = pct, fill = religion)) + geom_col(position = "dodge")

save(welfare, file = "data/welfare2.rda")
load("data/welfare2.rda")
library(ggplot2)
