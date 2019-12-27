# 결측치(Missing Value, NA) 처리
rm(list=ls())

# 결측치(NA)가 있는 데이터프레임 생성
df <- data.frame(gender = c("M", "F", NA, "M", "F"),
                 score = c(5, 4, 3, NA, 6))
df
# 데이터프레임의 구조(structure)
str(df)
# 기술 통계량
summary(df)

# is.na(): NA면 T, NA가 아니면 F를 리턴하는 함수
df %>% is.na  # 데이터 프레임의 각 원소의 NA 여부를 출력
is.na(df$gender)

search()
df %>% filter(!is.na(gender))
df %>% filter(!is.na(gender) & !is.na(score))

# df 데이터프레임에서 성별 score의 평균 출력
df %>% filter(!is.na(gender) & !is.na(score)) %>% group_by(gender) %>% summarise(mean(score))

mean(df$score, na.rm = T)

df %>% group_by(gender) %>% summarise(mean(score, na.rm = T))

# table(): 도수분포표
table(is.na(df$gender))
table(is.na(df))

# 결측치를 대체할 때
# 1) 0으로 대체
df$score <- ifelse(is.na(df$score), 0, df$score)
df
df %>% group_by(gender) %>% summarise(mean(score))

# 2) 평균값으로 대체
df[4, 2] = NA
# NA를 제외한 평균값
avg <- mean(df$score, na.rm = T)
avg
df$score <- ifelse(is.na(df$score), avg, df$score)
df
df %>% group_by(gender) %>% summ

mpg <- as.data.frame(ggplot2::mpg)
mpg[c(65, 124, 131, 153, 112), "hwy"] <- NA
# Q1. drv(구동방식)별로 hwy(고속도로 연비) 평균이 어떻게 다른지 알아보려고 합니다. 분석을 하기 전에 우선 두 변수에 결측치가 있는지 확인해야 합니다.
#   drv 변수와 hwy 변수에 결측치가 몇 개 있는지 알아보세요. 
table(is.na(mpg$drv))
table(is.na(mpg$hwy))
# Q2. filter()를 이용해 hwy 변수의 결측치를 제외하고, 어떤 구동방식의 hwy 평균이 높은지 알아보세요. 하나의 dplyr 구문으로 만들어야 합니다.
mpg %>% filter(!is.na(hwy)) %>% group_by(drv) %>% summarise(mean(hwy))


# 이상치: 논리적으로 들어갈 수 없는 값. 있어서는 안 되는 값
movie_rating <- data.frame(rating = c(5, 4, 3, 4, 5, 3, 10))
movie_rating
mean(movie_rating$rating)
# 영화 별점은 1 ~ 5점까지만 가능 / 10점은 있어서는 안될 값(이상치)
movie_rating$rating <- ifelse(movie_rating$rating %in% c(1:5), movie_rating$rating, NA)
movie_rating
mean(movie_rating$rating, na.rm = T)


# outlier
mpg <- as.data.frame(ggplot2::mpg)
summary(mpg$hwy)
boxplot(mpg$hwy)
boxplot(mpg$hwy)$stats
# stats[1, 1]: 아래쪽 수염의 위치
# stats[1, 5]: 위쪽 수염의 위치
# boxplot에서 수염을 벗어난 데이터들을 outlier라고 하고, outlier들을 NA로 처리한 뒤 통계처리
mean(mpg$hwy)  # 23.44017 - outlier가 포함된 값

# outlier를 NA로 바꿔줌
mpg$hwy <- ifelse(mpg$hwy < 12 | mpg$hwy > 37, NA, mpg$hwy)
# NA의 개수를 확인
table(is.na(mpg$hwy))
mean(mpg$hwy, na.rm = T)  # 23.18615 - outlier를 제외한 평균
