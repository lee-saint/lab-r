library(ggplot2)
library(brazilmaps)
library(sf)

# 주별 브라질 지도 데이터
brmap <- get_brmap(geo = "State")
plot_brmap(brmap)

# 산불 데이터 불러오기
forestfire <- read.csv("data/amazon_1.csv", encoding = "UTF-8", stringsAsFactors = F)
str(forestfire)
table(forestfire$month)
table(forestfire$state)
str(forestfire$state)
head(forestfire)
summary(forestfire$number)


# 새로운 파일을 구해왔다
fire <- read.csv("data/forestfire.csv", encoding = "UTF-8", sep = "\t", stringsAsFactors = F)
str(fire)
fire$Date <- utcdate(fire$Date)
str(fire)

fire$Month <- factor(x = fire$Month, 
                     levels = c("Janeiro", "Fevereiro", "Marco", "Abril", 
                                 "Maio", "Junho", "Julho", "Agosto", 
                                 "Setembro", "Outubro", "Novembro", "Dezembro"),
                     labels = c(1:12),
                     ordered = T)
fire$Month <- as.numeric(fire$Month)
table(fire$Month)

table(fire$State)
library(dplyr)
fire %>% filter(Month == 1, State == "Alagoas")
fire[which(fire$Year == 2017 & fire$Month == 1 & fire$State == "Alagoas"), ]
fire <- fire[-260, ]
table(fire$State)

ggplot(data = fire, mapping = aes(x = Year, y = Number)) + geom_col()
ggplot(data = fire, mapping = aes(x = Month, y = Number)) + geom_col()
ggplot(data = fire, mapping = aes(Number)) + geom_histogram(binwidth = 100)
ggplot(data = fire, mapping = aes(x = State, y = Number)) + geom_boxplot()

fire_month <- fire %>% group_by(Month) %>% summarise(med = median(Number))
ggplot(data = fire_month, mapping = aes(x = Month, y = med)) + geom_col()

fire_year <- fire %>% group_by(Year) %>% summarise(med = median(Number))
ggplot(data = fire_year, mapping = aes(x = Year, y = med)) + geom_col()

# 날짜 컬럼 factor -> 날짜 형식으로 변환
install.packages("anytime")
library(anytime)
forestfire$date <- utcdate(forestfire$date)
str(forestfire)

# 달 컬럼 순서화
forestfire$month <- factor(x = forestfire$month, 
                           levels = c("Janeiro", "Fevereiro", "Mar?", "Abril", 
                                         "Maio", "Junho", "Julho", "Agosto", 
                                         "Setembro", "Outubro", "Novembro", "Dezembro"),
                           labels = c(1:12),
                           ordered = T)
forestfire$month <- as.numeric(forestfire$month)
str(forestfire$month)

# 주-월별 산불 선 그래프?
ggplot(data = forestfire, mapping = aes(x = date, y = number, color = state)) + geom_line()

# 주-년별 산불 선그래프(합계)
library(dplyr)
forestfire_year <- forestfire %>% group_by(year, state) %>% summarise(sum = sum(number)) 
head(forestfire_year)
ggplot(data = forestfire_year, mapping = aes(x = year, y = sum, color = state)) + geom_line()


# ----------------------------------- 산불 데이터 + 지도 -------------------------------------------

# 주 이름 level을 맵데이터 주 이름 level과 맞추기
forestfire$state <- factor(x = forestfire$state,
                           levels = levels(brmap$nome))
levels(forestfire$state)
table(forestfire$state)
table(brmap$nome)

# Par?를 Para로 바꾸기
forestfire$state <- ifelse(forestfire$state == "Par?", "Para", forestfire$state)

# 산불 발생량 전체 합계(지도 표시를 위해)
forestfire_sum <- forestfire %>% group_by(state) %>% summarise(sum = sum(number))
head(forestfire_sum)

# 주별 산불 전체 발생량 지도에 표시
plot_brmap(map = brmap, data_to_join = forestfire_sum, join_by = c("nome" = "state"), var = "sum")




levels(brmap$nome)
unique(fire$State)
fire$State <- factor(x = fire$State, levels = unique(fire$State), labels = levels(brmap$nome))
str(fire$State)

fire_med <- fire %>% group_by(State) %>% summarise(med = median(Number))
plot_brmap(map = brmap, data_to_join = fire_med, join_by = c("nome" = "State"), var = "med") + scale_fill_continuous(low = "yellow", high = "darkred")



# 산불이 일어났다/안일어났다만 따지기
fire$fire <- ifelse(fire$Number > 0, "Y", "N")
fire$fire <- factor(fire$fire, levels = c("Y", "N"))
head(fire)
table(fire$fire)
fire_freq <- fire %>% group_by(State) %>% count(fire)
fire_freq %>% head
ggplot(data = fire_freq, mapping = aes(x = State, y = n, fill = fire)) + geom_bar(stat = "identity", position = "dodge")

fire_month_freq <- fire %>% group_by(Month) %>% count(fire)
head(fire_month_freq)
ggplot(data = fire_month_freq, mapping = aes(x = Month, y = n, fill = fire)) + geom_bar(stat = "identity", position = "dodge")

# ------------------------------------ 산불 데이터 + 날씨 -------------------------------------------

library(dplyr)
library(readxl)

# 연습
# 첫번째 날씨 파일 업로드
mg_weather_2010_1 <- read_xlsx("brazil_weather/Matogrosso_2010_1.xlsx", na = "NULL")
str(mg_weather_2010_1)

# 날짜 행 만들기
dates <- mg_weather_2010_1[2:1827, 1]
dates$...1 <- as.numeric(dates$...1)
dates$...1 <- as.Date(dates$...1, origin="1899-12-30")
dates <- dates %>% rename(date = ...1)
str(dates)

# 날짜 행에 일평균 기온 + 습도+ 이슬점 붙이기
dates$temp <- rowMeans(mg_weather_2010_1[2:1827, 2:25], na.rm = T)
dates$humid <- rowMeans(mg_weather_2010_1[2:1827, 26:49], na.rm = T)
dates$dewpoint <- rowMeans(mg_weather_2010_1[2:1827, 50:73], na.rm = T)
str(dates)

# 두번째 날씨 파일 업로드
mg_weather_2010_2 <- read_xlsx("brazil_weather/Matogrosso_2010_2.xlsx", na = "NULL")

# 날짜 행에 일평균 기압+풍속+복사에너지+강수량 붙이기
dates$pressure <- rowMeans(mg_weather_2010_2[2:1827, 2:25], na.rm = T)
dates$wind <- rowMeans(mg_weather_2010_2[2:1827, 26:49], na.rm = T)
dates$radiant <- rowMeans(mg_weather_2010_2[2:1827, 74:97], na.rm = T)
dates$rainfall <- rowMeans(mg_weather_2010_2[2:1827, 98:121], na.rm = T)
str(dates)

# 월평균 구하기
month_weather <- dates %>% mutate(year = format(date, "%Y"), month = format(date, "%m")) %>% group_by(year, month) %>% summarise_if(is.numeric, mean, na.rm = T) %>% as.data.frame()
month_weather$year <- as.numeric(month_weather$year)
month_weather$month <- as.numeric(month_weather$month)
month_weather$month <- factor(x = month_weather$month, levels = 1:12, ordered = T)
str(month_weather)
month_weather$state <- "MATO GROSSO"
table(month_weather$state)                 





# 반복문만들자 만들자반복문
state_names = c("Acre", "Alagoas", "Amapa", "Amazonas", "Bahia", "Ceara", "Distrito Federal", "Espirito Santo", "Goias", "Maranhao", "Mato Grosso", "Minas Gerais",
                "Para", "Paraiba", "Pernambuco", "Piaui", "Rio", "Rondonia", "Roraima", "Santa Catarina", "Sao Paulo", "Sergipe", "Tocantins")
str(month_avg_bound)
weather_merge <- data.frame(year            = numeric(),
                            month           = numeric(),
                            temp            = numeric(),
                            temp_diff       = numeric(),
                            humidity        = numeric(),
                            dewpoint        = numeric(),
                            temp_depr       = numeric(),
                            pressure        = numeric(),
                            wind            = numeric(),
                            radiant         = numeric(),
                            rainfall        = numeric(),
                            state           = character())
for (name in state_names) {
  filename <- sprintf("brazil_weather/%s_2010_1.xlsx", name)                          # 첫번째 파일이름 포매팅
  weather_2010_1 <- read_xlsx(filename, na = "NULL")                                  # 파일 불러오기
  
  daily_avg <- weather_2010_1[-1, 1]                                                  # 날짜 추출
  daily_avg <- daily_avg %>% rename(date = ...1)
  daily_avg$date <- as.numeric(daily_avg$date)                                        # chr to Date
  daily_avg$date <- as.Date(daily_avg$date, origin = "1899-12-30")
  
  daily_avg$temp <- rowMeans(weather_2010_1[-1, 2:25], na.rm = T)
  daily_avg$temp_diff <- apply(weather_2010_1[-1, 2:25], 1, FUN = function(x) max(x) - min(x))
  daily_avg$humidity <- rowMeans(weather_2010_1[-1, 26:49], na.rm = T)
  daily_avg$dewpoint <- rowMeans(weather_2010_1[-1, 50:73], na.rm = T)
  daily_avg$temp_depr <- rowMeans(weather_2010_1[-1, 2:25] - weather_2010_1[-1, 50:73], na.rm = T)
  
  filename <- sprintf("brazil_weather/%s_2010_2.xlsx", name)                          # 두번째 파일이름 포매팅
  weather_2010_2 <- read_xlsx(filename, na = "NULL")
  
  daily_avg$pressure <- rowMeans(weather_2010_2[-1, 2:25], na.rm = T)
  daily_avg$wind <- rowMeans(weather_2010_2[-1, 26:49], na.rm = T)
  daily_avg$radiant <- rowMeans(weather_2010_2[-1, 74:97], na.rm = T)
  daily_avg$rainfall <- rowMeans(weather_2010_2[-1, 98:121], na.rm = T)
  
  # 월평균 테이블 만들기
  month_avg1 <- daily_avg %>% mutate(year = format(date, "%Y"), month = format(date, "%m")) %>% group_by(year, month) %>% summarise_if(is.numeric, mean, na.rm = T) %>% as.data.frame()
  
  # 2015 ~ 2017년 데이터로 반복
  filename <- sprintf("brazil_weather/%s_2015_1.xlsx", name)                          # 첫번째 파일이름 포매팅
  weather_2015_1 <- read_xlsx(filename, na = "NULL")                                  # 파일 불러오기
  
  daily_avg <- weather_2015_1[-1, 1]                                                  # 날짜 추출
  daily_avg <- daily_avg %>% rename(date = ...1)
  daily_avg$date <- as.numeric(daily_avg$date)                                        # chr to Date
  daily_avg$date <- as.Date(daily_avg$date, origin = "1899-12-30")
  
  daily_avg$temp <- rowMeans(weather_2015_1[-1, 2:25], na.rm = T)
  daily_avg$temp_diff <- apply(weather_2015_1[-1, 2:25], 1, FUN = function(x) max(x) - min(x))
  daily_avg$humidity <- rowMeans(weather_2015_1[-1, 26:49], na.rm = T)
  daily_avg$dewpoint <- rowMeans(weather_2015_1[-1, 50:73], na.rm = T)
  daily_avg$temp_depr <- rowMeans(weather_2015_1[-1, 2:25] - weather_2015_1[-1, 50:73], na.rm = T)
  
  filename <- sprintf("brazil_weather/%s_2015_2.xlsx", name)                          # 두번째 파일이름 포매팅
  weather_2015_2 <- read_xlsx(filename, na = "NULL")
  
  daily_avg$pressure <- rowMeans(weather_2015_2[-1, 2:25], na.rm = T)
  daily_avg$wind <- rowMeans(weather_2015_2[-1, 26:49], na.rm = T)
  daily_avg$radiant <- rowMeans(weather_2015_2[-1, 74:97], na.rm = T)
  daily_avg$rainfall <- rowMeans(weather_2015_2[-1, 98:121], na.rm = T)
  
  month_avg2 <- daily_avg %>% mutate(year = format(date, "%Y"), month = format(date, "%m")) %>% group_by(year, month) %>% summarise_if(is.numeric, mean, na.rm = T) %>% as.data.frame()
  
  # 두 기간의 월평균 합친 후 후처리(year/month를 숫자로, 주 이름 추가)
  month_avg_bound <- bind_rows(month_avg1, month_avg2)
  month_avg_bound$year <- as.numeric(month_avg_bound$year)
  month_avg_bound$month <- as.numeric(month_avg_bound$month)
  month_avg_bound$state <- name
  
  # 조인이 안된다면...?
  weather_merge <- rbind(weather_merge, month_avg_bound)
}

str(weather_merge)

month_weather <- left_join(forestfire, weather_merge, by = c("state", "year", "month"))
month_weather <- month_weather %>% filter(year >= 2010)
str(month_weather)
month_weather <- as.data.frame(lapply(month_weather, function(x) ifelse(x == "NaN", NA, x)))

save(month_weather, file = "data/forestfire.rda")

library(ggplot2)
ggplot(data = month_weather, mapping = aes(temp, number, color = state)) + geom_point()
ggplot(data = month_weather, mapping = aes(humidity, number, color = state)) + geom_point()
ggplot(data = month_weather, mapping = aes(wind, number)) + geom_point()

fire_weather_nona <- na.omit(month_weather)
summary(fire_weather_nona)

fire_weather_mg <- fire_weather_nona %>% filter(state == "Mato Grosso")
ggplot(data = fire_weather_mg, mapping = aes(humidity, number)) + geom_point()

cor(fire_weather_nona$wind, fire_weather_nona$number)

fire_weather_no0 <- fire_weather_nona %>% filter(number != 0)
summary(fire_weather_no0)


fire_weather_nodup <- fire_weather_no0 %>% filter(!state %in% c("Mato Grosso", "Paraiba", "Rio"))
ggplot(data = fire_weather_nodup, mapping = aes(temp, number, color = state)) + geom_point()
ggplot(data = fire_weather_nodup, mapping = aes(humidity, number, color = state)) + geom_point()
ggplot(data = fire_weather_nodup, mapping = aes(wind, number, color = state)) + geom_point()

ggplot(data = fire_weather_nodup, mapping = aes(x = state, y = number)) + geom_boxplot()

summary(fire_weather_nodup$number)
boxplot(fire_weather_nodup$number)$stats
fire_weather_nooutlier <- fire_weather_nodup %>% filter(number < 335)

ggplot(data = fire_weather_nooutlier, mapping = aes(temp, number, color = state)) + geom_point()
ggplot(data = fire_weather_nooutlier, mapping = aes(humidity, number, color = state)) + geom_point()
ggplot(data = fire_weather_nooutlier, mapping = aes(wind, number, color = state)) + geom_point()

str(fire_weather_nooutlier)

library(psych)
pairs.panels(fire_weather_nooutlier[c("number", "temp", "humidity", "dewpoint", "pressure", "wind", "radiant", "rainfall")])




levels(fire$State)
weather_merge$state <- factor(x = weather_merge$state, levels = c("Acre", "Alagoas", "Amapa", "Amazonas", "Bahia", "Ceara", "Distrito Federal", "Espirito Santo", "Goias", "Maranhao", "Mato Grosso", "Mato Grosso Do Sul", "Minas Gerais",
                                                                  "Para", "Paraiba", "Parana", "Pernambuco", "Piaui", "Rio", "Rio de norte", "Rio de sul", "Rondonia", "Roraima", "Santa Catarina", "Sao Paulo", "Sergipe", "Tocantins"),
                              labels = levels(fire$State))
table(weather_merge$state)
fire_weather <- left_join(fire, weather_merge, by = c("State" = "state", "Year" = "year", "Month" = "month"))
str(fire_weather)
fire_weather <- na.omit(fire_weather)

library(psych)
pairs.panels(fire_weather[c("Number", "temp", "humidity", "dewpoint", "pressure", "wind", "radiant", "rainfall")])

ggplot(data = fire_weather, mapping = aes(temp, Number)) + geom_point()
ggplot(data = fire_weather, mapping = aes(humidity, Number)) + geom_point()

ggplot(data = fire_weather, mapping = aes(x = rainfall)) + geom_histogram()

pairs.panels(fire_weather[c("Number", "temp", "humidity", "temp_diff", "temp_depr")])
