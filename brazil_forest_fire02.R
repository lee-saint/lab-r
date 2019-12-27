library(ggplot2)
library(brazilmaps)
library(sf)
library(anytime)
library(dplyr)
library(readxl)
library(psych)
library(sp)
library(maptools)
library(rgeos)
library(ggvis)
install.packages("rgeos", type = "source")
install.packages("rgdal", type = "source")
install.packages("gpclib", type = "source")
install.packages("ggvis")
install.packages("maptools")
install.packages("reshape2")
gpclibPermit()

# 주별 브라질 지도 데이터
brmap <- get_brmap(geo = "State")
plot_brmap(brmap)
brmap %>% group_by(id, group) %>% ggvis(~long, ~lat) %>% layer_paths(strokeOpacity:=0.15) %>% hide_legend("fill") %>% hide_axis("x") %>% hide_axis("y") %>% set_options(width = 400, height = 600, keep_aspect = T)

# 산불 데이터 불러오기
fire <- read.csv("data/forestfire.csv", encoding = "UTF-8", sep = "\t", stringsAsFactors = F)
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

# 중복 데이터 삭제
table(fire$State)
fire %>% filter(Month == 1, State == "Alagoas")
fire[which(fire$Year == 2017 & fire$Month == 1 & fire$State == "Alagoas"), ]
fire <- fire[-260, ]
table(fire$State)

# 데이터 살펴보기
ggplot(data = fire, mapping = aes(Number)) + geom_histogram(binwidth = 100)
ggplot(data = fire, mapping = aes(x = Year, y = Number)) + geom_col()
ggplot(data = fire, mapping = aes(x = Month, y = Number)) + geom_col()
ggplot(data = fire, mapping = aes(x = State, y = Number)) + geom_boxplot()

# 월별 중앙값 구해서 그래프
fire_month <- fire %>% group_by(Month) %>% summarise(med = median(Number))
ggplot(data = fire_month, mapping = aes(x = Month, y = med)) + geom_col()

# 연도별 중앙값 구해서 그래프
fire_year <- fire %>% group_by(Year) %>% summarise(med = median(Number))
ggplot(data = fire_year, mapping = aes(x = Year, y = med)) + geom_col()


# ---------------------------- 지도와 함께 -----------------------------

# 주 값을 지도의 주 값과 동일한 팩터로 변경
levels(brmap$nome)
unique(fire$State)
fire$State <- factor(x = fire$State, levels = unique(fire$State), labels = levels(brmap$nome))
str(fire$State)

# 주별 중앙값 구해서 지도에 표시
fire_med <- fire %>% group_by(State) %>% summarise(med = median(Number))
plot_brmap(map = brmap, data_to_join = fire_med, join_by = c("nome" = "State"), var = "med") + scale_fill_continuous(low = "yellow", high = "darkred")

# ---------------------------- 산불 + 날씨 -----------------------------

# 주별 날씨 수치 월평균 구해서 하나의 데이터프레임으로
state_names = unique(fire$State)
# 빈 데이터프레임 초기화
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
  
  # rbind로 추가
  weather_merge <- rbind(weather_merge, month_avg_bound)
}


# join을 위해 주 이름 팩터화
weather_merge$state <- factor(x = weather_merge$state, levels = state_names,
                              labels = levels(fire$State))
table(weather_merge$state)

# join
fire_weather <- left_join(fire, weather_merge, by = c("State" = "state", "Year" = "year", "Month" = "month"))
str(fire_weather)

# NA값 없애기
fire_weather <- na.omit(fire_weather)
table(fire_weather$State)

# save
save(fire_weather, file = "data/fire_weather.rda")

# 온도 25 이상 변수 추가
fire_weather$temp25 <- ifelse(fire_weather$temp >= 25, 1, 0)
str(fire_weather)

# 각 수치별 연관성 확인
pairs.panels(fire_weather[c("Number", "temp", "humidity", "dewpoint", "pressure", "wind", "radiant", "rainfall")])
pairs.panels(fire_weather[c("Number", "temp", "humidity", "temp_diff", "temp_depr", "temp25")])

# --------------------------------------- 모델 훈련 ----------------------------------
# 선형 회귀
fire_reg <- lm(Number ~ Month + temp + temp_diff + humidity + dewpoint + temp_depr + pressure + wind + radiant + rainfall, data = fire_weather)

fire_reg
summary(fire_reg)

# 모델 개선
fire_weather$temp2 <- fire_weather$temp ^ 2
fire_weather$humid50 <- ifelse(fire_weather$humidity >= 50, 1, 0)
fire_weather$humid2 <- fire_weather$humidity ^ 2

fire_reg2 <- lm(Number ~ Month + temp + temp2 + temp_diff + temp_diff*humidity + dewpoint + temp_depr + pressure + humidity*wind + temp_diff * wind + radiant + rainfall, data = fire_weather)
fire_reg2
summary(fire_reg2)



# 회귀 트리
# train/test 데이터 나누기
random_ids <- order(runif(2402))
fire_train <- fire_weather[random_ids[1:1802], ]
fire_test <- fire_weather[random_ids[1803:2402], ]

# 회귀 트리 분류기 구축
library(rpart)
fire_rtree <- rpart(Number ~ temp +  temp_diff + humidity + dewpoint + temp_depr + pressure + wind + radiant + rainfall, data = fire_weather)

library(rpart.plot)
rpart.plot(fire_rtree, digits = 3, type = 4, extra = 101)

# 예측
fire_rpred <- predict(fire_rtree, fire_test)
summary(fire_rpred)
summary(fire_test$Number)

# 평가
cor(fire_rpred, fire_test$Number)  # 0.7520 / 0.8149

MAE <- function(actual, pred) {
  mean(abs(actual - pred))
}
MAE(fire_test$Number, fire_rpred)  # 540.6917 / 405.2557
mean(fire_test$Number)
MAE(fire_test$Number, 540.69)      # 728.8064


# 모델 트리 분류기 구축
library(Cubist)
fire_mtree <- cubist(fire_train[-2], fire_train$Number)
fire_mtree
summary(fire_mtree)

fire_mpred <- predict(fire_mtree, fire_test)
summary(fire_mpred)
summary(fire_test$Number)

# 평가
cor(fire_mpred, fire_test$Number)  # 0.7090 / 0.6333
MAE(fire_test$Number, fire_mpred)  # 365.0042 / 368.9305



# knn 신경망
str(fire_weather)
# 정규화
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
fire_norm <- as.data.frame(lapply(fire_weather[c(4, 6:14)], normalize))
summary(fire_norm)

# train/test 데이터 나누기
fire_ntrain <- fire_norm[random_ids[1:1802], ]
fire_ntest <- fire_norm[random_ids[1803:2402], ]
str(fire_ntest)

# 모델 훈련
library(neuralnet)
fire_nmodel <- neuralnet(Number ~ ., data = fire_ntrain, stepmax = 10000000)
plot(fire_nmodel)

# 평가
fire_nresult <- compute(fire_nmodel, fire_ntest[-1])
fire_npred <- fire_nresult$net.result
cor(fire_npred, fire_ntest$Number)  # 0.5827 / 0.5793
MAE(fire_npred, fire_ntest$Number)  # 0.0219 / 0.0203

# 성능개선
fire_nmodel2 <- neuralnet(Number ~ ., data = fire_ntrain, stepmax = 10000000, hidden = c(5, 3))
fire_nresult2 <- compute(fire_nmodel2, fire_ntest[-1])
fire_npred2 <- fire_nresult2$net.result
cor(fire_npred2, fire_ntest$Number)  # 0.6673 / 0.6768
MAE(fire_npred2, fire_ntest$Number)  # 0.0186 / 0.0192

# 역정규화
denorm <- function(norm, denorm) {
  return(norm * (max(denorm) - min(denorm)) + min(denorm))
}
fire_npred_denorm <- denorm(fire_npred2, fire_weather$Number)
MAE(fire_test$Number, fire_npred_denorm)  # 465.9977 / 480.2003




# SVM
library(kernlab)
str(fire_train)
fire_svm <- ksvm(Number ~ ., data = fire_train, kernal = "rbfdot")

fire_svm
fire_svm_pred <- predict(fire_svm, fire_test)
cor(fire_svm_pred, fire_test$Number)  # 0.6589 / 0.7071
MAE(fire_svm_pred, fire_test$Number)  # 411.3566 / 377.5542






# 모델 성능 개선
library(caret)
modelLookup("cubist")
modelLookup()[-c(1:332), ]
str(fire_weather)
fire_weather_num <- fire_weather[c(4, 6:14)]
str(fire_weather_num)
m <- train(Number ~ ., data = fire_weather_num, method = "cubist")
m

ctrl <- trainControl(method = "cv", selectionFunction = "oneSE")
m <- train(Number ~ ., data = fire_weather_num, trControl = ctrl, method = "cubist")
m

install.packages("ipred")
library(ipred)
mybag <- bagging(Number ~ ., data = fire_weather_num)
mybag
summary(mybag)
fire_bagpred <- predict(mybag, fire_weather_num)
summary(fire_bagpred)
MAE(fire_bagpred, fire_weather_num$Number)

modelLookup("treebag")
ctrl <- trainControl(method = "cv")
train(Number ~ ., data = fire_weather_num, method = "treebag", trControl = ctrl)

bagctrl <- bagControl(fit = svmBag$fit, predict = svmBag$pred, aggregate = svmBag$aggregate)
svmbag <-train(Number ~ ., data = fire_weather_num, "bag", trControl = ctrl, bagControl = bagctrl)
svmbag
warnings()

library(randomForest)
rf <- randomForest(Number ~ ., data = fire_number)
rf



ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
grid_rf <- expand.grid(.mtry = c(2, 4, 8, 16))
m_rf <- train(Number ~ ., data = fire_weather_num, method = "rf", trControl = ctrl, tuneGrid = grid_rf)
warnings()

grid_cubist <- expand.grid(.committees = 10, .neighbors = c(0, 5, 9))
m_c50 <- train(Number ~ ., data = fire_weather_num, method = "cubist", trControl = ctrl, tuneGrid = grid_cubist)

m_rf
m_c50

# ------------------------------- R Shiny -------------------------------------

install.packages("rmarkdown")
install.packages("shiny")

library(shiny)
ui <- fluidPage(
  sliderInput(inputId = "num", label = "Choose a number", value = 25, min = 1, max = 100),
  plotOutput("hist")
)
server <- function(input, output) {
  output$hist <- renderPlot({
    hist(rnorm(input$num))
  })
}
shinyApp(ui = ui, server = server)

ui <- fluidPage(
  actionButton(inputId = "clicks", label = "Click me")
)

server <- function(input, output) {
  observeEvent(input$clicks, {
    print(as.numeric(input$clicks))
  })
}

shinyApp(ui = ui, server = server)

library(ggvis)
mtcars %>% ggvis(~wt, ~mpg, fill = ~vs) %>% layer_points()

df <- fire_weather["temp"]
names(df) = "y"
df
str(df)
df %>% ggvis(~y) %>% layer_histograms()

regions <- data.frame(Region = rep(c("North", "Northeast", "Central-West", "Southeast", "South"), times = c(7, 9, 4, 4, 3)),
                      State = c("Acre", "Amapa", "Amazonas", "Para", "Rondonia", "Roraima", "Tocantins",
                                "Alagoas", "Bahia", "Ceara", "Maranhao", "Paraiba", "Pernambuco", "Piaui", "Rio Grande do Norte", "Sergipe",
                                "Goias", "Mato Grosso", "Mato Grosso do Sul", "Distrito Federal",
                                "Espirito Santo", "Minas Gerais", "Rio de Janeiro", "Sao Paulo",
                                "Parana", "Rio Grande do Sul", "Santa Catarina"),
                      stringsAsFactors = F)

fire <- fire %>% left_join(regions, by = "State")
head(fire[-5])
fire <- fire[-5]
head(fire_weather[6:12])
unique(weather_merge$state)
unique(fire$State)

str(fire$State)
fire$State <- factor(fire$State)

install.packages("knitr")
knitr::knit2html("Brazil_Forest_Fire/FF.Rmd")
rmarkdown::render("Brazil_Forest_Fire/FF.Rmd")
