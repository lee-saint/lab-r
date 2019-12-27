# 지도 위에 통계값들 표시하기

# ggplot2::map_data() 함수가 지도 데이터를 처리하기 위해 필요한 패키지
install.packages("maps")
# ggplot2::coord_map() 함수가 사용하는 패키지
install.packages("mapproj")
library(ggplot2)

asia_map <- map_data(map = "world", region = c("North Korea", "South Korea", "Japan", "China", "India"))
str(asia_map)
head(asia_map)
tail(asia_map)
# longitude(경도): 영국의 그리니치 천문대를 기준으로 동/서 방향 좌표
# latitude(위도): 적도를 기준으로 남/북 방향 좌표
# group: 함께 연결할 위도/경도 점들의 그룹(나라, 주, 도시, ...)
# order: 위도/경도 점들을 연결할 순서
# region: 지역 이름

# ggplot2 패키지를 사용할 때 data 파라미터는 위도/경도 파라미터를 가지고 있는 지도 데이터를 전달
# x축 파라미터 = 경도(long)
# y축 파라미터 = 위도(lat)
# 그래프릐 종류: 다각형 그래프(polygon)
ggplot(data = asia_map, mapping = aes(x = long, y = lat, group = group, fill = region)) + geom_polygon() + coord_map("polyconic") +
  ggthemes::scale_fill_solarized() + ggthemes::theme_solarized()


# 대한민국 지도
korea_map <- map_data(map = "world", region = c("South Korea", "North Korea"))
head(korea_map)
tail(korea_map)

ggplot(data = korea_map, mapping = aes(x = long, y = lat, group = group, fill = region)) + geom_polygon(color = "white") + coord_map("polyconic") +
  ggthemes::scale_fill_solarized() + ggthemes::theme_solarized()


# 미국 지도
us_state <- map_data(map = "state")
str(us_state)
head(us_state)
tail(us_state)

ggplot(data = us_state, mapping = aes(x = long, y = lat, group = group, fill = region)) + geom_polygon(color = 'black') + coord_map("polyconic")


library(dplyr)
search()

# 데이터프레임 확인
# USArrests: R에서 제공하는 학습용 데이터프레임
str(USArrests)
head(USArrests)
tail(USArrests)
# USArrest 데이터프레임에는 미국 주(state)의 이름들이 각 행의 이름으로 설정되어 있음
# us_map 데이터프레임과 join하기 위해서는 주 이름들이 데이터프레임의 변수로 있어야 함
library(tibble)
us_crime <- rownames_to_column(USArrests, var = "state")
str(us_crime)
head(us_crime)

# 미국 주 이름을 표시하는 us_state의 region 변수는 only 소문자 / us_crime의 state 변수는 첫글자가 대문자
us_crime$state <- tolower(us_crime$state)
head(us_crime)

# 지도/범죄를 합친 데이터프레임(join)
state_crime <- left_join(us_state, us_crime, by = c("region" = "state"))
str(state_crime)
head(state_crime)
tail(state_crime)

ggplot(data = state_crime, mapping = aes(x = long, y = lat, group = group, fill = Murder)) + geom_polygon(color = "white") + coord_map() + 
  scale_fill_continuous(low = "yellow", high = "darkred")

# choropleth map(단계 구분도)
# 지도 위에 통계값들을 색깔로 구분해서 표현하는 방법
# 인구, 질병, 범죄, ...

install.packages("ggiraphExtra")
library(ggiraphExtra)

ggChoropleth(data = us_crime, mapping = aes(fill = Murder, map_id = state), map = us_state)
# ggChoropleth() 함수의 변수들
# data = 통계값이 들어있는 데이터프레임(ex. 미국 주별 범죄율)
# map = 지도(위도, 경도, 지역, 그룹, ...) 정보 데이터프레임
# mapping
# 1) map_id = data와 map을 join할 수 있는 통계 데이터프레임의 변수 이름
#    map의 region과 일치되는 data의 변수
# 2) fill = 지도 위 색깔 구분의 기준이 되는 변수

ggChoropleth(data = us_crime, map = us_state, mapping = aes(fill = Murder, map_id = state), interactive = T)




# 문자 인코딩, 변환 관련 기능 패키지
install.packages("stringi")  # ICU(International Component of Unicode)
# 개발자 도구 패키지
install.packages("devtools")
devtools::install_github("cardiomoon/kormaps2014")

# install.packages(): R 공식 페이지(패키지 저장소)에서 다운로드
# devtools::install_github(): github.com 저장소에서 다운로드

library(kormaps2014)
library(ggplot2)
library(dplyr)
library(ggiraphExtra)
search()

# kormaps2014 패키지에 있는 인구조사 데이터프레임
# str(korpop1)
# 데이터프레임의 컬럼 이름이 한글이어서 결과가 제대로 보이지 않음
str(changeCode(korpop1))
# kormaps2014::changeCode():
# 데이터프레임의 컬럼(변수) 이름이 한글로 되어 있는 것을 처리해줌

head(changeCode(korpop1))  # 거주지역, 인구, 주거형태, ...

# 한글로 되어 있는 컬럼은 에러 발생 가능하므로 영어로 변경
korpop1 <- rename(korpop1, name = 행정구역별_읍면동, pop = 총인구_명)
head(changeCode(korpop1))

ggChoropleth(data = korpop1, mapping = aes(fill = pop, map_id = code, tooltip = name), map = kormap1, interactive = T)

head(changeCode(tbc))  # 결핵 환자 데이터프레임
tail(changeCode(tbc))
str(changeCode(tbc))
ggChoropleth(data = tbc, map = kormap1, mapping = aes(fill = NewPts, map_id = code, tooltip = name), interactive = T)

tbc2015 <- tbc %>% filter(year == 2015)
ggChoropleth(data = tbc2015, map = kormap1, mapping = aes(fill = NewPts, map_id = code, tooltip = name), interactive = T)
