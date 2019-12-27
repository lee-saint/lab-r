# csv, 엑셀 파일에서 데이터프레임 만들기

# 현재 작업 디렉토리(폴더) 확인
getwd()
# 현재 작업 디렉토리를 변경
setwd("C:/dev/lab-r")

# ./data/csv_exam.csv 파일을 읽어서 데이터프레임 생성
exam_csv <- read.csv("data/csv_exam.csv")
# 데이터프레임의 내용 확인
exam_csv
# 수학 점수의 합계
sum(exam_csv$math)
# 영어 점수의 평균
mean(exam_csv$english)
# 각 학생의 수학/영어/과학 점수 합계
total <- exam_csv$math + exam_csv$english + exam_csv$science
total


# 헤더가 없는 csv 파일에서 데이터프레임 생성
exam_csv2 <- read.csv("data/exam_nohead.csv", header = FALSE)
exam_csv2

exam_csv3 <- read.csv("data/csv_exam2.csv", sep = ":")
exam_csv3


# MS Excel 파일(xls, xlsx)을 읽어서 데이터프레임을 생성
# readxl 패키지를 설치
# install.packages("readxl")
# readxl::read_excel()
# 설치한 패키지를 검색 경로에 추가
library(readxl)
# 패키지가 검색 경로에 추가되었는지 확인
search()

exam_xlsx <- read_excel("data/excel_exam.xlsx")
exam_xlsx
# 수학 점수 출력
exam_xlsx$math
# 엑셀 파일에서 첫 행이 컬럼(변수) 이름이 아닌 경우
exam_xlsx_nohead <- read_excel("data/excel_exam_novar.xlsx",
                               col_names = FALSE)
exam_xlsx_nohead

# 엑셀 파일에 컬럼 이름이 없는 경우 직접 컬럼 이름을 만들어줄 수 있음
exam_xlsx_nohead <- read_excel("data/excel_exam_novar.xlsx",
                               col_names = c("id", "cl", "m", "e", "s"))
exam_xlsx_nohead


# 엑셀 파일에서 특정 sheet에 있는 데이터를 읽는 경우
exam_xlsx_sheet <- read_excel("data/excel_exam_sheet.xlsx", sheet = 3)
exam_xlsx_sheet
