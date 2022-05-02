#-------------------------------------------------------------------------------

### 데이터 분석 (파일)

library()
library(readxl)
data <- read.csv('C:/Sources/StudyR/Busan_202202_R/Data-20220207T040151Z-001/Data/전라남도_목포시_장애인_복지시설_20210802.csv')
data_temp <- data

str(data_temp)

#-------------------------------------------------------------------------------

### 웹 스크래핑(웹 크롤링)

# 텍스트와 이미지가 혼합되어 있는 HTML형식으로 구성된 웹사이트에서 정보 추출

# 순서
# 1. 웹 스크래핑 대상 URL 할당
# 2. 웹 문서 가져오기
# 3. 특정 태그의 데이터 추출
# 4. 데이터 정제
# 5. 데이터 프레임 만들기

# 패키지 설치

install.packages('rvest')   # 웹 스크래핑 패키지
install.packages('stringr') # 문자열 처리 패키지

library(rvest)
library(stringr)

#-------------------------------------------------------------------------------

# 1. 웹 스크래핑 대상 URL 할당

url <- 'https://www.bobaedream.co.kr/cyber/CyberCar.php?gubun=K&page=1'
url 

#-------------------------------------------------------------------------------

# 2. 웹 문서 가져오기

usedCar <- read_html(url)
usedCar

#-------------------------------------------------------------------------------

# 3. 특정 태그의 데이터 추출
# 가져온 usdecar에서 css가 ".product-item"인 겻을 찾음
# 사용하고자 하는 정보를 찾아서 css = 넣어줘야함
# f12를 누르고 좌측 상단 화살표 모양을 누르면 쉽게 찾을 수 있음

carInfos <- html_nodes(usedCar, css = ".product-item")
head(carInfos)
# html_nodes: 해당 태그가 포함하고 있는 소스코드 및 속성을 추출함
#             모든 요소를 반환
# css: 속성이 class인 경우 약자로 사용

# 차량 명칭 추출

title_tmp <- html_nodes(carInfos, css = ".tit.ellipsis")
title_tmp

title <- html_text(title_tmp)
title
# html_text: 텍스트 추출

#-------------------------------------------------------------------------------

# 4. 데이터 정제

title <- str_trim(title) # 문자열에서 공백 제거거
title

# 차량 연식 추출

year_tmp <- html_nodes(carInfos, css = ".mode-cell.year")
year_tmp

year <- html_text(year_tmp)
year

year <- str_trim(year)
year

# 연료 구분

fuel_tmp <- html_nodes(carInfos, css = ".mode-cell.fuel")
fuel_tmp

fuel <- html_text(fuel_tmp)
fuel

fuel <- str_trim(fuel)
fuel

# 주행거리 추출

km_tmp <- html_nodes(carInfos, css = ".mode-cell.km")
km_tmp

km <- html_text(km_tmp)
km

km <- str_trim(km)
km

# 판매가격 추출

price_tmp <- html_nodes(carInfos, css = ".mode-cell.price")
price_tmp

price <- html_text(price_tmp)
price

price <- str_trim(price)
price

price <- str_replace(price, '\n', '') # 문자열 변경(/n을 스페이스로 변경)
price

# 차량 명칭으로부터 제조사 추출

maker = c()
maker

title
str(title)
summary(title)

for (i in 1:length(title)) {
  maker <- c(maker, unlist(str_split(title[i], ' '))[1]) # 문자열 분리 (list로)
}
maker

#-------------------------------------------------------------------------------

# 5. 데이터 프레임 만들기

usedCars <- data.frame(title, maker, year, fuel, km, price)
View(usedCars)

#-------------------------------------------------------------------------------

# km 자료 숫자로 변경

usedCars$km

usedCars$km <- gsub('만km', '0000', usedCars$km) # 문자열 변환("만km" - "0000")
usedCars$km <- gsub('천km', '000', usedCars$km)
usedCars$km <- gsub('km', '', usedCars$km)
usedCars$km <- gsub('미등록', '', usedCars$km)
usedCars$km <- as.numeric(usedCars$km)
# gsub(찾을 것, 바꿀 것, 열 지정)

usedCars$km

# price 자료 숫자로 변경

usedCars$price

usedCars$price <- gsub('만원', '', usedCars$price)
usedCars$price <- gsub('계약', '', usedCars$price)
usedCars$price <- gsub('팔림', '', usedCars$price)
usedCars$price <- gsub('금융리스', '', usedCars$price)
usedCars$price <- gsub(',', '', usedCars$price)
usedCars$price <- as.numeric(usedCars$price)

usedCars$price

# 자료 파일로 저장하기

setwd('C:/Sources/StudyR/Busan_202202_R')
write.csv(usedCars, 'usedCars.csv')

#-------------------------------------------------------------------------------

### api를 활용해서 데이터 수집
# 다수의 기관에서 오픈 API를 통하여 해당 기관들이 보관하고 있는 자료들을 
# 대부분 REST 방식을 통하여 제공

# 1. 오픈 api 제공 웹사이트에 접속 및 로그인
# 2. 오픈 api 자료 검색
# 3. 활용신청 및 개발계정 api키 신청
# 4. 승인 받은 개발계정 api키 확인
# 5. 오픈 api 접속을 위한 웹 URL 및 요청변수 확인
# 6. R에서 오픈 api를 이용한 자료요청
# 7. 데이터 프레임 만들기

# 패키지 불러오기
# 오픈 API로 가져온 자료를 XML 형식으로 변경하는데 사용되는 함수를 지원

install.packages('XML')
library(XML)

# 웹사이트 URL 설정
# 시도별 실시간 측정정보 조회

api_url <- "http://apis.data.go.kr/B552584/ArpltnInforInqireSvc/getCtprvnRltmMesureDnsty"

# 승인 받은 KEY 등록

service_key <- "vG%2BxMZKw3h2Z0Ey%2B5jm91ai4mHl%2FzEFc45o%2FhvA%2F%2FpyDWj3Er%2FDaFUs8R0a3KnNaqEHvJVG4%2F0plhd316KrimQ%3D%3D"

# 요청변수 등록

numOfRows <- '30'
sidoName <- '경기'

sidoName <- URLencode(iconv(sidoName, to = 'UTF-8'))
sidoName

searchCondition <- 'DAILY'

# 오픈 API URL
# paste와 paste()의 차이
paste('a','b','c') # 공백을 구분자로 묶기
# paste('a','b','c',sep = ' ') # 원래 상태
paste0('a','b','c') # 구분자 없이 묶기

# URL 주소를 공백없이 모두 묶기

open_api_url <- paste0(api_url, "?serviceKey=", service_key,
                       "&numOfRows=", numOfRows,
                       "&sidoName=", sidoName,
                       "&searchCondition=", searchCondition)
open_api_url

# 오픈 API 통하여 XML 형식으로 자료 가져오기

raw.data <- xmlTreeParse(open_api_url,
                         useInternalNodes = TRUE,
                         encoding = 'UTF-8')
raw.data

# XML 형식의 자료를 데이터 프레임으로 변경하기
# </item> 태그 별로 데이터 구분하기

air_pollution <- xmlToDataFrame(getNodeSet(raw.data, " //item"))
air_pollution

View(air_pollution)

# subset() 검색조건에 맞는 컬럼들만 가지고 오기

air_pollution <- subset(air_pollution,
                        select = c(dataTime,
                                   stationName,
                                   so2Value,
                                   coValue,
                                   o3Value,
                                   no2Value,
                                   pm10Value))

View(air_pollution)                          

# 저장

setwd('C:/Sources/StudyR/Busan_202202_R')
write.csv(air_pollution, 'air_pollution.csv')

#-------------------------------------------------------------------------------

### Practice

library(XML)

api_url <- 'http://apis.data.go.kr/6260000/BusanJobOpnngInfoService/getJobOpnngInfo'

service_key <- 'vG%2BxMZKw3h2Z0Ey%2B5jm91ai4mHl%2FzEFc45o%2FhvA%2F%2FpyDWj3Er%2FDaFUs8R0a3KnNaqEHvJVG4%2F0plhd316KrimQ%3D%3D'

numOfRows <- 10

pageNo <- 1

open_api_url <- paste0(api_url, 
                       '?serviceKey=', service_key,
                       '&numOfRows=', numOfRows,
                       '&pageNo=', pageNo)

raw.data <- xmlTreeParse(open_api_url,
                         useInternalNodes = TRUE,
                         encoding = 'UTF-8')

raw.data

Busan_data <- xmlToDataFrame(getNodeSet(raw.data, " //item"))
Busan_data

View(Busan_data)

final_data <- subset(Busan_data,
                     select = c(recruitAgencyName,
                                title))

final_data
write.csv(final_data, 'final_recruit_data.csv')


