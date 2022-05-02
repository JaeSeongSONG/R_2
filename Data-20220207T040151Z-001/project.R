#-------------------------------------------------------------------------------

### 지도 시각화

### 미국 주별 강력 범죄율 단계 구분도 만들기

# 패키지 준비하기

install.packages('ggiraphExtra')
library(ggiraphExtra)

# 미국 주별 범죄 데이터 준비하기

str(USArrests)

head(USArrests)

# 행 이름을 state 변수로 바꿔 데이터 프레임 생성

crime <- rownames_to_column(USArrests, var = 'state')
# 행 이름만 붙어있던 데이터에 인덱스 번호가 붙음

crime

# 지도 데이터와 동일하게 맞추기 위해 state 값을 소문자로 수정

crime$state <- tolower(crime$state)

str(crime)

# 미국 주 지도 데이터 준비하기

library(ggplot2)
states_map <- map_data('state')
str(states_map)
states_map

# 단계 구분도 만들기

ggChoropleth(data = crime,           # 지도에 표현할 데이터
             aes(fill = Murder,      # 색으로 표현할 변수
                 map_id = state),    # 지역 기준 변수
             map = states_map)       # 지도 데이터
# fill: 어떤 값으로 채울 것인가
# ggChoropleth: 지도 데이터 위에 특정한 맵핑 데이터를 사용하여 다른 색상/기호 등으로 시각화
# ggChoropleth(data=데이터프레임A,map=데이터프레임M,mapping=aes(map_id=변수X, fill=변수Y))


### 인터렉티브 단계 구분도 만들기

ggChoropleth(data = crime,           
             aes(fill = Murder,      
                 map_id = state),    
             map = states_map,
             interactive = T) 

#-------------------------------------------------------------------------------

### 대한민국 시도별 인구, 겷핵 환자 수 단계 구분도 만들기

# 패키지 준비하기

install.packages('stringi')

install.packages('devtools')
devtools::install_github('cardiomoon/kormaps2014')

library(kormaps2014)

# 대한민국 시도별 인구 데이터 준비하기

str(korpop1)

str(changeCode(korpop1)) # encoding: cp949

library(dplyr)

korpop1 <- rename(korpop1, pop = 총인구_명, name = 행정구역별_읍면동)
# 변수 명이 한글로 되어있어 오류가 날 수 있기 때문에 영어로 바꿈

str(changeCode(korpop1))

library(ggiraphExtra)
library(ggplot2)

str(changeCode(korpop1))

korpop1$name <- iconv(korpop1$name, "UTF-8","CP949")

ggChoropleth(data = korpop1,      # 지도에 표현할 데이터
             aes(fill = pop,      # 색깔로 표현할 변수
                 map_id = code,   # 지역 기준 변수
                 tooltip = name), # 지도 위에 표시할 지역명
             map = kormap1,       # 지도 데이터
             interactive = T)     # 인터랙티브

#-------------------------------------------------------------------------------

### 대한민국 시도별 인구, 겷핵 환자 수 단계 구분도 만들기

# 결핵 환자 수

str(changeCode(tbc))

tbc$name <- iconv(tbc$name, "UTF-8", "CP949")

ggChoropleth(data = tbc,          # 지도에 표현할 데이터
             aes(fill = NewPts,   # 색깔로 표현할 변수
                 map_id = code,   # 지역 기준 변수
                 tooltip = name), # 지도 위에 표시할 지역명
             map = kormap1,       # 지도 데이터
             interactive = T)     # 인터랙티브
