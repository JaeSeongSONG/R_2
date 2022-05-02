#-------------------------------------------------------------------------------

### 인터렉티브 그래프

# plotly 패키지로 인터랙티브 그래프 만들기

#-------------------------------------------------------------------------------

# 패키지 준비하기

install.packages('plotly')
library(plotly)
library(ggplot2)

# ggplot으로 그래프 만들기

mpg
p <- ggplot(data = mpg, aes(x = displ, y = hwy, col = drv)) + geom_point()
# col = color
p
ggplotly(p)

# 인터렉티브 막대 그래프 만들기

p <- ggplot(data = diamonds, aes(x = cut, fill = clarity)) + 
  geom_bar(position = 'dodge')
# position = 막대의 위치, dodge = 복수의 데이터를 독립적인 막대 그래프로 표현
# fill = 테두리 말고 안에 색 채우기

p

#-------------------------------------------------------------------------------

# dygraph 패키지로 인터랙티브 시계열 그래프 만들기

# 패키지 준비하기

install.packages('dygraphs')
library('dygraphs')

# 데이터 준비하기

economics <- ggplot2::economics
head(economics)

# 시간 순서 속성을 지니는 xts 데이터 타입으로 변경
# xts = 시계열 데이터로 만드는 라이브러리

 library(xts)

eco <- xts(economics$unemploy, order.by = economics$date)
head(eco)
# economics$unemply를 economic$date의 순서대로

# 그래프 생성

dygraph(eco)

# 날짜 범위 선택 기능

dygraph(eco) %>% 
  dyRangeSelector()

#-------------------------------------------------------------------------------

### 여러 값 표현하기

# 저축률

eco_a <- xts(economics$psavert, order.by = economics$date)
head(eco_a)

# 실업자 수

eco_b <- xts(economics$unemploy/1000, order.by = economics$date)
head(eco_b)

# 합치기
# 속성값을 맞추는 것이 매우 중요

eco2 <- cbind(eco_a, eco_b)
colnames(eco2) <- c('psavert', 'unemploy')
head(eco2)

# 합친 그래프 만들기

dygraph(eco2) %>% 
  dyRangeSelector()
