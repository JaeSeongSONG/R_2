#-------------------------------------------------------------------------------

### 산점도  - 변수 간 관계 표현하기

library(ggplot2)

# - Data: 그래프를 그리려는 데이터

# - Aesthetics: 데이터 매핑하고자 하는 스케일 (x축, y축, 색깔 등), '심미적・미학적'이라는 뜻

# - Geometries: 데이터 사용하는 그래프 요소들 (박스플랏, 바그래프, 산점도 등), '기하학'이라는 뜻

# - Facets: 여러 면으로 그래프 그리기, 측면・양상・면이라는 뜻

# - Statistics: 그래프를 이해하기 위한 목적으로 다시 나타내기

# - Coordinates: 데이터가 그려지는 공간, 좌표라는 뜻

# - Themes: 데이터가 아닌 요소들

ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point() # geom_point() = 산점도 그리기
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point() + xlim(3,6) # x축 범위 3-6으로 지정

ggplot(data = mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  xlim(3,6) +
  ylim(10,30)# y축 범위 10-30으로 지정

mpg <- as.data.frame(ggplot2::mpg)
midwest <- as.data.frame(ggplot2::midwest)

ggplot(data = mpg, aes(x = cty, y = hwy)) + geom_point()
ggplot(data = midwest, aes(x = poptotal, y = popasian)) + 
  geom_point() +
  xlim(0,500000) +
  ylim(0,10000)

#-------------------------------------------------------------------------------

### 막대 그래프 - 집단 간 차이 표현하기

library(dplyr)

df_mpg <- mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy))

df_mpg

ggplot(data = df_mpg, aes(x = drv, y = mean_hwy)) +
  geom_col()

# 크기 순으로 정렬하기

ggplot(data = df_mpg, aes(x = reorder(drv, -mean_hwy), y = mean_hwy)) +
  geom_col() # reorder 사용

#-------------------------------------------------------------------------------

### 빈도 막대 그래프 

ggplot(data = mpg, aes(x = drv)) +
  geom_bar()

ggplot(data = mpg, aes(x = hwy)) +
  geom_bar()

#-------------------------------------------------------------------------------

### Practice

suv <- mpg %>% 
  filter(class == 'suv') %>% 
  group_by(manufacturer) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  arrange(desc(mean_cty)) %>% 
  head(5)

ggplot(data = suv, aes(x = reorder(manufacturer, -mean_cty), y = mean_cty)) +
         geom_col()

ggplot(data = mpg, aes(x = class)) + geom_bar()



       