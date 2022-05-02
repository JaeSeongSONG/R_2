# data frame 만들기

# 한글 안될때 
Sys.setlocale("LC_ALL", "korean")

english <- c(90,80,60,70)
str(english) # type = num

math <- c(50,60,100,20)
math

class <- c(1,1,2,2)
class

df_midterm <- data.frame(english, math, class)
df_midterm

a<- data.frame()

# df_midterm_english 평균 산출

mean(df_midterm$english)

# df_midterm_math 평균 산출

mean(df_midterm$math)



# excel data 사용 (readxl 활용)

install.packages("readxl")
library(readxl)

setwd("C:/Sources/StudyR/220207_day6/1day")
getwd()

df_exam = read_excel("./Data-20220207T040151Z-001/Data/excel_exam.xlsx") # ./ 는 현재 디렉토리를 뜻함
df_exam

mean(df_exam$english)
mean(df_exam$science)



# excel data에서 column이 없을 때
# ...1 ...2 ...3 의 형식으로 column name이 붙음

df_exam_novar = read_excel("./Data-20220207T040151Z-001/Data/excel_exam_novar.xlsx", col_names = F)
df_exam_novar



# excel data에서 sheet가 여러개인 경우

df_exam_sheet = read_excel("./Data-20220207T040151Z-001/Data/excel_exam_sheet.xlsx", sheet = 3)
df_exam_sheet



# csv 파일 저장하기

df_midterm <- data.frame(english = c(90,80,60,70),
                         math = c(50,60,100,20),
                         class = c(1,1,2,2))

df_midterm
write.csv(df_midterm, file = "./df_midterm.csv")



# 데이터를 확인할 수 있는 함수
# head() 첫번째 행부터 출력(6개), tail() 마지막 행부터 출력(6개), view() 뷰어 창에서 출력, dim() 차원을 알려줌
# str() 타입을 알려줌, summary() 데이터의 요약(기초통계)를 출력

install.packages(readxl)
library(readxl)

setwd = ("C:/Sources/StudyR/Busan_202202_R/Data-20220207T040151Z-001/Data")
getwd()

exam <- read.csv("./Data-20220207T040151Z-001/Data/csv_exam.csv")
exam



### 기초 함수

head(exam,10)
tail(exam)
str(exam)
summary(exam)
dim(exam)

#-------------------------------------------------------------------------------

# mpg data 확인
# mpg data를 데이터 프레임 형태로 불러오기

mpg <- as.data.frame(ggplot2::mpg)
mpg
head(mpg, 10)
summary(mpg)

#-------------------------------------------------------------------------------

# data control에 사용되는 패키지 설치 및 로드
# dplyr 설치: 데이터 프레임을 처리하는 함수군으로 구성

install.packages("dplyr")
library(dplyr)

### data control

df_raw <- data.frame(var1 = c(1,2,1),
                     var2 = c(2,3,2))
df_raw

df_new <- rename(df_raw, v2 = var2) # var2를 v2로 변경
df_new

df <- data.frame(var1 = c(4,3,8),
                 var2 = c(2,6,1))

df$varsum <- df$var1 + df$var2 # dataframe에 새로운 column 추가하기 (파생변수 만들기)
df$var_sum <- 10 # 모든 값을 10으로 고정
df

df$var_mean <- (df$var1 + df$var2) / 2 # var_mean 변수 생성
df

mpg$total <- (mpg$cty + mpg$hwy) / 2 # 통합 연비 변수 평균 구하기
mean(mpg$total)
summary(mpg$total)
hist(mpg$total) # histogram 그리기

#-------------------------------------------------------------------------------

# ifelse 구문

mpg$test <- ifelse(mpg$total >= 20, "pass", "fail")
head(mpg, 20)

mpg$grade <- ifelse(mpg$total >= 30, "A", ifelse(mpg$total >= 20, "B", "C"))
head(mpg, 20)
table(mpg$grade) # data를 성격에 맞게 분류해서 요약
qplot(mpg$grade)

# ggplot2

library(ggplot2)
qplot(mpg$test) # qplot: 빈도 막대 그래프



### Practice

midwest <- as.data.frame(ggplot2::midwest)
head(midwest)
tail(midwest)
View(midwest)
dim(midwest)
str(midwest)
summary(midwest)

total <- midwest$poptotal
asian <- midwest$popasian
total
asian

midwest$ratio <- asian/total*100 
hist(midwest$ratio)

mean(midwest$ratio)
midwest$updown <- ifelse(midwest$ratio > mean(midwest$ratio), "large", "small")
head(midwest)

table(midwest$updown)
barplot(table(midwest$updown))  # 빈도 막대 그래프 활용

#-------------------------------------------------------------------------------

### data Preprocessing - dplyr package

filter() # 행추출
select() # 열 추출
arrange() # 정렬
mutate() # 변수 추가
summarise() # 통계치 산출
group_by() # 집단별로 나누기
left_join() # 데이터 합치기(열)
bind_rows() # 데이터 합치기(행)

#-------------------------------------------------------------------------------

# filter 조회
# 조건이 있을 때

library(dplyr)
exam
exam %>% filter(class == 1) # 1반인 경우
exam %>% filter(class != 1) # 1반이 아닌 경우
exam %>% filter(class > 1) # 1보다 큰 수의 반인 경우
exam %>% filter(class == 1 & math >= 50) # AND 조건
exam %>% filter(math >= 90 | english >= 90) # OR 조건
exam %>% filter(class == 1 | class == 3 | class == 5)
exam %>% filter(class %in% c(1,3,5)) # 위 결과와 동일

class1 <- exam %>% filter(class == 1)
class2 <- exam %>% filter(class == 2)
mean(class1$math)
mean(class2$math)

mpg
str(mpg)
car1 = mpg %>% filter(displ <= 4)
car2 = mpg %>% filter(displ >= 5)
mean(car1$hwy)
mean(car2$hwy)

audi = mpg %>% filter(manufacturer == "audi")
toyota = mpg %>% filter(manufacturer == "toyota")  
mean(audi$cty)
mean(toyota$cty)

mpg_new <- mpg %>% filter(manufacturer %in% c("audi", "toyota", "honda"))
mean(mpg_new$hwy)

#-------------------------------------------------------------------------------

# select 추출
# 조건이 없고, 추출할 때

exam %>% select(math) # math 추출
exam %>% select(class, math, english) # 여러개 추출
exam %>% select(-math) # math 제외 추출
exam %>% select(-math, -english)

exam %>% 
  filter(class == 1) %>% 
  select(english) # class가 1인 데이터에서 english 추출

exam %>% 
  select(id, math) %>%
  head

#-------------------------------------------------------------------------------

# arrange 정렬

exam %>% arrange(math) # math 오름차순 정렬
exam %>% arrange(desc(math)) # math 내림차순 정렬
exam %>% arrange(class, math) # 2개의 변수 순서대로 오름차순 정렬

#-------------------------------------------------------------------------------

# mutate 새로운 파생 column을 만드는 함수

exam %>% 
  mutate(total = math + english + science) %>% 
  head

exam %>% 
  mutate(total = math + english + science,
         mean = (math + english + science) / 3) %>% 
  head

exam %>% 
  mutate(test = ifelse(science >= 60, "pass", "fail")) %>% 
  head

#-------------------------------------------------------------------------------

# summarise 요약하기, group_by 집단으로 만들기

exam %>% summarise(mean_math = mean(math)) # column처럼 만들지만 요약만 함

exam %>% 
  group_by(class) %>% 
  summarise(mean_math = mean(math))

exam %>% 
  group_by(class) %>% 
  summarise(mean_math = mean(math),
            sum_math = sum(math),
            median_math = median(math),
            n = n()) # 학생 수 dplyr의 group_by 안 summarise 함수에서만 사용

mpg %>% 
  group_by(manufacturer, drv) %>% # manufacturer과 drv가 모두 같은 것끼리 집단화
  summarise(mean_cty = mean(cty)) %>% 
  head(10)

mpg %>% 
  group_by(manufacturer) %>% 
  filter(class == "suv") %>% 
  mutate(tot = (cty + hwy) / 2) %>%
  summarise(mean_tot = mean(tot)) %>% 
  arrange(desc(mean_tot)) %>% 
  head(5)

#-------------------------------------------------------------------------------

# 자주 사용하는 요약통계량 함수

mean()
sd() # 표준편차
sum()
median()
min()
max()
n() # 빈도

#-------------------------------------------------------------------------------

### 데이터 합치기
# 옆으로 합치기
# left_join(data, data, by = '')

test1 <- data.frame(id = c(1, 2, 3, 4, 5),
                    midterm = c(60, 80, 70, 90, 85))

test2 <- data.frame(id = c(1, 2, 3, 4, 5),
                    final = c(70, 83, 65, 95, 80))

total <- left_join(test1, test2, by = 'id') # by 기준으로 합쳐 total에 할당
total

name <- data.frame(class = c(1, 2, 3, 4, 5),
                   teacher = c('Kim', 'Lee', 'park', 'choi', 'jung'))
exam

exam_new <- left_join(exam, name, by = 'class')
exam_new

# 밑으로 합치기
# bind_rows()

group_a <- data.frame(id = c(1, 2, 3, 4, 5),
                      test = c(60, 80, 70 , 90, 85))

group_b <- data.frame(id = c(6, 7, 8, 9, 10),
                      test = c(70, 83, 65, 95, 80))                      
                      
group_all = bind_rows(group_a, group_b)                      
group_all                      

#-------------------------------------------------------------------------------

### practice

fuel <- data.frame(fl = c('c', 'd', 'e', 'p', 'r'),
                   price_fl = c(2.35, 2.38, 2.11, 2.76, 2.22),
                   stringsAsFactors = F)
fuel

# data.frame()으로 데이터프레임을 생성할 때, 변수에 문자가 있을 경우 자동으로 factor타입으로 변환
# 하지만 factor 변수는 연산이 되지 않으므로 stringsAsFactors() 함수를 써서 factor타입으로 변환되지 않게 한다.

df_all <- left_join(mpg, fuel, by = 'fl')
df_all %>% 
  select(model, fl, price_fl) %>% 
  head(5)

#-------------------------------------------------------------------------------

### practice

data = mpg %>% select(class, cty)
head(data)  

data1 = data %>% filter(class == 'suv')
data2 = data %>% filter(class == 'compact')
mean(data1$cty)
mean(data2$cty)

mpg %>% filter(manufacturer == "audi") %>% 
  arrange(desc(hwy)) %>% 
  head(5)

copy_mpg <- mpg
copy_mpg
mpg %>% 
  mutate(total = cty + hwy, 
         mean = (cty + hwy) / 2) %>% 
  arrange(desc(mean)) %>% 
  head(3)

mpg %>% 
  group_by(class) %>% 
  summarise(mean_cty = mean(cty))

mpg %>% 
  group_by(class) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  arrange(desc(mean_cty))

mpg %>% 
  group_by(manufacturer) %>% 
  summarise(mean_hwy = mean(hwy)) %>% 
  arrange(desc(mean_hwy)) %>% 
  head(3)

mpg %>%  
  filter(class == "compact") %>% # 속도의 차이: 데이터를 날리고 grouping이 빠름
  group_by(manufacturer) %>%
  summarise(count = n()) %>% 
  arrange(desc(count)) 

#-------------------------------------------------------------------------------

### data 정제

### 결측치 제외

df <- data.frame(sex = c('M', 'F', NA, 'M', 'F'),
                 score = c(5, 4, 3, 4, NA))

str(df)

# is.na

df
is.na(df)
table(is.na(df))    

mean(df$score) # NA
sum(df$score) # NA

df %>% 
  filter(is.na(score)) # score 가 NA 인 데이터만 출력

df %>%   
  filter(!is.na(score)) # score 결측치 제거

df_nomiss <- df %>% 
  filter(!is.na(score) & !is.na(sex))
df_nomiss
mean(df_nomiss$score)
sum(df_nomiss$score)

# na.omit

df_nomiss2 <- na.omit(df) # 모든 변수에 결측치 있는 행 전체를 제거
df_nomiss2 # 분석에 필요한 데이터까지 손실 될 가능성 유의

# na.rm

mean(df$score, na.rm = T) # 결측치 제외하고 평균 산출
sum(df$score, na.rm = T) # 결측치 제외하고 합계 산출

exam[c(3, 8, 15), 'math'] <- NA
exam %>% summarise(mean_math = mean(math))
exam %>% summarise(mean_math = mean(math, na.rm = T)) # 결측치 제외하고 평균 산출

#-------------------------------------------------------------------------------

### data 정제

### 결측치 대체
# 대표값(평균, 최빈값)을 사용하여 대체

# 평균으로 대체하기

exam$math <- ifelse(is.na(exam$math), 55, exam$math)
table(is.na(exam$math))
exam

mpg[c(65, 124, 131, 153, 212), 'hwy'] <- NA

table(is.na(mpg$drv)) # NA 없음
table(is.na(mpg$hwy)) # NA 5개 존재
mpg %>% 
  filter(!is.na(drv) & !is.na(hwy)) %>% 
  group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy))

#-------------------------------------------------------------------------------

### data 정제

### 이상치 제외

outlier <- data.frame(sex = c(1, 2, 1, 3, 2, 1),
                      score = c(5, 4, 3, 4, 2, 6))
outlier         

# 이상치 확인하기

table(outlier$sex)
table(outlier$score)

# 처리하기

outlier$sex <- ifelse(outlier$sex == 3, NA, outlier$sex)
outlier

outlier$score <- ifelse(outlier$score > 5, NA, outlier$score)
outlier

outlier %>% 
  filter(!is.na(sex) & !is.na(score)) %>% 
  group_by(sex) %>% 
  summarise(mean_score = mean(score))

# 극단치 기준 정해서 제거하기

mpg <- as.data.frame(ggplot2::mpg)
boxplot(mpg$hwy)$stats # 12 - 37 을 벗어나는 값들을 극단치라고 함

mpg$hwy <- ifelse(mpg$hwy < 12 | mpg$hwy > 37, NA, mpg$hwy)
table(is.na(mpg$hwy))

mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy, na.rm = T))

#-------------------------------------------------------------------------------

### practice

mpg[c(10, 14, 58, 93), 'drv'] <- 'k' # 10, 14 행 등에 drv 열 값을 k 로 바꿈
mpg[c(29, 43, 129, 203), 'cty'] <- c(3, 4, 39, 42) 
mpg

table(mpg$drv)
mpg$drv <- ifelse(mpg$drv %in% c('f', 4, 'r'), mpg$drv, NA)

boxplot(mpg$cty)$stats # 9 - 26
mpg$cty <- ifelse(mpg$cty < 9 | mpg$cty > 26, NA, mpg$cty)
boxplot(mpg$cty)

mpg %>% 
  filter(!is.na(drv) & !is.na(cty)) %>% 
  group_by(drv) %>% 
  summarise(mean_cty = mean(cty, na.rm = T))
