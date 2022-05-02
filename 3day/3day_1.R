#-------------------------------------------------------------------------------

### 선 그래프 - 시간에 따라 달라지는 데이터 표현

# 시계열 그래프 만들기

library(ggplot2)
mpg <- as.data.frame(ggplot2::mpg)

ggplot(data = economics, aes(x = date, y = unemploy)) + geom_line()

#-------------------------------------------------------------------------------

### Practice

ggplot(data = economics, aes(x = date, y = psavert)) + geom_line()

#-------------------------------------------------------------------------------

# box plot

ggplot(data = mpg, aes(x = drv, y = hwy)) + geom_boxplot()
# boxplot이 3개 나오는 이유는 drv의 종류가 3개이기 때문
# 또한 f 열에서 이상치가 발견됨

#-------------------------------------------------------------------------------

### Practice

library(dplyr)
class2 <- mpg %>% filter(class %in% c('compact', 'subcompact', 'suv'))
ggplot(data = class2, aes(x = class, y = cty)) + geom_boxplot()

#-------------------------------------------------------------------------------

### data analysis project

install.packages('foreign')
library(foreign) # spss 파일 로드
library(dplyr) # 전처리
library(ggplot2) # 시각화
library(readxl) # 엑셀 파일 불러오기

setwd = 'C:/Sources/StudyR/Busan_202202_R'
getwd()
dir()
raw_welfare <- read.spss(file = './Data-20220207T040151Z-001/Data/koweps_hpc10_2015_beta1.sav',
                           to.data.frame = T)

# 복사본 만들기

welfare <- raw_welfare
welfare

# 데이터 검토

head(welfare)
tail(welfare)
dim(welfare)
View(welfare) # 가능하면 사용하지 않는 것이 좋음
str(welfare)
summary(welfare)
is.data.frame(welfare) # data frame인 것을 확인

# 변수명 바꾸기
# 메타 정의서 확인

welfare <- rename(welfare,
                   sex = h10_g3,            # 성별
                   birth = h10_g4,          # 태어난 연도
                   marriage  = h10_g10,     # 혼인 상태
                   religion = h10_g11,      # 종교
                   income = p1002_8aq1,     # 월급
                   code_job = h10_eco9,     # 직종코드
                   code_region = h10_reg7)  # 지역 코드

#-------------------------------------------------------------------------------

### 성별과 월급의 관계 찾기
# 성별 변수 검토 및 전처리

class(welfare$sex) # character
table(welfare$sex)

# 전처리

welfare$sex <- ifelse(welfare$sex == 9, NA, welfare$sex) # 이상치 결측 처리
table(is.na(welfare$sex)) # 결측치 없음

# 성별 항목 이름 부여

welfare$sex <- ifelse(welfare$sex == 1, 'male', 'female') # 1: male , 2: female
table(welfare$sex)

# 월급 변수 검토 및 전처리

class(welfare$income) # numeric
summary(welfare$income)
qplot(welfare$income) # 빈도표 
qplot(welfare$income) + xlim(0, 1000) # x축의 한계를 1000으로 설정

# 전처리

summary(welfare$income) # NA's = 12030으로 NA 값이 존재하는 것을 확인

# 이상치 결측 처리

welfare$income <- ifelse(welfare$income %in% c(0, 9999), NA, welfare$income)

# 결측치 확인

table(is.na(welfare$income)) # NA = 12044

### 성별에 따른 월급 차이 분석하기
# 성별 월급 평균표 만들기

sex_income <- welfare %>%  
  filter(!is.na(income)) %>% # income 에서 NA 값을 가진 것들을 제외
  group_by(sex) %>%  # sex 로 grouping
  summarise(mean_income = mean(income)) # 성별을 기준으로 income의 평균 조회

sex_income

# 그래프 만들기

ggplot(data = sex_icome, aes(x = sex , y = mean_income)) + geom_col()

#-------------------------------------------------------------------------------

### 나이와 월급과의 관계
# 나이 변수 검토 및 전처리

class(welfare$birth) # numeric
summary(welfare$birth) # 연도 값으로 가짐
qplot(welfare$birth)

# 전처리

table(is.na(welfare$birth)) # FALSE 16664

welfare$birth <- ifelse(welfare$birth == 9999, NA, welfare$birth)
table(is.na(welfare$birth))

# 파생변수 만들기

welfare$age <- 2022 - welfare$birth + 1
summary(welfare$age)

qplot(welfare$age)

# 나이에 따른 월급 평균표 만들기

age_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(mean_income = mean(income))

head(age_income)

# 그래프 만들기

ggplot(data = age_income, aes(x = age, y = mean_income)) + geom_line()

#-------------------------------------------------------------------------------

### 연령대에 따른 월급 차이

# 파생변수 만들기

welfare <- welfare %>% 
  mutate(ageg = ifelse(age < 30, 'young', ifelse(age <= 69, 'middle', 'old')))

table(welfare$ageg)
qplot(welfare$ageg)

# 연령대별 월급 평균표 만들기

ageg_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg) %>% 
  summarise(mean_income = mean(income))

ageg_income

# 그래프 만들기

ggplot(data = ageg_income, aes(x = ageg, y = mean_income)) + geom_col()

# 정렬

ggplot(data = ageg_income, aes(x = ageg, y = mean_income)) + 
  geom_col() +
  scale_x_discrete(limits = c('young', 'middle', 'old'))
# scale_x_discrete: x축의 변수 순서를 바꾸는 함수

#-------------------------------------------------------------------------------

### 성별 월급 차이는 연령대별로 다를까

# 연령대 및 성별 월급 평균표 만들기

sex_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg, sex) %>%  # grouping 2개
  summarise(mean_income = mean(income))

sex_income

# 그래프 그리기

ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) +
  geom_col() +
  scale_x_discrete(limits = c('young', 'middle', 'old'))
# fill 함수를 사용하여 한 그래프에 표현되도록 만듦

ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) +
  geom_col(position = 'dodge') +
  scale_x_discrete(limits = c('young', 'middle', 'old'))
# position = 'dodge' 는 그래프 2개로 표현

#-------------------------------------------------------------------------------

### 나이 및 성별 월급 차이 분석

sex_age <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age, sex) %>% # age와 sex로 grouping 
  summarise(mean_income = mean(income))

head(sex_age)

# 그래프 만들기

ggplot(data = sex_age, aes(x = age, y = mean_income, col = sex)) + geom_line()
# col 함수를 사용하여 그래프 구분

#-------------------------------------------------------------------------------

### 직업별 월급 차이

class(welfare$code_job) # numeric
table(welfare$code_job)

# 직업분류코드 불러오기

library(readxl)
list_job <- read_excel('./Data-20220207T040151Z-001/Data/Koweps_Codebook.xlsx', col_names = T, sheet = 2, file)
head(list_job)

# welfare에 직업명 결합

welfare <- left_join(welfare, list_job, id = 'code_job')
welfare %>% 
  filter(!is.na(code_job)) %>% 
  select(code_job, job) %>% 
  head(10)

# 직업별 월급 평균표 만들기

job_income <- welfare %>% 
  filter(!is.na(job) & !is.na(income)) %>% 
  group_by(job) %>% 
  summarise(mean_income = mean(income))

head(job_income)

# 상위 10개 추출

top10 <- job_income %>% 
  arrange(desc(mean_income)) %>% 
  head(10)

top10

# 그래프 만들기

ggplot(data = top10, aes(x = reorder(job, mean_income), y = mean_income)) +
  geom_col() +
  coord_flip()
# coord_flip()
# 수평 박스플롯을 그릴 때
# 가로 막대그래프를 세로 막대그래프로 바꾸고 싶을 때
# 분류(category, factor)의 이름이 너무 길어서 레이블이 중첩되어, 그래프를 회전하여 보아야 하는 경우

# 하위 10개 추출

bottom10 <- job_income %>% 
  arrange(mean_income) %>% 
  head(10)

bottom10

# 그래프 만들기

ggplot(data = bottom10, aes(x = reorder(job, mean_income), y = mean_income)) +
  geom_col() +
  coord_flip()

#-------------------------------------------------------------------------------

### 성별 직업 빈도
# 성별 직업 빈도 상위 10개 추출

job_male <- welfare %>% 
  filter(!is.na(job) & sex == 'male') %>% 
  group_by(job) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(10)

job_male

job_female <- welfare %>% 
  filter(!is.na(job) & sex == 'female') %>% 
  group_by(job) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(10)

job_female

# 그래프 만들기

ggplot(data = job_male, aes(x = reorder(job, n), y = n)) +
  geom_col() +
  coord_flip()
# reorder(정렬하고 싶은 변수, 연속형 데이터, function)

ggplot(data = job_female, aes(x = reorder(job, n), y = n)) +
  geom_col() +
  coord_flip()

#-------------------------------------------------------------------------------

### 종교 유무에 따른 이혼율
# 종교 변수 검토 및 전처리

class(welfare$religion) # numeric
table(welfare$religion) # 1: 8047 2: 8617

welfare$religion <- ifelse(welfare$religion == 1, 'yes', 'no')
table(welfare$religion) # yes: 8047 no: 8617
qplot(welfare$religion)

# 혼인 상태 변수 검토 및 전처리

class(welfare$marriage) # numeric
table(welfare$marriage) # 0 1 2 3 4 5 6

# 전처리

welfare$group_marriage <- ifelse(welfare$marriage == 1, 'marriage', 
                                 ifelse(welfare$marriage == 3, 'divorce', NA))
table(welfare$group_marriage)
table(is.na(welfare$group_marriage)) # 결측값 존재
qplot(welfare$group_marriage) # 빈도

# 종교 유무에 따른 이혼률 분석하기

religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(religion, group_marriage) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100, 1))

religion_marriage

# count 활용해서 종교 유무에 따른 이혼률 분석하기
# count(a, b) a와 b에 모두 중복되는 값을 세는 것

religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>%
  count(religion, group_marriage) %>% 
  group_by(religion) %>% 
  mutate(pct = round(n/sum(n)*100, 1))

religion_marriage

# 이혼율 표 만들기

divorce <- religion_marriage %>% 
  filter(group_marriage == 'divorce') %>% 
  select(religion, pct) # pct = percent

divorce  

# 그래프 만들기

ggplot(data = divorce, aes(x = religion, y = pct)) + geom_col()

#-------------------------------------------------------------------------------

### 연령대 및 종교 유무에 따른 이혼율 분석하기
# 연령대별 이혼율 표 만들기

ageg_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(ageg, group_marriage) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>%  # sum(n) 에서 n은 summarise에서 선언한 n을 뜻함
  mutate(pct = round(n/tot_group*100, 1))

ageg_marriage 

# count() 활용

ageg_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
           count(ageg, group_marriage) %>% 
           group_by(ageg) %>% 
           mutate(pct = round(n/sum(n)*100, 1))

ageg_marriage

# 연령대별 이혼율 그래프 만들기
# 초년 제외, 이혼 추출

ageg_divorce <- ageg_marriage %>% 
  filter(ageg != 'young' & group_marriage == 'divorce') %>% 
  select(ageg, pct)

ageg_divorce

# 그래프 만들기

ggplot(data = ageg_divorce, aes(x = ageg, y = pct)) + geom_col()

# 연령대, 종교유무, 결혼상태별 비율표 만들기

ageg_religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage) & ageg != 'young') %>% 
  group_by(ageg, religion, group_marriage) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100, 1))

ageg_religion_marriage

# 연령대 및 종교 유무별 이혼율 표 만들기

df_divorce <- ageg_religion_marriage %>% 
  filter(group_marriage == 'divorce') %>% 
  select(ageg, religion, pct)

str(df_divorce$religion)
df_divorce

 # 그래프 만들기

ggplot(data = df_divorce, aes(x = ageg, y = pct, fill = religion)) +
  geom_col(position = 'dodge')

#-------------------------------------------------------------------------------

### 지역별 연령대 비율
# 노년층이 많은 지역은 어디일까?

class(welfare$code_region) # numeric
table(welfare$code_region) # 1 2 3 4 5 6 7
qplot(welfare$code_region)

# 지역 코드 목록 만들기

list_region <- data.frame(code_region = c(1:7),
                          region = c("서울",
                                     "수도권(인천/경기)",
                                     "부산/경남/울산",
                                     "대구/경북",
                                     "대전/충남",
                                     "강원/충북",
                                     "광주/전남/전북/제주도"))

list_region

# welfare에 지역명 변수 추가

welfare <- left_join(welfare, list_region, id = 'code_region')

welfare %>% 
  select(code_region, region) %>% 
  head

# 지역별 연령대 비율표 만들기

region_ageg <-welfare %>% 
  group_by(region, ageg) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100, 2))

head(region_ageg)

# 그래프 만들기

ggplot(data = region_ageg, aes(x = region, y = pct, fill = ageg)) +
  geom_col() +
  coord_flip()

# 막대 정렬하기: 노년층 비율 높은 순

list_order_old <- region_ageg %>% 
  filter(ageg == 'old') %>% 
  arrange(pct)

list_order_old

# 지역명 순서 변수 만들기

order <- list_order_old$region
order

# 연령대 순으로 막대 색깔 나열하기

class(region_ageg$ageg) # character
levels(region_ageg$ageg) # null

region_ageg$ageg <- factor(region_ageg$ageg, 
                           levels = c('old', 'middle', 'young'))
class(region_ageg$ageg) # factor                   
levels(region_ageg$ageg) # old, middle, young

# 그래프 그리기 (factor)

ggplot(data = region_ageg, aes(x = region, y = pct, fill = ageg)) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limits = order)
