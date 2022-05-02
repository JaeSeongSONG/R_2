devtools::install_github("cardiomoon/moonBook2")
devtools::install_github('cardiomoon/kormaps2014')

library(kormaps2014)
library(dplyr)
library(ggiraphExtra)
library(ggplot2)
library(plotly)

Sys.setlocale("LC_ALL", "korean")

korpop1_temp3 = as.data.frame(kormaps2014::korpop1)
head(korpop1_temp3)
head(changeCode(korpop1_temp3))

tbc_temp3 = as.data.frame(kormaps2014::tbc)
head(tbc_temp3)
head(changeCode(tbc_temp3))


str(changeCode(korpop1))
str(changeCode(tbc))
tbc$name <- iconv(tbc$name, "UTF-8", "CP949")
tbc

korpop1_temp <- changeCode(korpop1_temp3)
tbc_temp <- changeCode(tbc_temp3)

head(korpop1_temp)
head(tbc_temp)

korpop1_temp <- rename(korpop1_temp, 
       name = 행정구역별_읍면동)

korpop1_temp <- rename(korpop1_temp, 
                       date = 시점)

korpop1_temp <- rename(korpop1_temp, 
                       kor_male = 내국인_남자_명,
                       kor_female = 내국인_여자_명)

korpop_temp <- korpop1_temp

#-------------------------------------------------------------------------------

# korpop1_temp, tbc_temp

# 연도별 결핵 환자 수

class(korpop_temp)
str(korpop_temp)
str(tbc_temp)

table(is.na(korpop_temp)) # 결측치 없음
table(is.na(tbc_temp$year)) # 결측치 없음
table(is.na(tbc_temp$NewPts)) # 결측치 있음

year_NewPts <- tbc_temp %>%  
  filter(!is.na(as.numeric(NewPts))) %>% 
  group_by(year) %>%  
  summarise(sum_NewPts = sum(as.numeric(NewPts))) 


year_NewPts


ggplot(data = year_NewPts, aes(x = year , y = sum_NewPts)) + 
  geom_col() +
  coord_flip()

#-------------------------------------------------------------------------------

# 광주광역시와 서울 특별시의 외국인 인구 차이

str(korpop_temp)

korpop_temp <- rename(korpop_temp,
                      all_foreigner = 외국인_계_명)

gj_sl <- korpop_temp %>% 
  filter(name %in% c('서울특별시', '광주광역시')) %>% 
  select(name, all_foreigner)

gj_sl

ggplot(data = gj_sl, aes(x = name, y = all_foreigner)) +
  geom_col()

#-------------------------------------------------------------------------------

# 지역별 총 인구 중 남자의 비율

class(korpop_temp)
str(korpop_temp)
str(tbc_temp)

korpop_temp <- rename(korpop_temp,
                      all_pop = 총인구_명)
korpop_temp <- rename(korpop_temp,
                      all_man = 남자_명)

table(is.na(korpop_temp$kor_male)) # 결측치 없는 것 확인
table(is.na(korpop_temp$all_man))

str(changeCode(korpop1))
korpop1$name <- iconv(korpop1$name, "UTF-8","CP949")
 
male_korea <- korpop_temp %>% 
  select(name, all_pop, all_man) %>% 
  group_by(name) %>% 
  summarise(pct_male = (as.numeric(all_man)/as.numeric(all_pop)) * 100)

male_korea

p <- ggplot(data = male_korea, aes(x = reorder(name, pct_male), y = pct_male)) +
         geom_col() +
         coord_flip() +
         ylim(0, 70)

ggplotly(p)

#-------------------------------------------------------------------------------

# 지역별 아파트의 비율

str(korpop_temp)
str(tbc_temp)

head(korpop_temp)
head(tbc_temp)

korpop_temp <- rename(korpop_temp,
                      house = 주택_계_호,
                      detached_h = 단독주택_호,
                      apartment = 아파트_호,
                      town_house = 연립주택_호,
                      multiple_h = 다세대주택_호,
                      non_residential = 비거주용_건물내_주택_호)

korpop_temp <- rename(korpop_temp,
                      non_house = 주택이외의_거처_호)

str(korpop_temp)
korpop_temp$name

table(is.na(korpop_temp$house))
table(is.na(korpop_temp$detached_h))
table(is.na(korpop_temp$apartment))
table(is.na(korpop_temp$town_house))
table(is.na(korpop_temp$multiple_h))
table(is.na(korpop_temp$non_residential)) # NA 값이 없는 것으로 확인


house_korea <- korpop_temp %>%  
  select(name, apartment) %>% 
  group_by(name) %>% 
  summarise(sum_apart = sum(as.numeric(apartment))) 

house_korea
str(house_korea)

ggplot(data = house_korea, aes(x = reorder(name, sum_apart), y = sum_apart)) +
  geom_col() +
  coord_flip() 


