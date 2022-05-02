library(KoNLP)
library(dplyr)
library(stringr)
library(wordcloud)
library(RColorBrewer)

setwd = 'C:/Sources/StudyR/Busan_202202_R'
getwd()

# 데이터 준비

kim <- readLines('./Data-20220207T040151Z-001/Data/kim.txt', encoding = 'UTF-8')
no <- readLines('./Data-20220207T040151Z-001/Data/no.txt', encoding = 'UTF-8')
lee <- readLines('./Data-20220207T040151Z-001/Data/lee.txt', encoding = 'UTF-8')
park <- readLines('./Data-20220207T040151Z-001/Data/park.txt', encoding = 'UTF-8')

# 특수문자 제거

kim <- str_replace_all(kim, '\\W', ' ')
no <- str_replace_all(no, '\\W', ' ')
lee <- str_replace_all(lee, '\\W', ' ')
park <- str_replace_all(park, '\\W', ' ')

# 연설문에서 명사 추출

nouns_kim <- extractNoun(kim)
nouns_no <- extractNoun(no)
nouns_lee <- extractNoun(lee)
nouns_park <- extractNoun(park)

nouns_kim
nouns_no
nouns_lee
nouns_park

# list를 vector로 변환, 테이블 생성 (빈도)

wordcount_kim <- table(unlist(nouns_kim)) # list -> vector로 변환
wordcount_no <- table(unlist(nouns_no))
wordcount_lee <- table(unlist(nouns_lee))
wordcount_park <- table(unlist(nouns_park))

wordcount_kim
wordcount_no
wordcount_lee
wordcount_park

# data frame으로 변환

df_kim <- as.data.frame(wordcount_kim, stringsAsFactors = F)
df_no <- as.data.frame(wordcount_no, stringsAsFactors = F)
df_lee <- as.data.frame(wordcount_lee, stringsAsFactors = F)
df_park <- as.data.frame(wordcount_park, stringsAsFactors = F)

# 변수명 수정

df_kim <- rename(df_kim,
                  word = Var1,
                  freq = Freq)
df_no <- rename(df_no,
                 word = Var1,
                 freq = Freq)
df_lee <- rename(df_lee,
                 word = Var1,
                 freq = Freq)
df_park <- rename(df_park,
                 word = Var1,
                 freq = Freq)

# 2 글자 이상만 추출

df_kim <- filter(df_kim, nchar(word) >= 2)
df_no <- filter(df_no, nchar(word) >= 2)
df_lee <- filter(df_lee, nchar(word) >= 2)
df_park <- filter(df_park, nchar(word) >= 2)

# top 20

top_20_kim <- df_kim %>% 
  arrange(desc(freq)) %>% 
  head(20)

top_20_no <- df_no %>% 
  arrange(desc(freq)) %>% 
  head(20)

top_20_lee <- df_lee %>% 
  arrange(desc(freq)) %>% 
  head(20)

top_20_park <- df_park %>% 
  arrange(desc(freq)) %>% 
  head(20)

# word cloud

pal <- brewer.pal(8, 'Dark2')

# kim

set.seed(1234) # 난수 고정
wordcloud(words = df_kim$word, # 단어
          freq = df_kim$freq,  # 빈도
          min.freq = 2,         # 최소 단어 빈도
          max.word = 200,       # 표현 단어 수
          random.order = F,     # 고빈도 단어 중앙 배치
          rot.per = .1,         # 회전 단어 비율
          scale = c(4, 0.3),    # 단어 크기 범위
          colors = pal)         # 색깔 목록

# no

set.seed(1234) # 난수 고정
wordcloud(words = df_no$word, # 단어
          freq = df_no$freq,  # 빈도
          min.freq = 2,         # 최소 단어 빈도
          max.word = 200,       # 표현 단어 수
          random.order = F,     # 고빈도 단어 중앙 배치
          rot.per = .1,         # 회전 단어 비율
          scale = c(4, 0.3),    # 단어 크기 범위
          colors = pal)         # 색깔 목록

# lee

set.seed(1234) # 난수 고정
wordcloud(words = df_lee$word, # 단어
          freq = df_lee$freq,  # 빈도
          min.freq = 2,         # 최소 단어 빈도
          max.word = 200,       # 표현 단어 수
          random.order = F,     # 고빈도 단어 중앙 배치
          rot.per = .1,         # 회전 단어 비율
          scale = c(4, 0.3),    # 단어 크기 범위
          colors = pal)         # 색깔 목록

# park

set.seed(1234) # 난수 고정
wordcloud(words = df_park$word, # 단어
          freq = df_park$freq,  # 빈도
          min.freq = 2,         # 최소 단어 빈도
          max.word = 200,       # 표현 단어 수
          random.order = F,     # 고빈도 단어 중앙 배치
          rot.per = .1,         # 회전 단어 비율
          scale = c(4, 0.3),    # 단어 크기 범위
          colors = pal)         # 색깔 목록
