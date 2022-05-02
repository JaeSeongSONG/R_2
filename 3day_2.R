#-------------------------------------------------------------------------------

### text mining

# 문자로 된 데이터에서 가치 있는 정보를 얻어 내는 분석 기법
# 형태소 분석: 문장을 구성하는 어절들이 어떤 품사로 되어 있는지 분석

# install.packages('usethis')
# usethis::edit_r_environ()
# PATH="${RTOOLS40_HOME}\usr\bin;${PATH}"
# Sys.which("make")

install.packages("rJava")
install.packages("remotes")
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))
library(KoNLP)
text <- "R은 통계 계산과 그래픽을 위한 프로그래밍 언어이자 소프트웨어 환경이자 프리웨어이다.[2] 뉴질랜드 오클랜드 대학의 로버트 젠틀맨(Robert Gentleman)과 로스 이하카(Ross Ihaka)에 의해 시작되어 현재는 R 코어 팀이 개발하고 있다. R는 GPL 하에 배포되는 S 프로그래밍 언어의 구현으로 GNU S라고도 한다. R는 통계 소프트웨어 개발과 자료 분석에 널리 사용되고 있으며, 패키지 개발이 용이해 통계 소프트웨어 개발에 많이 쓰이고 있다."
extractNoun(text)

# 패키지 로드

library(dplyr)
# useNIADic()

#-------------------------------------------------------------------------------

# 데이터 준비

setwd = 'C:/Sources/StudyR/Busan_202202_R'
getwd()
txt <- readLines('./Busan_202202_R/Data-20220207T040151Z-001/Data/hiphop.txt', encoding = "UTF-8")
txt

# 특수문자 제거

install.packages('stringr')
library(stringr)

txt <- str_replace_all(txt, '\\W', ' ')
# \W : 영소문자, 영대문자, 숫자, _(언더바)를 제외한 모든 문자
# str_replace_all(txt, '\\W', ' '): txt에서 \w를 ' '으로 바꾸는 것

txt

# 가사에서 명사 추출

nouns <- extractNoun(txt)
nouns

# 추출한 명사 list를 문자열 벡터로 변환, 단어별 빈도표 생성

wordcount <- table(unlist(nouns)) # list -> vector로 변환
wordcount

# 데이터 프레임으로 변환

df_word <- as.data.frame(wordcount, encoding = "UTF-8", stringsAsFactors = F)
# stringsAsFactors = F: 변수에 문자가 있을 경우 자동으로 factor타입으로 변환하지 않음

df_word

# 변수명 수정

df_word <- rename(df_word,
                  word = Var1,
                  freq = Freq)

df_word

# 두 글자 이상 단어 추출

df_word <- filter(df_word, nchar(word) >= 2)
# nchar: 문자열의 길이

top_20 <- df_word %>% 
  arrange(desc(freq)) %>% 
  head(20)

top_20

#-------------------------------------------------------------------------------

### 워드 클라우드 만들기

# 패키지 준비

install.packages('wordcloud')

# 패키지 로드

library(wordcloud)
library(RColorBrewer) # 색상표 라이브러리

# 단어 색상 목록 만들기

pal <- brewer.pal(9, 'Blues')
# Dark2 색상 목록에서 8개 색상 가져오기

# 워드 클라우드 생성

set.seed(1234) # 난수 고정
wordcloud(words = df_word$word, # 단어
          freq = df_word$freq,  # 빈도
          min.freq = 2,         # 최소 단어 빈도
          max.word = 200,       # 표현 단어 수
          random.order = F,     # 고빈도 단어 중앙 배치
          rot.per = .1,         # 회전 단어 비율
          scale = c(4, 0.3),    # 단어 크기 범위
          colors = pal)         # 색깔 목록

#-------------------------------------------------------------------------------

### 국정원 트위터 텍스트 마이닝

# 데이터 로드

twitter <- read.csv('./Busan_202202_R/Data-20220207T040151Z-001/Data/twitter.csv',
                    header = T,
                    stringsAsFactors = F,
                    fileEncoding = 'UTF-8')

twitter
str(twitter)

# 변수명 수정

twitter <- rename(twitter,
                  no = 번호, 
                  id = 계정이름,
                  date = 작성일,
                  tw = 내용)

twitter
head(twitter)

# 특수문자 제거

# twitter$tw <- str_replace_all(twitter$tw, '\\w', ' ')

head(twitter$tw)

# 트위터에서 명사 추출

nouns <- extractNoun(twitter$tw)
nouns

# 추출한 명사 list를 문자열 벡터로 변환, 단어별 빈도표 생성

wordcount <- table(unlist(nouns))

wordcount

# 데이터 프레임으로 변환

df_word <- as.data.frame(wordcount, stringsAsFactors = F)

df_word

# 변수명 수정

df_word <- rename(df_word,
                  word = Var1,
                  freq = Freq)

df_word

# 두 글자 이상 단어만 추출

df_word <- filter(df_word, nchar(word) >= 2)

# 상위 20개 추출

top20 <- df_word %>% 
  arrange(desc(freq)) %>% 
  head(20)

top20

# 단어 빈도 막대 그래프 만들기

library(ggplot2)

order <- arrange(top20, freq)$word

ggplot(data = top20, aes(x = word, y = freq)) +
  ylim(0, 2500) =
  geom_col() +
  coord_flip() +
  scale_x_discrete(limit = order) +               # 빈도 순서 변수 기준 막대 정렬
  geom_text(aes(label = freq), hjust = - 0.3)    # 빈도 표시

# 워드 클라우드 만들기

pal <- brewer.pal(8, 'Dark2')

set.seed(1234) # 난수 고정
wordcloud(words = df_word$word, # 단어
          freq = df_word$freq,  # 빈도
          min.freq = 2,         # 최소 단어 빈도
          max.word = 200,       # 표현 단어 수
          random.order = F,     # 고빈도 단어 중앙 배치
          rot.per = .1,         # 회전 단어 비율
          scale = c(4, 0.3),    # 단어 크기 범위
          colors = pal)         # 색깔 목록
