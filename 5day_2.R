#-------------------------------------------------------------------------------

# api: 특정 사이트에 url로 접근하여 json 등의 형태로 전달해주는 것
#      실시간으로 계속 받는 것이 가능

# DB: Oracle, MySQL, NoSQL 등 
#     Server vs. Client
#     eg) Client(Chrome) Server(Naver web server)
#     Client가 url, ip주소를 통해 Server애 정보를 요청하면 Server가 응답 
#     eg) Client(User - Tools(SQL Developer)) Server(Oracle DBMS)
#     driver는 server와 client를 연결하는 통로
#     driver는 연결정보가 있어야 이용이 가능함(id, password 등)
#     localhost: 내 pc의 domain 주소 (내 pc의 위치) 
#                127.0.0.1 (루프백): 자신이 송신한 패킷을 그대로 수신한 효과

# web crawling: HTML 소스를 통해 XML로 받아서 Tag안 텍스트를 가져오는 것

#-------------------------------------------------------------------------------

### ojdbc.jar를 이용하여 데이터 접속을 위한 라이브러리

install.packages('RJDBC')
library(RJDBC)

# 오라클 드라이버 연결 경로 설정

driver <- JDBC('oracle.jdbc.OracleDriver',
               classPath = 'C:/DEV/Server/Oracle/product/12.2.0/dbhome_1/inventory/backup/2022-01-11_01-17-19PM/Scripts/ext/jlib/ojdbc8.jar')
driver

# 오라클 접속하기

conn <- dbConnect(driver,
                  'jdbc:oracle:thin:@//localhost:1521/orcl',
                  'busan', 'dbdb')
conn

#-------------------------------------------------------------------------------

# data 넣기

sql_in <- paste('Insert into test',
                '(AA,BB,CC)',
                "Values('a1','b1','c1')")
sql_in

in_stat = dbSendQuery(conn, sql_in)
in_stat

dbClearResult(in_stat)

# data 조회하기

sql_sel <- "SELECT * FROM test WHERE AA = 'a1'"
sql_sel

getData <- dbGetQuery(conn, sql_sel)
getData
 
getData$AA

str(getData)

########## 필수 conn 끊기**********

dbDisconnect(conn)
