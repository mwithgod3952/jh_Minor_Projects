library(reshape2)
library(dplyr)
library("zoo")

df1 <-read.csv("card.csv", stringsAsFactors=FALSE)
df1.long_f = melt(data = df1,
                  variable.name = "Date",
                  value.name = "Performance"); df1.long_f

Ndate = gsub("X", "", df1.long_f$Date) 
Ndate1 = gsub("[[:punct:]]", "", Ndate)
Year = substr(Ndate1, 1, 4)
Day = substr(Ndate1, 5, 6)

df1.long_f = df1.long_f %>% mutate(Year, Day) %>% mutate(Date = as.yearmon(paste(Year, Day, sep = "-"))) 
df1 = df1.long_f[, c("한국표준산업분류별", "Date", "Year", "Day", "Performance")]
  
df1 = dplyr::arrange(df1, Year, Day, desc(Performance))

df1$한국표준산업분류별 = as.factor(df1$한국표준산업분류별)
levels(df1$한국표준산업분류별)

df.v1 = filter(df1, 한국표준산업분류별 == "교육서비스업")
df.v2 = filter(df1, 한국표준산업분류별 == "도매 및 소매업")
df.v3 = filter(df1, 한국표준산업분류별 == "보건업 및 사회복지 서비스업")
df.v4 = filter(df1, 한국표준산업분류별 == "사업시설관리 및 사업지원 서비스업")
df.v5 = filter(df1, 한국표준산업분류별 == "숙박 및 음식점업")
df.v6 = filter(df1, 한국표준산업분류별 == "예술, 스포츠 및 여가관련 서비스업")
df.v7 = filter(df1, 한국표준산업분류별 == "운수업")
df.v8 = filter(df1, 한국표준산업분류별 == "협회 및 단체, 수리 및 기타 개인 서비스업")

str(df.v1)
df.R_v1 = data.frame(Date <- df.v1[2], Performance <- df.v1[5]) 
df.R_v2 = data.frame(Date <- df.v2[2], Performance <- df.v2[5])
df.R_v3 = data.frame(Date <- df.v3[2], Performance <- df.v3[5])
df.R_v4 = data.frame(Date <- df.v4[2], Performance <- df.v4[5])
df.R_v5 = data.frame(Date <- df.v5[2], Performance <- df.v5[5])
df.R_v6 = data.frame(Date <- df.v6[2], Performance <- df.v6[5])
df.R_v7 = data.frame(Date <- df.v7[2], Performance <- df.v7[5])
df.R_v8 = data.frame(Date <- df.v8[2], Performance <- df.v8[5])


xlim = range(c(df.R_v1$Date, df.R_v2$Date))
ylim = range(c(MIN_Y, MAX_Y + 1000))

# ;
MIN_Y = min(min(df.R_v1[2]),
            min(df.R_v2[2]),
            min(df.R_v3[2]),
            min(df.R_v4[2]),
            min(df.R_v5[2]),
            min(df.R_v6[2]),
            min(df.R_v7[2]),
            min(df.R_v8[2]))

MAX_Y = max(max(df.R_v1[2]),
            max(df.R_v2[2]),
            max(df.R_v3[2]),
            max(df.R_v4[2]),
            max(df.R_v5[2]),
            max(df.R_v6[2]),
            max(df.R_v7[2]),
            max(df.R_v8[2]))

windows()
plot(df.R_v1$Date, df.R_v1$Performance, xlim = xlim, ylim = ylim,  
     type = 'o', col = 1,
     lwd = 3,
     cex.axis = 0.7,
     main = "2017-2020_월간국내카드승인실적",
     xlab = "년-월",
     ylab = "승인실적(단위 : 백)")
lines(df.R_v2$Date, df.R_v2$Performance, type = 'o', lwd = 3, col = 2)
lines(df.R_v3$Date, df.R_v3$Performance, type = 'o', lwd = 3, col = 3)
lines(df.R_v4$Date, df.R_v4$Performance, type = 'o', lwd = 3, col = 4)
lines(df.R_v5$Date, df.R_v5$Performance, type = 'o', lwd = 3, col = 5)
lines(df.R_v6$Date, df.R_v6$Performance, type = 'o', lwd = 3, col = 6)
lines(df.R_v7$Date, df.R_v7$Performance, type = 'o', lwd = 3, col = 7)
lines(df.R_v8$Date, df.R_v8$Performance, type = 'o', lwd = 3, col = 8)
legend(x = Date[1,], 
       y = 5000, 
       legend = c('교육서비스', 
                  '도매 및 소매업',                          
                  '보건_사회복지 섯비스',             
                  '사업시설관리_사업지원 서비스',       
                  '숙박_음식점',                        
                  '예술_스포츠_여가관련 서비스',       
                  '운수업',                                  
                  '협회_단체_수리_기타 개인서비스')       , 
       fill = c(1,2,3,4,5,6,7,8), 
       lty = 1,
       cex = 0.6)
