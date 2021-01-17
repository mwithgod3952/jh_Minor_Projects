library(tidyverse)
library(dplyr)
library(lubridate)
library(zoo)
library(patchwork)
library(RColorBrewer)
library(ggplot2)
library(gridExtra)
library(car)
library(precintcon)
library(isotree)

# write.csv(METAR,"tdata.csv", row.names = FALSE)

getwd()
METAR <- read.csv("기상데이터_1979_2021.csv", header = TRUE)
str(METAR)
ncol(METAR)

existing <- colnames(METAR)
necessary <- c("기온","습도","풍속","이슬","일조","일사","지면","강수")

col.data <- data.frame(coln.x1 = existing)
col.data$coln.x2 <- NA
nrow(col.data)

col.data$coln.x2 = ifelse(str_detect(col.data$coln.x1,necessary[1]) == TRUE,1,
                         ifelse(str_detect(col.data$coln.x1,necessary[2]) == TRUE,2,
                                ifelse(str_detect(col.data$coln.x1,necessary[3]) == TRUE,3,
                                       ifelse(str_detect(col.data$coln.x1,necessary[4]) == TRUE,4,
                                              ifelse(str_detect(col.data$coln.x1,necessary[5]) == TRUE,5,
                                                     ifelse(str_detect(col.data$coln.x1,necessary[6]) == TRUE,6,
                                                            ifelse(str_detect(col.data$coln.x1,necessary[7]) == TRUE,7,
                                                                   ifelse(str_detect(col.data$coln.x1,necessary[8]) == TRUE,8,NA))))))))
                                                           
col.data = na.omit(col.data)
METAR_new = cbind(METAR[,1:3],METAR[,col.data[,1]])
METAR_new = group_by(METAR_new, "지점", "일시")

METAR_new.col.nm = as.data.frame(colnames(METAR_new))
METAR_new.col.nm$x2 <- NA 
METAR_new.col.nm$x2 = ifelse(str_detect(METAR_new.col.nm[,1], "나타난날") == FALSE,1,NA)
METAR_new.col.nm = na.omit(METAR_new.col.nm)
METAR_new <- METAR[,METAR_new.col.nm[c(-14:-15,-23,-25,-26),1]]
new.colnm <- colnames(METAR_new[4:ncol(METAR_new)])

# Object type conversion
METAR_new <- as.data.frame(METAR_new)
METAR_new %>% str()
Vars1 = c("지점", "지점명")
METAR_new[Vars1] <- map_df(.x = METAR_new[Vars1], .f = as.factor)

METAR$일시 <- as.Date(as.yearmon(METAR[,3]))
Year = format(METAR$일시, "%Y")
Months = format(METAR$일시, "%m")
METAR_new <- METAR_new %>% mutate(Year=Year, Months=Months)

sb.cnm <- METAR_new
METAR <- METAR_new[,c(1:3,ncol(sb.cnm)-1,ncol(sb.cnm),seq(4,ncol(sb.cnm) - 2, by = 1))]

