#' @Anomaly_detection_B.On_Boxplot
plot_list <- function(ranx){
  ggplot(METAR, aes(x = 1, y = ranx)) +
    geom_point(position ="jitter", alpha =0.05) + 
    geom_boxplot(width=0.8, outlier.size=3, outlier.shape=16, outlier.colour="#F03B20", alpha = 0.5) +
    stat_summary(geom = "point", fun.y = "mean", size =3, fill = "#E78AC3") +
    theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank()) +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
    scale_x_continuous(breaks = NULL)
}
#' Check from the 6th to the 23rd graph
windows()
# enter the number of the collum you want to check
do.call(grid.arrange,lapply(METAR[c(6)], plot_list))
#' 
#' @Combine_graphs
METAR.G <- METAR %>% 
  select(6:23) %>% 
  gather(key = 'measure', value = 'value')

ggplot(data = METAR.G, mapping = aes(x = 1, y = value)) +
  geom_point(position = 'jitter', color = "#525252", alpha = 0.01) +
  geom_boxplot(width = 0.8,
               outlier.size = 3,
               outlier.shape = 16,
               outlier.colour = "#F03B20",
               alpha = 0.8) +
  theme(panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_x_continuous(breaks = NULL) +
  facet_wrap(facets = 'measure', scales = 'free_y')
#'
#'
#' Colum detection that needs an outlier removal treatment_ 13t:8th, 21th:22th   
#' @Anomaly_detection1_B.On_Regression
lm_list <- function(lx){
  ggplot(lm(lx ~ ., data = METAR), aes(.fitted, .resid)) +
    geom_point() +
    geom_hline(yintercept = 0) +
    geom_smooth(se = FALSE) + 
    theme(panel.background = element_blank())
}
# enter the number of the collum you want to check
do.call(grid.arrange,lapply(METAR[c(22)], lm_list))

  #' @Pending..
  # rstandard(lm_1)
  # outlierTest(lm_1)
  # 
  # plot(rstandard(lm_1), main = colnames(METAR[15]))
  # plot(lm_1)                 
  # plot(cooks.distance(lm_1), pch = 4, cex = 1.5)
  # text(x = 1:length(cooks.distance(lm_1)), y = cooks.distance(lm_1), 
  #       labels = ifelse(cooks.distance(lm_1) > 4/nrow(METAR[15]),names(cooks.distance(lm_1)),""), col = "red")
  #' @concept
 
#' @Anomaly_detection2_B.On_Isolation.forest
windows()
#'
#' @최소상대습도_Drop_somthing
Min.r.hum <- METAR[c(3,13)] 
iforest_1 <- isolation.forest(Min.r.hum[2])

Min.r.hum$pred = predict.isolation_forest(iforest_1, Min.r.hum, type = "score")
Min.r.hum$outlier <- as.factor(ifelse(Min.r.hum$pred >=0.80, "outlier", "normal"))

ln_1 = Min.r.hum[which(Min.r.hum[4] == "outlier"),2]

mh <- ggplot(Min.r.hum, aes(x = Min.r.hum[,1], y = Min.r.hum[,2], color = outlier)) + 
  geom_point(shape = 1, alpha = 0.8) +
  
  geom_hline(yintercept = min(ln_1), linetype ='dashed', color ='red', size=0.1) + 
  geom_vline(xintercept = Min.r.hum[which(Min.r.hum[4] == "outlier"),1][1], linetype = 'dashed', color='red', size = 0.1) + 
  annotate("text", x = Min.r.hum[which(Min.r.hum[4] == "outlier"), 1], y=ln_1 - 1, label=ln_1, size=3) +
  
  labs(x = "년-월_(도시별)", y = "최소상대습도") +
  labs(alpha = "", colour="Legend") +
  
  theme(axis.title = element_text(size = 12, face='bold')) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.title = element_blank()) + 
  scale_color_manual(values=c("#377EB8", "#D95F02")); mh 

#'
#' @_월합강수량.00.24h만.mm_Not_dropping
Mon.prec <- METAR[c(3,14)] 
iforest_2 <- isolation.forest(Mon.prec[2])

Mon.prec$pred = predict.isolation_forest(iforest_2, Mon.prec, type = "score")
Mon.prec$outlier <- as.factor(ifelse(Mon.prec$pred >=0.90, "outlier", "normal"))

mop <- ggplot(Mon.prec, aes(x = Mon.prec[,1], y = Mon.prec[,2], color = outlier)) + 
  geom_point(shape = 1, alpha = 0.8) +
  
  labs(x = "년-월_(도시별)", y = "월합강수량_(단위:mm)") +
  labs(alpha = "", colour="Legend") +
  
  theme(axis.title = element_text(size = 12, face='bold')) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.title = element_blank())
  
  scale_color_manual(values=c("#377EB8", "#D95F02")) 
#'
#' @일최다강수량_Drop_somthing
most.prec <- METAR[c(3,15)]
iforest_3 <- isolation.forest(most.prec[2])

most.prec$pred = predict.isolation_forest(iforest_3, most.prec, type = "score")
most.prec$outlier <- as.factor(ifelse(most.prec$pred >=0.80, "outlier", "normal"))

ln_3 = most.prec[which(most.prec[4] == "outlier"),2]

mp <- ggplot(most.prec, aes(x = most.prec[,1], y = most.prec[,2], color = outlier)) + 
  geom_point(shape = 1, alpha = 0.8) +
  
  geom_hline(yintercept = min(ln_3), linetype ='dashed', color ='red', size=0.1) + 
  geom_vline(xintercept = most.prec[which(most.prec[4] == "outlier"),1][3], linetype = 'dashed', color='red', size = 0.1) + 
  annotate("text", x = most.prec[which(most.prec[4] == "outlier"), 1], y=ln_3 - 13, label=ln_3, size=3) +
  
  labs(x = "년-월_(도시별)", y = "일최다강수량_(단위:mm)") +
  labs(alpha = "", colour="Legend") +
  
  theme(axis.title = element_text(size = 12, face='bold')) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.title = element_blank()) + 
  
  scale_color_manual(values=c("#377EB8", "#D95F02")); mp 
#'
#' @평균풍속.m.s_Drop_somthing
avg.ws <- METAR[c(3,16)]
iforest_4 <- isolation.forest(avg.ws[2])

avg.ws$pred = predict.isolation_forest(iforest_4, avg.ws, type = "score")
avg.ws$outlier <- as.factor(ifelse(avg.ws$pred >=0.80, "outlier", "normal"))

ln_4 = avg.ws[which(avg.ws[4] == "outlier"),2]

aws <- ggplot(avg.ws, aes(x = avg.ws[,1], y = avg.ws[,2], color = outlier)) + 
  geom_point(shape = 1, alpha = 0.8) +
  
  geom_hline(yintercept = c(min(ln_4[which(ln_4 > 10)]), max(ln_4[which(ln_4 < 10)])),
             linetype ='dashed', color ='red', size=0.1) + 
  geom_vline(xintercept = avg.ws[which(avg.ws[4] == "outlier"),1][1], linetype = 'dashed', color='red', size = 0.1) + 
  annotate("text", x = avg.ws[which(avg.ws[4] == "outlier"), 1], y=ln_4 - 0.2, label=ln_4, size=3) +
  
  labs(x = "년-월_(도시별)", y = "평균풍속_(단위:m.s)") +
  labs(alpha = "", colour="Legend") +
  
  theme(axis.title = element_text(size = 12, face='bold')) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.title = element_blank()) +
  
  scale_color_manual(values = c("#377EB8", "#D95F02")); aws
#'
#' @최대풍속.m.s_Drop_somthing
max.ws <- METAR[c(3,17)]
iforest_5 <- isolation.forest(max.ws[2])

max.ws$pred = predict.isolation_forest(iforest_5, max.ws, type = "score")
max.ws$outlier <- as.factor(ifelse(max.ws$pred >=0.83, "outlier", "normal"))

ln_5 = max.ws[which(max.ws[4] == "outlier"),2]

mws <- ggplot(max.ws, aes(x = max.ws[,1], y = max.ws[,2], color = outlier)) + 
  geom_point(shape = 1, alpha = 0.8) +
  
  geom_hline(yintercept = min(ln_5),
             linetype ='dashed', color ='red', size=0.1) + 
  geom_vline(xintercept = max.ws[which(max.ws[4] == "outlier"),1][1], linetype = 'dashed', color='red', size = 0.1) + 
  annotate("text", x = max.ws[which(max.ws[4] == "outlier"), 1], y=ln_5 - 0.7, label=ln_5, size=3) +
  
  labs(x = "년-월_(도시별)", y = "최대풍속_(단위:m.s)") +
  labs(alpha = "", colour="Legend") +
  
  theme(axis.title = element_text(size = 12, face='bold')) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.title = element_blank()) +
  
  scale_color_manual(values = c("#377EB8", "#D95F02")); mws
#'
#' @최대순간풍속.m.s_Drop_somthing
mx.iws <- METAR[c(3,18)]
iforest_6 <- isolation.forest(mx.iws[2])

mx.iws$pred = predict.isolation_forest(iforest_6, mx.iws, type = "score")
mx.iws$outlier <- as.factor(ifelse(mx.iws$pred >=0.78, "outlier", "normal"))

ln_6 = mx.iws[which(mx.iws[4] == "outlier"),2]

iws <- ggplot(mx.iws, aes(x = max.ws[,1], y = mx.iws[,2], color = outlier)) + 
  geom_point(shape = 1, alpha = 0.8) +
  
  geom_hline(yintercept = c(2.6,54.4),
             linetype ='dashed', color ='red', size=0.1) + 
  geom_vline(xintercept = mx.iws[which(mx.iws[4] == "outlier"),1][c(1:2,7)], linetype = 'dashed', color='red', size = 0.1) + 
  annotate("text", x = mx.iws[which(mx.iws[4] == "outlier"), 1], y=ln_6 - 0.9, label=ln_6, size=3) +
  
  labs(x = "년-월_(도시별)", y = "최대순간풍속_(단위:m.s)") +
  labs(alpha = "", colour="Legend") +
  
  theme(axis.title = element_text(size = 12, face='bold')) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.title = element_blank()) +
  
  scale_color_manual(values = c("#377EB8", "#D95F02")); iws
#'
#' @합계.일조시간.hr_Drop_somthing
dr.bs <- METAR[c(3,21)]
iforest_7 <- isolation.forest(dr.bs[2])

dr.bs$pred = predict.isolation_forest(iforest_7, dr.bs, type = "score")
dr.bs$outlier <- as.factor(ifelse(dr.bs$pred >=0.8, "outlier", "normal"))

ln_7 = dr.bs[which(dr.bs[4] == "outlier"),2]

dbs <- ggplot(dr.bs, aes(x = dr.bs[,1], y = dr.bs[,2], color = outlier)) + 
  geom_point(shape = 1, alpha = 0.8) +
  
  geom_hline(yintercept = c(min(ln_7[which(ln_7 > 10)]), max(ln_7[which(ln_7 < 10)])),
             linetype ='dashed', color ='red', size=0.1) + 
  geom_vline(xintercept = dr.bs[which(dr.bs[4] == "outlier"),1][c(1,4)], linetype = 'dashed', color='red', size = 0.1) + 
  annotate("text", x = dr.bs[which(dr.bs[4] == "outlier"), 1], y=ln_7 - 8, label=ln_7, size=3) +
  
  labs(x = "년-월_(도시별)", y = "합계.일조시간_(단위:hr)") +
  labs(alpha = "", colour="Legend") +
  
  theme(axis.title = element_text(size = 12, face='bold')) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.title = element_blank()) +
  
  scale_color_manual(values = c("#377EB8", "#D95F02")); dbs
#'
#' @일조율_Drop_somthing
pr.sh <- METAR[c(3,22)]
iforest_8 <- isolation.forest(pr.sh[2])

pr.sh$pred = predict.isolation_forest(iforest_8, pr.sh, type = "score")
pr.sh$outlier <- as.factor(ifelse(pr.sh$pred >=0.8, "outlier", "normal"))

ln_8 = pr.sh[which(pr.sh[4] == "outlier"),2]

prs <- ggplot(pr.sh, aes(x = pr.sh[,1], y = pr.sh[,2], color = outlier)) + 
  geom_point(shape = 1, alpha = 0.8) +
  
  geom_hline(yintercept = c(min(ln_8[which(ln_8 > 10)]), max(ln_8[which(ln_8 < 10)])),
             linetype ='dashed', color ='red', size=0.1) +
  geom_vline(xintercept = pr.sh[which(pr.sh[4] == "outlier"),1][c(1:2)], linetype = 'dashed', color='red', size = 0.1) +
  annotate("text", x = pr.sh[which(pr.sh[4] == "outlier"), 1], y=ln_8 - 1.5, label=ln_8, size=3) +
  
  labs(x = "년-월_(도시별)", y = "일조율") +
  labs(alpha = "", colour="Legend") +
  
  theme(axis.title = element_text(size = 12, face='bold')) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.title = element_blank()) +
  
  scale_color_manual(values = c("#377EB8", "#D95F02")); prs