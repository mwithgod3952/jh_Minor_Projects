# Divided by month & combined in wide form
METAR.fv <- bind_rows(City_list)
M.list <- METAR.fv %>% group_by(Months) %>% group_split(Months, .keep = TRUE) 

# Relabel columns by month & Creat id for merge and modeing
for(ml in 1:12){
  nm <- do.call(data.frame, M.list[[ml]])
  nm$id = paste0(nm[,1],nm[,20])
  colnames(nm) <- c("Year","Months",paste0(colnames(M.list[[ml]][3:19]),ml,"월"), "City","id")
  nm = nm[c(21,20,1:19)]
  nm <- nm[,-2:-3]
  assign(paste0("MF", ml), nm)
}

MF_list = list(MF12,MF11,MF10,MF9,MF8,MF7,MF6,MF5,MF4,MF3,MF2,MF1)
MF_full <- MF_list %>% reduce(full_join, by = "id", all.x = TRUE)

# Remove duplicate matrix
MF_full <- MF_full[,-which(substr(colnames(MF_full),1,6) == "Months")]

# Load output file to be merged
getwd()
dir()
setwd("D:/D_P1_T/DT")
setwd("./Meteoro_DT")
DT.Oni.p <- read.csv("DT.Oni.p.csv")

colnames(DT.Oni.p) <- c("년도","도시","생산량","id","Class")

F.nal_df = merge(DT.Oni.p, MF_full, by= "id", all.x= TRUE)
# F.nal_df = F.nal_df[,c(-1:-4)]  

# Input Mice
#' @for(fn in 2:ncol(F.nal_df)){
#   F.nal_df[is.na(F.nal_df[fn]),fn] = median(F.nal_df[,fn], na.rm = T)
# }

F.nal_df[1:10,1:6] 

#'@-----------------------------------------------------------------------------Nomality_Test
LF.results <- do.call(rbind, lapply(c(4,6:ncol(F.nal_df)), function(x){
  lil.results= lillie.test(na.omit(F.nal_df[,x]))
  lil.pval = lil.results$p.value
  return(data.frame(lfp = lil.pval))
}
))

SW.results <- do.call(rbind, lapply(c(4,6:ncol(F.nal_df)), function(x){
  sha.results= shapiro.test(na.omit(F.nal_df[,x]))
  sha.pval= sha.results$p.value
  return(data.frame(swp = sha.pval))
}
))
stan.r <- cbind(LF.results, SW.results)
stan.r$r <- ifelse(stan.r$lfp<=0.05 & stan.r$swp<=0.05,'non-passing','passing')

nrow(filter(stan.r,stan.r$r=='passing'))
#'@65__32%
nrow(filter(stan.r,stan.r$r=='non-passing'))
#'@140__68%

windows()
ggplot(F.nal_df, aes(x=F.nal_df[,4])) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white",
                 breaks=seq(min(F.nal_df[,4],na.rm=TRUE), max(F.nal_df[,4],na.rm=TRUE), by=100))+
  geom_density(alpha=.2, fill="red")+
  labs(x=expression('생산량(ha)'))+
  theme_bw()

#'Group designation for @relationship-test
# Group1 : For_ Spearman Correalation Analysis
F.nal.sp <- F.nal_df[,c(4,(which(stan.r[-1,]$r == "non-passing")+ 5))]

#'@ST1_ Partial correlation
sp.cor <- ppcor::pcor(na.omit(F.nal.sp), method = "spearman")
sp.cor.df <- data.frame(estimate= sp.cor$estimate[,1], p.value= sp.cor$p.value[,1])
sp.cor.df <- sp.cor.df %>% mutate(검정= ifelse(p.value <0.05,"유의",""))

sp.r <- F.nal.sp[,which(sp.cor.df$검정 == "유의")]
sp.r <- na.omit(sp.r)

#'@ST2_ Correlation
windows()
corrplot(cor(sp.r, method= "spearman"))
windows()
col <- colorRampPalette(c("#BB4444","#EE9988","#FFFFFF","#77AADD","#4477AA"))
corrplot(cor((na.omit(sp.r))),
         method= "color",
         col = col(200),
         type= "lower",
         order= "hclust",
         addCoef.col= "black",
         tl.cex = 0.7,
         tl.col= "black",
         tl.srt= 45,
         diag= F)
?corrplot
# Group2 : For_ Pearson Correalation Analysis
#'@ST1_ Partial correlation
F.nal.pe <- F.nal_df[,c(4,(which(stan.r[-1,]$r == "passing")+ 5))]

pe.cor <- ppcor::pcor(na.omit(F.nal.pe), method = "pearson")
pe.cor.df <- data.frame(estimate= pe.cor$estimate[,1], p.value= pe.cor$p.value[,1])
pe.cor.df <- pe.cor.df %>% mutate(검정= ifelse(p.value <0.05,"유의",""))

pe.r <- F.nal.pe[,which(pe.cor.df$검정 == "유의")]
pe.r <- na.omit(pe.r)

#'@ST2_ Correlation
windows(height = 10, width = 10)
corrplot(cor(pe.r, method= "pearson"), method="ellipse")
windows(height = 10, width = 10)
corrplot(cor((na.omit(pe.r))),
         method= "color",
         col = col(200),
         type= "lower",
         order= "hclust",
         addCoef.col= "black",
         number.cex=0.8,
         tl.cex = 0.6,
         tl.col= "black",
         tl.srt= 45,
         diag= F)
#'@-----------------------------------------------------------------------------

#'@Multiple_linear_regression ** only for Group2 analyzed by Pearson correlation
model_1 <- lm(생산량~ .,data= pe.r)
summary(model_1)
windows()
plot(model_1)

#' @ST1__ Stepwise regression
#' @1) -1__ Backward regression 
Full_model <- lm(생산량~ .,data= pe.r)
Reduced.model <- step(Full_model, direction= "backward")
summary(Reduced.model)
#' Adjusted R-squared:  @0.7968
print(Reduced.model$anova)

anova(model_1, Reduced.model)
# F-test pvalue = 0.805


#' @1) -2__ Forward regression 
Min.model <- lm(생산량~ 1, data= pe.r)
FWD.model <- step(Min.model, direction= "forward", scope= (생산량~ 평균상대습도...12월 + 평균상대습도...10월 + 평균최저기온..C.8월 + 
                                                                최대풍속.풍향.16방위.8월 + 평균지면온도..C.8월 + 최고기온..C.7월 + 
                                                                평균최저기온..C.6월 + 합계.일조시간.hr.6월 + 일조율...6월 + 
                                                                평균지면온도..C.6월 + 평균상대습도...5월 + 최대순간풍속.풍향.16방위.5월 + 
                                                                평균기온..C.4월 + 평균최고기온..C.4월 + 평균지면온도..C.4월 + 
                                                                평균지면온도..C.3월 + 평균이슬점온도..C.2월 + 평균최고기온..C.1월 + 
                                                                평균이슬점온도..C.1월), trace=0)
summary(FWD.model)
#' Adjusted R-squared:  @0.7937
print(FWD.model$anova)

#' @1) -2__ all subset regression 
Leaps <- regsubsets(생산량~ 평균상대습도...12월 + 평균상대습도...10월 + 평균최저기온..C.8월 + 
                         최대풍속.풍향.16방위.8월 + 평균지면온도..C.8월 + 최고기온..C.7월 + 
                         평균최저기온..C.6월 + 합계.일조시간.hr.6월 + 일조율...6월 + 
                         평균지면온도..C.6월 + 평균상대습도...5월 + 최대순간풍속.풍향.16방위.5월 + 
                         평균기온..C.4월 + 평균최고기온..C.4월 + 평균지면온도..C.4월 + 
                         평균지면온도..C.3월 + 평균이슬점온도..C.2월 + 평균최고기온..C.1월 + 
                         평균이슬점온도..C.1월,
                       data= pe.r, nbest= 4)
summary.out <- summary(Leaps)
as.data.frame(summary.out$outmat)

windows(height = 7,width = 7)
par(oma=c(5,1,0.5,1))
plot(Leaps, scale="adjr2")

which.max(summary.out$adjr2)
summary.out$which[29,]
Leaps.model <- lm(생산량~  합계.일조시간.hr.6월+일조율...6월+평균지면온도..C.6월+평균기온..C.4월+평균최고기온..C.4월+
                       평균이슬점온도..C.2월+평균최고기온..C.1월+평균이슬점온도..C.1월,
                     data= pe.r)

summary(Leaps.model)
#' anova bt Backward regression and Subset regression
anova(Reduced.model, Leaps.model)
# p.value = 0.003334

windows()
plot(Leaps.model)

windows(height = 10, width = 10)
scatterplotMatrix(pe.r[c('생산량','합계.일조시간.hr.6월','일조율...6월','평균지면온도..C.6월','평균기온..C.4월','평균최고기온..C.4월',
                              '평균이슬점온도..C.2월','평균최고기온..C.1월','평균이슬점온도..C.1월',
                         '최대풍속.풍향.16방위.8월','평균지면온도..C.8월','평균지면온도..C.4월')], 
                  pch=19, col="grey", cex=1.2,
                  smooth=list(smoother=loessLine, spread=FALSE,lty.smooth=1,lwd.smooth=3,col.smooth="forestgreen"),
                  regLine=list(method=lm,lty=1,lwd=3,col="red"))

#'@'최대풍속.풍향.16방위.8월'은 변동화율이 0에 가까움으로 제거하기로 결정,
#'@'평균지면온도..C.8월','평균지면온도..C.4월'은 설명력을 높일 수 있도록 Feature engineering 도모

vif(Leaps.model)
vif(Reduced.model)
