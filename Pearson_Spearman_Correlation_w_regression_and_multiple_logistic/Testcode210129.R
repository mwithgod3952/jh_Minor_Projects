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
#'@ST1_ Scatterplot
windows(height = 10, width = 10)
scatterplotMatrix(pe.r[c('생산량', '평균기온..C.4월','평균지면온도..C.4월','합계.일조시간.hr.6월','일조율...6월','평균최저기온..C.6월',
                            '평균최저기온..C.8월','평균지면온도..C.8월','평균상대습도...5월')], 
                  pch=19, col="grey", cex=1.2,
                  smooth=list(smoother=loessLine, spread=FALSE,lty.smooth=1,lwd.smooth=3,col.smooth="forestgreen"),
                  regLine=list(method=lm,lty=1,lwd=3,col="red"))


pe.r.lm <- lm(생산량~ 평균기온..C.4월+평균지면온도..C.4월+합계.일조시간.hr.6월+일조율...6월+평균최저기온..C.6월+
                   평균최저기온..C.8월+평균지면온도..C.8월+평균상대습도...5월, data= pe.r)
summary(pe.r.lm)
