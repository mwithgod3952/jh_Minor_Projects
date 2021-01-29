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

# Group2 : For_ Pearson Correalation Analysis
F.nal.pe <- F.nal_df[,c(4,(which(stan.r[-1,]$r == "passing")+ 5))]

 
pcor(F.nal.sp[1],F.nal.sp[3])


