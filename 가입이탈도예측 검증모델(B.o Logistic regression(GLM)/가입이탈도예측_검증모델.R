# Binomial logistic regression analysis(이항로지스틱회귀분석)
# 1)_ 프로젝트 목적 : 서비스 제공지 변경 유무 정보를 내포하는 Churn 객체를 결과변수로 보고,
#                     나머지 Feature Column들이 예측변수로서 고객 이탈도에 미치는 영향을 확률적으로 증명한다.
# 1 - 1)_ 기술배경  : Binomial logistic regression analysis(Core Technology), hi-square test, Confusion matrix, Feature Engineering 

# 2)_ Binomial logistic regression analysis
#         : 결과변수 즉, 종속변수가 이분형 번주를 가짐으로 본 프로젝트에 적합
#           예측변수 즉,독립변수로부터 결과변수 즉, 종속변수의 범주예측 (확률에 대한 범주를 에측한다는 것이 linear regression model 과 상이 *.*.*)         
#         : 결과변수의 예측값은 θ(Z) Model을 통해 0과 1사이에서 확률을 정의(Sigmoid Function에 대하여_ dθ(Z)/dz = θ(Z)*(1-θ(Z))를 활용해, Loss값을 도출)  
#         : x_axis = -infinity ~ +infinity, y_axis =  θ(Z)일 때를 한 예로, digit 0.5, Where, log(Odds) = 0를 기준값으로 이보다 큰 값을 유의한 값이라 주장   

library(dplyr)
library(modeldata)
# Step1_ 데이터 선정

data(mlc_churn)
str(mlc_churn) %>%  head()

churn <- mlc_churn
churn <- churn[-c(1,3)]
churn %>%  head() %>% str()

# 본 데이터는 이탈여부가 '참'인 값이 1로 '거짓'이 2로 Labeling 되어 있으므로 분석에 용이한 Labeling으로 변경
# 이탈여부가 '거짓'인 값이 1로 '참'이 2로 Labeling
churn$churn <- factor(ifelse(churn$churn == "no", 1, 2),
                      levels=c(1,2),
                      labels=c("no","yes"))
str(churn)

# Step2_ : Training data set 70%, Testing data set 30%로 Sampling
n <- nrow(churn)
set.seed(seed = 1234)
index <- sample(n, size = n*0.7, replace = FALSE)
churn.Train <- churn %>% slice(index)
churn.Test <- churn %>% slice(-index)

# Or
# churn.Train <- churn[1:3333,]
# churn.Test <- churn[3334:5000,]

prop.table(table(churn.Train$churn))
prop.table(table(churn.Test$churn))

# no         yes 
# 0.8562857  0.1437143 
# 
# no         yes 
# 0.864      0.136 

# 평가 : 0.14, 0.13으로 서비스 이탈자에 대한 Proportion 유사성 확인 

# Step3_   
# : GLM에 결과변수와 예측변수를 지정
# : 결과변수는, 이탈여부를 나타내는 Churn / 예측변수는, 결과변수를 제외한 모든 변수
# : 인수 'family', 결과변수의 확률분포에 해당하는 함수와, 함수 내의 link함수를 지정
# : 예측변수에 보함된 범주형 변수는 모두 더미변수로 변환되기 때무넹 별도의 변수변환 작업은 Skip
#     Ex )
        # voice_mail_plan(yes) < level of yes = 1           
        # voice_mail_plan(yes) < level of yes = 1
# : 회귀계수_ Coefficients < Estimate checking
# : 이 분석의 경우 고객이탈의 확률이 미이탈의 몇 배(link function : log(odds)인지를 확인
#     전제 ) 
        # 다른 독림변수의 변화가 없는 가정
        # log(odds)의 변화량_ Coefficients, Where, 해당변수가 한 단위만큼 변화  
        # +_양의 변화와 -_음의 변화를 구분하여 해석

churn.logit = glm(churn ~ ., data = churn.Train,
                  family=binomial(link = "logit"))
summary(churn.logit)
exp(coef(churn.logit))
# p / 1-p 해석  
#     : international_planyes 
#     : 7.711821e+00
#     : Dummy variable international_plan(yes)_ 0에서 1로 변할 때, 
#       Odds ratio(고객의 이탈확률 / 미이탈확률)는 770% 증가

#     : voice_mail_planyes
#     : 1.319919e-01 = About 0.13times increase
#     : Dummy variable international_plan(yes)_ 0에서 1로 변할 때, 
#       Odds ratio(고객의 이탈확률 / 미이탈확률)는 -87% 감소

#     : total_day_charge
#     : 4.539006e+00
#     : 만약, total_day_charge 일반적 연속형이 아닌 2배씩 관측되었다고 가정하면, 4.539^2 증가가

# Goodness of fit tests(모형의 적합도 = 모델의 비적합도 검증)
  # Deviance : 이탈도
  # Null devience_ 상수항만 취급, Residual deviance : 예측변수를 모두 포함한 이항로지스틱회귀모델의 이탈도
  # 3332 - (17의 예측변수 + 상수항 1) = 3315가 Residual deviance의 자유도
  # Null deviance, Residual deviance 간 Chi-square test : 자유도의 감소폭 만큼 충분히 이탈도가 감소하는지를 검증 

    # Null deviance: 2758.3  on 3332
    # Residual deviance: 2158.7  on 3315
pchisq(q = 2758.3-2158.7, df = 3332-3315, lower.tail = FALSE)
# Or
pchisq(q = churn.logit$null.deviance -  churn.logit$deviance,
       df = churn.logit$df.null - churn.logit$df.residual, lower.tail = FALSE)
    # 1.731898e-116
    # 결론 : 유의 > 자유도의 차이 만큼 충분한 크기의 이탈도 개선효과가 있음

# ------------------------------------------------------------------------------Below_ 예측구간

# 1)_ pridict function : 새로운 예측변수 값이 주어졌을 때 결과변수 값1 에 대한 확률 예측

# pridict function은 기본적으로 log(odds)의 값을 도출_  변환필요_ response type of p
# 0.5에 기준하여 이탈 유무 판정을 위한 factor형 변환
churn.logit.pred <- predict(churn.logit, newdata = churn.Test, type = "response")
churn.logit.pred %>% head()

churn.logit.pred = factor(churn.logit.pred > 0.5,
                          levels = c(FALSE,TRUE),
                          labels = c("no","yes"))
head(churn.logit.pred)
table(churn.logit.pred)

# 2)_혼동행렬 생성
table(churn.Test$churn,churn.logit.pred,
      dnn = c("Actual", "Predicted"))
#         Predicted
# Actual  no   yes
# no      1414 29
# yes     181  43

mean(churn.Test$churn == churn.logit.pred) #_ 정확도 : About 87%


# 3)_ 유의수준 0.05에서 통계적으로 유의하지 않은 변수들 탐지 및 제거고려
# Coefficients:\\

# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                   -8.6515638  0.7243142 -11.944  < 2e-16 ***
# .
# .
# .
#'@total_day_minutes             -0.2441993  3.2742224  -0.075 0.940547    
#'@total_day_calls                0.0031962  0.0027612   1.158 0.247048    
#'@total_day_charge               1.5127081 19.2601862   0.079 0.937398    
#'@total_eve_minutes              0.8186945  1.6357258   0.501 0.616717    
#'@total_eve_calls                0.0010579  0.0027826   0.380 0.703817    
#'@total_eve_charge              -9.5463678 19.2437266  -0.496 0.619840    
#'@total_night_minutes           -0.1238287  0.8764906  -0.141 0.887650    
#'@total_night_calls              0.0006993  0.0028419   0.246 0.805628    
#'@total_night_charge             2.8338084 19.4769043   0.145 0.884319    
#'@.
#'@.
#'@.
#'
# 3)_1_ Step Function
churn.logit.rech <- step(churn.logit)
summary(churn.logit.rech)
#'@Coefficients:
#'@Estimate Std. Error z value Pr(>|z|)    
#'@(Intercept)                   -8.067161   0.515870 -15.638  < 2e-16 ***
#'@international_planyes          2.040338   0.145243  14.048  < 2e-16 ***
#'@voice_mail_planyes            -2.003234   0.572352  -3.500 0.000465 ***
#'@number_vmail_messages          0.035262   0.017964   1.963 0.049654 *  
#'@total_day_charge               0.076589   0.006371  12.022  < 2e-16 ***
#'@total_eve_minutes              0.007182   0.001142   6.290 3.17e-10 ***
#'@total_night_charge             0.082547   0.024653   3.348 0.000813 ***
#'@total_intl_calls              -0.092176   0.024988  -3.689 0.000225 ***
#'@total_intl_charge              0.326138   0.075453   4.322 1.54e-05 ***
#'@number_customer_service_calls  0.512256   0.039141  13.087  < 2e-16 ***

table(churn.Test$number_customer_service_calls)
testdata <- data.frame(number_customer_service_calls=c(0:7),
                       international_plan="no",
                       voice_mail_plan="no",
                       number_vmail_messages=mean(churn.Test$number_vmail_messages),
                       total_day_minutes=mean(churn.Test$total_day_minutes),
                       total_day_charge=mean(churn.Test$total_day_charge),              
                       total_eve_charge=mean(churn.Test$total_eve_charge),               
                       total_night_charge=mean(churn.Test$total_night_charge),             
                       total_intl_minutes=mean(churn.Test$total_intl_minutes),             
                       total_intl_calls=mean(churn.Test$total_intl_calls)) 

# number_customer_service_calls international_plan voice_mail_plan number_vmail_messages total_day_minutes total_day_charge total_eve_charge total_night_charge total_intl_minutes total_intl_calls       prob
# 1                             0                 no              no              7.908667          180.7481         30.72772         17.05875            8.88298           10.21447            4.448 0.05774525
# 2                             1                 no              no              7.908667          180.7481         30.72772         17.05875            8.88298           10.21447            4.448 0.09457642
# 3                             2                 no              no              7.908667          180.7481         30.72772         17.05875            8.88298           10.21447            4.448 0.15113140
# 4                             3                 no              no              7.908667          180.7481         30.72772         17.05875            8.88298           10.21447            4.448 0.23280945
# 5                             4                 no              no              7.908667          180.7481         30.72772         17.05875            8.88298           10.21447            4.448 0.34090249
# 6                             5                 no              no              7.908667          180.7481         30.72772         17.05875            8.88298           10.21447            4.448 0.46853287
# 7                             6                 no              no              7.908667          180.7481         30.72772         17.05875            8.88298           10.21447            4.448 0.60041745
# 8                             7                 no              no              7.908667          180.7481         30.72772         17.05875            8.88298           10.21447            4.448 0.71918947

testdata$prob <- predict(churn.logit.rech, newdata=testdata, type = "response")  
testdata[c("number_customer_service_calls", "prob")]

# number_customer_service_calls       prob
# 1                             0 0.05774525 _5.9%
# 2                             1 0.09457642
# 3                             2 0.15113140
# 4                             3 0.23280945
# 5                             4 0.34090249
# 6                             5 0.46853287
# 7                             6 0.60041745
# 8                             7 0.71918947 _69.3%

#' @해석 : 다른 예측변수들이 일정_ '@전화횟수가 '@7회 
#' @49.3%까지 이탈확률증가

#' @과산포문제
#' @Case,발생가능성 : 결과변수에 실제 관측된 분산이 이항분포로부터 기대되는 분산보다 더 클 때,
#' @이탈도:자유도비율_ 이 1을 크게 상회할 시 과산포를 의심.

deviance(churn.logit.rech)/df.residual(churn.logit.rech)

#' @과산포문제확률정검증
fit.origin <- glm(churn ~ international_plan +
                          voice_mail_plan + 
                          number_vmail_messages +
                          total_day_minutes +
                          total_day_charge +           
                          total_eve_charge +               
                          total_night_charge +          
                          total_intl_minutes +             
                          total_intl_calls +
                          number_customer_service_calls,
                          family = binomial(),
                          data=churn.Train)
fit.overdis <- glm(churn ~ international_plan +
                            voice_mail_plan + 
                            number_vmail_messages +
                            total_day_minutes +
                            total_day_charge +           
                            total_eve_charge +               
                            total_night_charge +          
                            total_intl_minutes +             
                            total_intl_calls +
                            number_customer_service_calls,
                            family = quasibinomial(),
                            data=churn.Train)
pchisq(summary(fit.overdis)$dispersion * fit.origin$df.residual,
       fit.origin$df.residual, lower.tail = FALSE)
