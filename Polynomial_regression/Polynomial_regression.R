#' @Polynomial_regression_analysis

#' 프로젝트 목적 : @'소득'에 따른 @'교육기간'의 증-감소 현황파학(관계규명)이 목적. 

#' 모델개요 : 독립변수의 @nth_order_plynomal_modeling (y = β0 + β1x + β2x^2) 즉, 여러개의 독립변수를 회귀모델에 Input(w. I())
#' 모델특징 : 관측값을 통과하는 추세선(loess을 그렸을 때 <n-1>개의 굴정이 관찰되면, n차 다항식으로 modeling.

#'@----------------------------------------------------------------------------- 
library(car)
library(dplyr)
#' @ST1_ 단순선형회귀를 통해 @'소득', @'교육'간의  관계성 파학
Prestige.lm <- lm(income ~ education, data = Prestige)
summary(Prestige.lm)

#' @Coefficients:
#                Estimate Std.    Error       value Pr(>|t|)    
#' (Intercept)        -2853.6     1407.0     -2.028   0.0452 *  
#' education           @898.8      127.0      7.075 2.08e-10 ***
#' Intercept : -2853.6 / Slope : 898.8
 
#' @결과_ Education 기간이 1년 증가할 때 마다 소득은 898.8$ 증가를 확인

#' @ST2_ 증가율의 변화 파학 목적의 Visualization.   
windows()
plot(Prestige$income ~ Prestige$education,
     col = "#377EB8", pch = 19,
     xlab = "Education (years)", ylab = "income (dollors)",
     main = "Education and Income")
abline(Prestige.lm, col = "salmon", lwd = 1)

#' @ST3_ Education의 mean값을 중심으로 데이터 분할 및 2차 회귀분석    
lm(income ~ education, data = Prestige,
   subset = (education > mean(Prestige$education)))
# Coefficients:
#   (Intercept)    education  
#'       -10299         @1455

#' @increace_per_455$
 
lm(income ~ education, data = Prestige,
   subset = (education <= mean(Prestige$education)))
# Coefficients:
#   (Intercept)    education  
#'      2546.6        @281.8

#' @increace_per_281.8$

#' @결과_ 평균보다 더 많은 교육을 받은 직군은, 1455$의 소득 증가로, 1173.2$ 더 많은 소득증가의 특성이 관찰되었다.

#' @ST4_
#' @Loess(Local.regression)_curve_visualization
windows()
scatterplot(income ~ education, data = Prestige,
            pch = 19, col = "black", cex = 0.7,
            regLine = list(method = lm, lty = 2, lwd = 2, col = "orangered"), 
            smooth = list(smooth = loessLine, spread = TRUE,
                          lty.smooth = 1, lwd.smooth = 3, col.smooth = "green"),
            xlab = "Education (years)", ylab = "income (dollors)",
            main = "Education and Income")

#' @ST5_ Polynomial regression 
#' +a)_ Formula symbol
#' @y ~ x + w + x:w
#' @y ~ x * y * z : y ~ x + w + z + x:w + x:z + w:z + x:w
#' @y ~ (x + w + z)^2 : y ~ x + w + z + x:w + x:z + w:z
#' @y ~. : y ~x + y + w + z
#' @y ~ (x + w + z)^2 - w:z : y ~ x + w + z + x:w + x:z
#' @y ~ x + I((Z + W)^2) : Y ~ X + U(U = (Z+W)^2)

Prestige.polynomial <- lm(income ~ education + I(education^2), data = Prestige)
summary(Prestige.polynomial)            
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -5951.4 -2091.1  -358.2  1762.4 18574.2 
# 
# Coefficients:
#    Estimate Std. Error t value Pr(>|t|)   
# (Intercept)    12918.23    5762.27   2.242  0.02720 * 
#'    education      -2102.90    1072.73  -1.960  @0.05277 . 
# I(education^2)   134.18      47.64   2.817  0.00586 **
#    ---
#    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
#' @Residual standard error: @3369 on 99 degrees of freedom
#' @Multiple R-squared:  @0.383,	Adjusted R-squared:  0.3706 
#' F-statistic: 30.73 on 2 and 99 DF,  @p-value: @4.146e-11 

# RSE, R-Squared 값은 절대적인 판단 기준이 될 수 없다.

#' @ST5_ 단순 회귀모델과 대조적으로 성능평가
summary(Prestige.lm)
# Residuals:
#     Min      1Q   Median     3Q     Max 
# -5493.2 -2433.8   -41.9  1491.5  17713.1 
# 
# Coefficients:
#    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -2853.6     1407.0  -2.028   0.0452 *  
#    education      898.8      127.0   7.075 2.08e-10 ***
#    ---
#    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
#'@Residual standard error: @3483 on 100 degrees of freedom
#'@Multiple R-squared:  @0.3336,	Adjusted @R-squared:  @0.3269 
# F-statistic: 50.06 on 1 and 100 DF,  p-value: 2.079e-10

#' @RSE : 3369, 3489
#' @R-Squared : 0.3336, 0.383


#'education      -2102.90    1072.73  -1.960  @0.05277 .
# I(education^2)   134.18      47.64   2.817   0.00586 **
#' @ST5_ Ply reggression에 이차항만을 추가할 경우 성능은 상대적으로 탁월하나, #' 다중공산성을 의심.
#' Model 성능 good_p-value: 4.146e-11, 다중공산성에대한 이유분석, 및 해결책 : arrange(data.frame(Prestige$education, fitted(Prestige.polynomial)), Prestige$education) 

plot(Prestige$income ~ Prestige$education,
     col = "black", pch = 19,
      xlab = "Education (years)", ylab = "income (dollors)",
      main = "Education and Income") 
lines(arrange(data.frame(Prestige$education, fitted(Prestige.polynomial)), 
              Prestige$education), col = "red")
