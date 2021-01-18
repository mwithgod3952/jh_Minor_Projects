library(car)
str(Prestige)
head(Prestige)

Prestige.lm <- lm(income ~ education, data = Prestige)
summary(Prestige.lm)

#' @Coefficients:
#               Estimate Std.    Error t     value Pr(>|t|)    
# (Intercept)        -2853.6     1407.0     -2.028   0.0452 *  
# education            898.8      127.0      7.075 2.08e-10 ***
# Intercept : -2853.6 / Slope : 898.8

windows(width=12, height = 8)
plot(Prestige$income ~ Prestige$education,
     col = "#377EB8", pch = 19,
     xlab = "Education (years)", ylab = "income (dollors)",
     main = "Education and Income")
abline(Prestige.lm, col = "salmon", lwd = 2)

lm(income ~ education, data = Prestige,
   subset = (education > mean(Prestige$education)))
# Coefficients:
#   (Intercept)    education  
#       -10299          1455
#' @increace_per_455$
 
lm(income ~ education, data = Prestige,
   subset = (education <= mean(Prestige$education)))
# Coefficients:
#   (Intercept)    education  
#       2546.6         281.8
#' @increace_per_281.8$

#' @Loess_curve
windows()
scatterplot(income ~ education, data = Prestige,
            pch = 19, col = "black", cex = 0.7,
            regLine = list(method = lm, lty = 2, lwd = 2, col = "orangered"), 
            smooth = list(smooth = loessLine, spread = FALSE,
                          lyu.smooth = 1, lwd.smooth = 3, col.smooth = "green"),
            xlab = "Education (years)", ylab = "income (dollors)",
            main = "Education and Income")

Prestige.polynomial <- lm(income ~ education + I(education^2), data = Prestige)
summary(Prestige.polynomial)            
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -5951.4 -2091.1  -358.2  1762.4 18574.2 
# 
# Coefficients:
#    Estimate Std. Error t value Pr(>|t|)   
# (Intercept)    12918.23    5762.27   2.242  0.02720 * 
#    education      -2102.90    1072.73  -1.960  0.05277 . 
# I(education^2)   134.18      47.64   2.817  0.00586 **
#    ---
#    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
#' @Residual standard error: @3369 on 99 degrees of freedom
#' @Multiple R-squared:  @0.383,	Adjusted @R-squared:  @0.3706 
# F-statistic: 30.73 on 2 and 99 DF,  p-value: 4.146e-11

summary(Prestige.lm)
# Residuals:
#     Min      1Q   Median     3Q     Max 
# -5493.2 -2433.8   -41.9  1491.5 17713.1 
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




