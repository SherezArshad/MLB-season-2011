#Name:Daniyal Arshad


download.file("http://www.openintro.org/stat/data/mlb11.RData", destfile = "mlb11.RData")
load("mlb11.RData")


#1
plot(mlb11$runs ~ mlb11$at_bats)
#There is no linear association between the two, the points are spread throughout the plot. 


#2
cor(mlb11$runs, mlb11$at_bats)
#0.610627
# The correlation variable is 0.610, its slightly postive. 

#3
plot_ss(mlb11$at_bats, mlb11$runs)
# Sum of Squares:  130497.7
#Call:
#lm(formula = y ~ x, data = pts)

#Coefficients:
#  (Intercept)            x  
#-3575.6855       0.7711  

#4
lm1 = lm(mlb11$runs ~ mlb11$at_bats, data = mlb11)
lm1
# y=0.6305x-2789.2429

#Call:
#lm(formula = mlb11$runs ~ mlb11$at_bats, data = mlb11)

#Coefficients:
#  (Intercept)  mlb11$at_bats  
#-2789.2429         0.6305 


#5
summary(lm1)
#Call
#lm(formula = mlb11$runs ~ mlb11$at_bats, data = mlb11)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-125.58  -47.05  -16.59   54.40  176.87 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   -2789.2429   853.6957  -3.267 0.002871 ** 
#  mlb11$at_bats     0.6305     0.1545   4.080 0.000339 ***
#  ---
#  Signif. codes:  
#  0 *** 0.001 ** 0.01 * 0.05 . 0.1   1

#Residual standard error: 66.47 on 28 degrees of freedom
#Multiple R-squared:  0.3729,	Adjusted R-squared:  0.3505 
#F-statistic: 16.65 on 1 and 28 DF,  p-value: 0.0003388



#6
plot(lm1)
# The graph is close to linear due to it being a horizontal line. 
# There is normal distribution, while the constant variance is close to normal. 


#7
plot(mlb11$runs ~ mlb11$homeruns, data = mlb11)
lm2 =lm(mlb11$runs ~ mlb11$homeruns, data = mlb11)
#The regression line equation is y = 1.835x+415.239



#8
summary(lm2)

#homerun is a better predictor of runs. 

#Call:
#  lm(formula = mlb11$runs ~ mlb11$homeruns, data = mlb11)

# Residuals:
#  Min      1Q  Median      3Q     Max 
# -91.615 -33.410   3.231  24.292 104.631 

# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    415.2389    41.6779   9.963 1.04e-10 ***
#  mlb11$homeruns   1.8345     0.2677   6.854 1.90e-07 ***
#  ---
#  Signif. codes:  0 *** 0.001 ** 0.01 * 0.05 . 0.1  1

# Residual standard error: 51.29 on 28 degrees of freedom
# Multiple R-squared:  0.6266,	Adjusted R-squared:  0.6132 
# F-statistic: 46.98 on 1 and 28 DF,  p-value: 1.9e-07



#9
lm3 = lm(mlb11$runs ~ mlb11$hits, data = mlb11)
summary(lm3)


lm4 = lm(mlb11$runs ~ mlb11$bat_avg, data = mlb11)
summary(lm4)


lm5=lm(mlb11$runs ~ mlb11$strikeouts, data = mlb11)
summary(lm5)


lm6=lm(mlb11$runs ~ mlb11$stolen_bases, data = mlb11)
summary(lm6)


lm7=lm(mlb11$runs ~ mlb11$wins, data = mlb11)
summary(lm7)

# Batting average is best fit with runs. 

#10
regression_model = lm(runs~at_bats+hits+homeruns+bat_avg+strikeouts+stolen_bases+wins,data=mlb11)
summary(regression_model)

#Call:
#  lm(formula = runs ~ at_bats + hits + homeruns + bat_avg + strikeouts + 
#       stolen_bases + wins, data = mlb11)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-34.273 -17.965   2.141  20.011  40.257 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   2.025e+03  3.750e+03   0.540 0.594549    
#at_bats      -4.764e-01  6.679e-01  -0.713 0.483159    
#hits          2.047e+00  2.599e+00   0.787 0.439522    
#homeruns      1.030e+00  2.220e-01   4.639 0.000127 ***
#  bat_avg      -7.568e+03  1.458e+04  -0.519 0.608816    
#strikeouts    4.780e-02  6.733e-02   0.710 0.485216    
#stolen_bases  5.207e-01  1.705e-01   3.053 0.005825 ** 
#  wins          9.586e-01  6.783e-01   1.413 0.171559    
#---
#  Signif. codes:  0 *** 0.001 ** 0.01 * 0.05 . 0.1   1

# Residual standard error: 27.08 on 22 degrees of freedom
# Multiple R-squared:  0.9182,	Adjusted R-squared:  0.8922 
# F-statistic:  35.3 on 7 and 22 DF,  p-value: 1.562e-10

# Explains variance. The R squared value is now 92%. 
# Takes in many factors rather than just one. 