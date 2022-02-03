#Question 1
DATA=read.table('https://raw.githubusercontent.com/gdlc/STAT_COMP/master/DATA/goutData.txt', header = T)
#Fitted a model using R inbuillt function lm()
model=lm(su~race+sex+age, data = DATA)
summary(model)

X=model.matrix(su~sex+race+age, data=DATA)
y=DATA$su
s=myOLS(X, y)



"""Call:
lm(formula = su ~ race + sex + age, data = DATA)

Residuals:
    Min      1Q  Median      3Q     Max 
-4.4843 -0.9717 -0.1829  0.8276  5.4296 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  4.31975    0.81533   5.298 1.95e-07 ***
raceW       -0.78212    0.16932  -4.619 5.22e-06 ***
sexM         1.52853    0.14306  10.684  < 2e-16 ***
age          0.02674    0.01299   2.058   0.0402 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.413 on 396 degrees of freedom
Multiple R-squared:  0.2504,	Adjusted R-squared:  0.2447 
F-statistic: 44.09 on 3 and 396 DF,  p-value: < 2.2e-16

In the above fitted model we can see if the distribution of data is symmentric, in the residuals section we can see 
the data is skeweved on the right. The next section of output is Coefficients, the formula for our model is 
                Y=


"""



#question 2
model1=lm(su~race+sex+age+(race*sex), data = DATA)
"""
Residuals are essentially the difference between the actual observed response values 
(distance to stop dist in our case) and the response values that the model predicted
the linear regression equation will be Y=Intercept+(-0.9355)*raceW+(1.2119)*sexM+age+(0.40430)*(raceW*sexM)
If the P-value is less than 0.05 the it is statistically significant, hence we reject the NULL hypothesis
boxplot(model[['residuals']],main='Boxplot: Residuals',ylab='residual value') this cmd explains if our data is symentric or not
t-statistics which tells us about how far our estimated parameter is from a hypothesized 0 value, scaled by the standard deviation of the estimate.

Call:
lm(formula = su ~ (race * sex) + race + sex + age, data = DATA)

Residuals:
    Min      1Q  Median      3Q     Max 
-4.5293 -0.9190 -0.1923  0.8184  5.3810 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  4.46104    0.82394   5.414 1.07e-07 ***
raceW       -0.93555    0.21447  -4.362 1.65e-05 ***
sexM         1.21196    0.30712   3.946 9.40e-05 ***
age          0.02629    0.01299   2.024   0.0437 *  
raceW:sexM   0.40430    0.34712   1.165   0.2448    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.413 on 395 degrees of freedom
Multiple R-squared:  0.253,	Adjusted R-squared:  0.2454 
F-statistic: 33.44 on 4 and 395 DF,  p-value: < 2.2e-16
"""

#Question 3
# The hypothesis that we are interested to test is
# H0:Sex has no effect on su level for a person to get Gout
#HA:Sex has an effect on su level for a person to get Gout

H0=lm(su~race+age, data = DATA)
Ha=lm(su~sex+race+age, data = DATA)
summary(H0)
"""
Call:
lm(formula = su ~ race + age, data = DATA)

Residuals:
    Min      1Q  Median      3Q     Max 
-3.6817 -1.0790 -0.0424  0.9440  6.2661 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  4.60616    0.92375   4.986 9.21e-07 ***
raceW       -0.63105    0.19127  -3.299  0.00106 ** 
age          0.03098    0.01472   2.105  0.03593 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.602 on 397 degrees of freedom
Multiple R-squared:  0.03429,	Adjusted R-squared:  0.02943 
F-statistic: 7.049 on 2 and 397 DF,  p-value: 0.0009816
"""
summary(Ha)
"""
Call:
lm(formula = su ~ sex + race + age, data = DATA)

Residuals:
    Min      1Q  Median      3Q     Max 
-4.4843 -0.9717 -0.1829  0.8276  5.4296 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  4.31975    0.81533   5.298 1.95e-07 ***
sexM         1.52853    0.14306  10.684  < 2e-16 ***
raceW       -0.78212    0.16932  -4.619 5.22e-06 ***
age          0.02674    0.01299   2.058   0.0402 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.413 on 396 degrees of freedom
Multiple R-squared:  0.2504,	Adjusted R-squared:  0.2447 
F-statistic: 44.09 on 3 and 396 DF,  p-value: < 2.2e-16
"""
anova(H0, Ha)

"""
Analysis of Variance Table

Model 1: su ~ race + age
Model 2: su ~ sex + race + age
  Res.Df     RSS Df Sum of Sq      F    Pr(>F)    
1    397 1019.11                                  
2    396  791.06  1    228.04 114.16 < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
"""

#Question 4
#In  this question I implemented a function myAvova, which take two lm() object 
#and produces a Variance table for the hypothesis in Question 3
#Anova test is a comparison of variances 
myAnova(H0, Ha)
myAnova<-function(H0, HA){
  n=dim(DATA)[1]
  RSSO=sum(residuals(H0)^2)
  RSSA=sum(residuals(Ha)^2)
  MSS=RSSO-RSSA
  df1=length(coef(Ha))-length(coef(H0))
  df2=n-length(coef(Ha))
  Fstat=(MSS/(df1))/(RSSA/(df2))
  pValue=pf(Fstat, lower.tail = F, df1=df1, df2=df2)
  print(Fstat)
}

#Question 5
Wald_Test<-function(Ha, L){
  Wald_Test=numeric(3)
  r=dim(L)[1]
  b=coef(Ha)
  Vn=vcov(Ha)
  W = t(L%*%b-h) %*% solve(L%*%Vn%*%t(L)) %*%(L%*%b-h)
  W = as.numeric(W)
  pval=1- pchisq(W, r)
  Wald_Test[1]=W; Wald_Test[2]=r ; Wald_Test=pval
  print(pval)
  print(W)
  print(r)
  
}
L=L <- matrix(c(0, 1, 0, 0), nrow = 1, byrow = TRUE)

print(Wald_Test(Ha, L))

Wald_rtest=wald.test(b = coef(Ha), varb = vcov(Ha), L = L)







