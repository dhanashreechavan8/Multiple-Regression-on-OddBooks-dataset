install.packages("ggcorrplot")
library(ggcorrplot)

#Validate the relationship between variables using subset regression
install.packages("leaps")
library(leaps)
library(car)

#oddbooks dataset analysis
installed.packages("DAAG")
installed.packages("olsrr")


library(DAAG) # oddbooks
library(olsrr)

oddbooks

data(oddbooks) 

#Before you apply linear regression models, you’ll need to verify that several assumptions are met. Most notably, you’ll need to make sure that a linear relationship exists between the dependent variable and the independent variable/s.

ggcorrplot(cor(oddbooks), hc.order = TRUE,method = "circle")

cor(oddbooks)

#stepAIC Backward selection (mass)

starting.model <- lm(weight ~ thick + height + breadth, data = oddbooks)
simple.model <- lm(weight ~ 1, data = oddbooks)
back_select=stepAIC(starting.model, scope = list(upper = starting.model, lower = simple.model), direction = "backward")
summary(back_select)
back_select

#sub-set selection

fit <- lm(weight ~ thick + height + breadth, data = oddbooks)
fitcompare<-ols_step_best_subset(fit)
summary(fitcompare)

#as AIC, SBIC and SBC are lowest for the first model i.e. the first model, we will select 1st model as it has best subset of predictor even though Adj. R-square is little larger than model 3


#Using BIC algorithm:
attach(oddbooks)
BIC(lm(weight~1)) #170.2134
BIC(lm(weight~height)) #151.8114
BIC(lm(weight~thick)) #160.9688
BIC(lm(weight~breadth)) #146.2811
BIC(lm(weight~thick+height)) #151.7305
BIC(lm(weight~thick+breadth)) #146.7772
BIC(lm(weight~height+breadth)) #146.7816
BIC(lm(weight~thick+height+breadth)) #148.9239
detach(oddbooks)

#as least score is for variable breath hence we will choose that variable

#final model with one independant variable
logoddbooks<-log(oddbooks)
fit_final <- lm(weight ~ breadth, data = logoddbooks)
summary(fit_final)
plot(logoddbooks$breadth, logoddbooks$weight, xlab='Breadth', ylab='Weight', main ='Breadth Vs Weight')
abline(fit_final, col='green')


