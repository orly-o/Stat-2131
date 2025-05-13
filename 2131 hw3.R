# 2131 Homework 2 ####

setwd("C:/Users/orlyo/OneDrive/Desktop/Grad School/Fall 2020/STAT 2131 - Applied Statistical Methods 1/Homeworks")

library(dplyr)

# Question 1 (KNNL 3.14), refers to 1.22 data

data = read.csv("HW3/data122.csv")

  # (a)
qf(.99, df1 = 2, df2 = 12) # F-stat
model1 = lm(Y ~ X, data = data) # Ho
model2 = lm(Y ~ factor(X), data = data) # Ha
anova(model1, model2, data = data) # Test Ho that model can be represented linealry

# Question 2 (KNNL 3.18)

lot = read.csv("HW3/data318.csv")

  # (a)

plot(lot$X, lot$Y, xlab = "Production Lot Size", ylab = "Time in Hours", main = "(2a) Scatterplot")

  # (b)

lot$Xt = sqrt(lot$X) # create transfored data
model = lm(Y ~ Xt, data = lot) # linear model for Y on Xt
summary(model)

  # (c)

plot(lot$Xt, lot$Y, xlab = "Sq. Rt. of Production Lot Size", ylab = "Time in Hours", 
     main = "(2c) Scatterplot for Transformed Data", pch = 18, col = 3) # scatterplot of transformed data against Y
abline(model, col = 4, lwd = 4) # regression line from linear model

  # (d)

model.res = resid(model) # calculate residuals from the model of trasnformed data
plot(lot$Xt, model.res, ylab = "Residuals", xlab = "Sq. Rt. of Production Lot Size", main = "(2d) Residuals", 
     pch = 20, col = 2) # plot residuals with the transformed Xt data
abline(0, 0, lwd = 2) # horizontal line at 0 for comparison

model.stdres = rstandard(model) # calculate qqnorm of model
qqnorm(model.stdres, ylab = "Standardized Residuals", xlab = "Normal Scores", main = "(2d) QQ Norm Plot", 
       pch = 19, col = 4) # plot qqnorm of residuals to see if data is normally distributed
qqline(model.stdres, lwd = 2) # qqline for assessment

lot$predicted <- predict(model) # save predicted values
lot$residuals <- residuals(model) # save residuals
lot %>%
  select(residuals, predicted) %>% # select predicted and residual values
  head() # return top of selected columns

