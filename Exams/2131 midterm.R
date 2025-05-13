#### STAT 2131 MIDTERM ####
## Q4 ##

setwd("C:/Users/orlyo/OneDrive/Desktop/Grad School/Fall 2020/STAT 2131 - Applied Statistical Methods 1/Midterm")
load(file = "Genes.RData")
library(car)

View(Data)
View(Y)

# (a)

Y_1 = Y[,1]
conc.f = as.factor(Data$conc)
model1 = lm(Y_1 ~ conc.f)

# (b)

summary(model1)
confint(model1)

# (c)

confint(model1)

qqnorm(Y_1, pch = 1, frame = TRUE)
qqline(Y_1, col = "red", lwd = 2)
mtext("Q4 part c", side = 3)

# (d)

model2 = lm(Y_1 ~ conc.f + 0)
summary(model2)

# (e)

# model = lm(Y ~ conc.f + 0)
# summary(model)

Y_1 = Y[,1] ; Y_2 = Y[,2] ; Y_3 = Y[,3] ; Y_4 = Y[,4] ; Y_5 = Y[,5] ; 
Y_6 = Y[,6] ; Y_7 = Y[,7] ; Y_8 = Y[,8] ; Y_9 = Y[,9] ; Y_10 = Y[,10]

model_1 = lm(Y_1 ~ conc.f + 0); model_2 = lm(Y_2 ~ conc.f + 0); model_3 = lm(Y_3 ~ conc.f + 0)
model_4 = lm(Y_4 ~ conc.f + 0); model_5 = lm(Y_5 ~ conc.f + 0); model_6 = lm(Y_6 ~ conc.f + 0)
model_7 = lm(Y_7 ~ conc.f + 0); model_8 = lm(Y_8 ~ conc.f + 0); model_9 = lm(Y_9 ~ conc.f + 0)
model_10 = lm(Y_10 ~ conc.f + 0)

Regressionp <- function (x) {
  if (class(x) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(x)$fstatistic
  p <- pf(f[1], f[2], f[3], lower.tail = F)
  attributes(p) <- NULL
  return(p)
}

p = c(Regressionp(model_1), Regressionp(model_2), Regressionp(model_3), Regressionp(model_4), 
      Regressionp(model_5), Regressionp(model_6), Regressionp(model_7), Regressionp(model_8), 
      Regressionp(model_9), Regressionp(model_10))
p

p.ad = p.adjust(p, method = "bonferroni")
p.ad > .05/4
