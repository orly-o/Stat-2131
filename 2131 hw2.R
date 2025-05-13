# 2131 HW2 Code ####

# Q1

data = read.csv("data122.csv")

  # (a)

model = lm(Y ~ X, data = data)
summary(model)

plot(Y ~ X, data = data)
abline(model, col = 4)

  # (b)
new.data = data.frame(X = 30)

predict(model, newdata = new.data, interval = "confidence", level = .98)
predict(model, newdata = new.data, interval = "prediction", level = .98)

  # (e)

qf(.98, df1 = 2, df2 = 14)

# Q5

  # (b)/(c)

qt(.025, df = 22)
