install.packages("glmnet")
library(glmnet)

data(CoxExample)
x <- CoxExample$x
y <- CoxExample$y
y[1:5, ]
head(x)

fit <- glmnet(x, y, family = "cox") #lasso, default alpha=1

plot(fit)

coef(fit, s = 0.1) #s: Value(s) of the penalty parameter lambda at which predictions are required
print(fit) 
#Df: number of nonzero coefficients
#%Dev: the percent of null deviance explained
#Lambda: default: fits the model for 100 values of lambda

set.seed(1)
cvfit <- cv.glmnet(x, y, family = "cox", type.measure = "C")
plot(cvfit)

cvfit$lambda.min
cvfit$lambda.1se

coef(cvfit, s = "lambda.min")

predict(cvfit, newx = x[1:5,], s = "lambda.min")



?glmnet
?coef.glmnet

#plot survival curve


fit <- glmnet(x, y, family = "cox")
survival::survfit(fit, s = 0.05, x = x, y = y)

