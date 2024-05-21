rm(list=ls())

#install.packages("cmprsk")
library(survival)
library(cmprsk)
library(KMsurv)

data(bmt)
attach(bmt)
#?bmt

table(d1)
table(d2)
table(d3)

table(d1,d2)

d.all<-ifelse(d1==1,2,0)
d.all[d2==1]<-1

table(d.all)

#?cuminc
r<-cuminc(t2,d.all)
print(r)
plot(r,lty=1,color=1:2,xlab="days")

r<-cuminc(t2,d.all,z3)
print(r)
plot(r,lty=1,color=1:4,xlab="days")

?crr #Competing Risks Regression
