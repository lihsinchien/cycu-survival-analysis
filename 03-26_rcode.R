##########################
## Section 4.6: left truncation
##########################

library(survival)

## left truncation

l<-c(4,0,5,1,2,1)-.1
t<-c(5,4,7,2,8,5)
d<-rep(1,6)
(my.surv<-Surv(l,t,d))
r<-survfit(my.surv~1)
summary(r)

## left truncation + right censored
l<-c(4,0,5,1,2,1)-.1
t<-c(5,1,7,2,4,5)
d<-c(1,0,1,1,0,1)

my.surv.object<-Surv(l,t,d)
my.surv.object

my.fit<-survfit(my.surv.object~1)
summary(my.fit)



################################################
## Section 5.4: Example 5.4 cohort life table
################################################

library(KMsurv)

data<-read.csv(file="table5-6.csv")
data

y<-data$Y0-data$c/2
data2<-data.frame(data,"Y"=y)
data2

S_1<-c(1,cumprod(1-data$weaned/y)[-10])
(data3<-data.frame(data,"Y"=y,"S1"=round(S_1,digits=4)))

int<-c(2,1,2,2,4,6,8,12,16,NA)

h_m<-data$weaned/(int*(y-data$weaned/2))
(data4<-data.frame(data,"Y"=y,"S1"=round(S_1,digits=4),"hm"=round(h_m,digits=4)))

################
library(KMsurv)
data(bmt)
head(bmt)
data.all<-bmt[bmt$group==1,]
dim(data.all)

d<-data.all$d3
t<-data.all$t2

r0<-survfit(Surv(t,d)~1)
r1 <- survfit(Surv(t,d)~1, conf.type="plain")
r2 <- survfit(Surv(t,d)~1, conf.type="log")
r3 <- survfit(Surv(t,d)~1, conf.type="log-log")
r4 <- survfit(Surv(t,d)~1, conf.type="logit")
r5 <- survfit(Surv(t,d)~1, conf.type="arcsin")
rbind(summary(r0,time=365),
summary(r1,time=365),
summary(r2,time=365),
summary(r3,time=365),
summary(r4,time=365),
summary(r5,time=365))
