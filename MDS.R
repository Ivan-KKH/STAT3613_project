library(cmdpref)

mdsdata<-read.csv("MDS.csv")
fit<-cmdpref(mdsdata[,2:62], ndim =2, monotone=F)
summary(fit)
plot(fit)
