# Correspondence Analysis

#install.packages("ca")
library(ca)

CA<-read.csv("CA.csv", header = TRUE,row.names = 1)
CA

#obj= contingency table
fit<-ca(obj=CA)

#summary() does not work when 
#   nd= smaller than max. number of dimensions
summary(fit)
#dim= dimensions to be printed
cacoord(fit,type="principal",dim=1:2)

#par(mar=c(4,4,1,4))
plot(fit)
