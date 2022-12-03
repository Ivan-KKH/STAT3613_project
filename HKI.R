library(ggplot2)

HKI <- read.csv(file = "HKI_sales.csv")

#sales <- c(HKI, HKI, KL, HKI)

HKI

norm_price = 8
norm_sales = 7


HKI$nprice<-HKI$price/norm_price
HKI$nsales<-HKI$sales/norm_sales

HKI


ggplot(data=HKI)+
  geom_line(mapping=aes(x=nsales,y=nprice,color="A"))+
  geom_point(mapping=aes(x=nsales,y=nprice),color="red")+
  labs(x="norm. price",y="norm. sales") +
  scale_color_manual(labels = c("A", "B"), values = c("red", "blue"))



fit_HKI <- nls(f=nsales~a * nprice^b,data=HKI, start = list(a = 1, b = -1.144))
summary(fit_HKI)
r2<-1-sum(residuals(fit_HKI)^2)/sum((fit_HKI$m$lhs()-mean(fit_HKI$m$lhs()))^2)
r2



ggplot(data=HKI)+
  geom_line(mapping=aes(x=nprice,y=predict(fit_HKI),color="A"))+
  geom_point(mapping=aes(x=nprice,y=nsales),color="red")


#Logistic model
a<-HKI$nsales[1] - HKI$nsales[7]
d<-HKI$nsales[7]
slope<-(HKI$nsales[3] - HKI$nsales[5])/(HKI$nprice[3] - HKI$nprice[5])
c<-4*slope/a
b<--c
fit2_HKI <- nls(nsales~a/(1+exp(-b-c*nprice))+d,data=HKI,start=list(a=a,b=b,c=c,d=d))
summary(fit2_HKI)
fit2_r2<-1-sum(residuals(fit2_HKI)^2)/sum((fit2_HKI$m$lhs()-mean(fit2_HKI$m$lhs()))^2)
fit2_r2

ggplot(data=HKI)+
  geom_line(mapping=aes(x=nprice,y=predict(fit2_HKI),color="A"))+
  geom_point(mapping=aes(x=nprice,y=nsales),color="red")


net<-function(x) {
  nd<-data.frame(nprice=x)
  nsales<-predict(fit_HKI,nd)
  # de-norm
  margin<- x * norm_price - 13.33 * nsales * norm_sales
  # de-norm
  sales <-nsales* norm_sales
  # de-norm
  price <- x * norm_price
  cost <- 13.33 * sales
  gross<-margin*sales
  net<-gross-cost
  print(cbind(x, nsales, margin, sales, price, cost, gross, net))
  
  
  
  #print(cbind(nsalesa,salesa,nsalesb,salesb,sfa,sfb,grossa,grossb,neta,netb,net))
  return(net)
}

net2<-function(x) {
  nd<-data.frame(nprice=x)
  nsales<-predict(fit_HKI,nd)
  # de-norm
  # revenue<- x * norm_price
  # de-norm
  sales <-nsales* norm_sales * 1000
  # de-norm
  price <- x * norm_price
  number<-ceiling(sales/60)
  cost <- 800*number
  revenue<-price*sales
  net<-revenue-cost
  print(cbind(x, nsales, sales, number, price, cost, revenue, net))
  
  #print(cbind(nsalesa,salesa,nsalesb,salesb,sfa,sfb,grossa,grossb,neta,netb,net))
  return(net)
}


net2(2.5)
op<-optimize(f=net2,interval=c(0,50), maximum= TRUE)

op
