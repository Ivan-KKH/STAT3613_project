library(ggplot2)

KL <- read.csv(file = "KL_sales.csv")

#sales <- c(KL, KL, KL, HKI)

KL

norm_price = 10
norm_sales = 9


KL$nprice<-KL$price/norm_price
KL$nsales<-KL$sales/norm_sales

KL


ggplot(data=KL)+
  geom_line(mapping=aes(x=nsales,y=nprice,color="A"))+
  geom_point(mapping=aes(x=nsales,y=nprice),color="red")+
  labs(x="norm. price",y="norm. sales") +
  scale_color_manual(labels = c("A", "B"), values = c("red", "blue"))



fit_KL <- nls(f=nsales~a * nprice^b,data=KL, start = list(a = 1, b = -2.222))
summary(fit_KL)
r2<-1-sum(residuals(fit_KL)^2)/sum((fit_KL$m$lhs()-mean(fit_KL$m$lhs()))^2)
r2



ggplot(data=KL)+
  geom_line(mapping=aes(x=nprice,y=predict(fit_KL),color="A"))+
  geom_point(mapping=aes(x=nprice,y=nsales),color="red")


#Logistic model
a<-KL$nsales[1] - KL$nsales[10]
d<-KL$nsales[10]
slope<-(KL$nsales[4] - KL$nsales[6])/(KL$nprice[4] - KL$nprice[6])
c<-4*slope/a
b<--c
fit2_KL <- nls(nsales~a/(1+exp(-b-c*nprice))+d,data=KL,start=list(a=a,b=b,c=c,d=d))
summary(fit2_KL)
fit2_r2<-1-sum(residuals(fit2_KL)^2)/sum((fit2_KL$m$lhs()-mean(fit2_KL$m$lhs()))^2)
fit2_r2

ggplot(data=KL)+
  geom_line(mapping=aes(x=nprice,y=predict(fit2_KL),color="A"))+
  geom_point(mapping=aes(x=nprice,y=nsales),color="red")


net<-function(x) {
  nd<-data.frame(nprice=x)
  nsales<-predict(fit_KL,nd)
  # de-norm
  margin<- x * norm_price - 14.16 * nsales * norm_sales
  # de-norm
  sales <-nsales* norm_sales
  # de-norm
  price <- x * norm_price
  cost <- 14.16 * sales
  gross<-margin*sales
  net<-gross-cost
  print(cbind(x, nsales, margin, sales, price, cost, gross, net))
  
  
  
  #print(cbind(nsalesa,salesa,nsalesb,salesb,sfa,sfb,grossa,grossb,neta,netb,net))
  return(net)
}

net2<-function(x) {
  nd<-data.frame(nprice=x)
  nsales<-predict(fit2_KL,nd)
  # de-norm
  # revenue<- x * norm_price
  # de-norm
  sales <-nsales* norm_sales * 100
  # de-norm
  price <- x * norm_price
  number<-ceiling(sales/60)
  cost <- 850*number
  revenue<-price*sales
  net<-revenue-cost
  print(cbind(x, nsales, sales, number, price, cost, revenue, net))
  
  #print(cbind(nsalesa,salesa,nsalesb,salesb,sfa,sfb,grossa,grossb,neta,netb,net))
  return(net)
}

net2(2.5)
op<-optimize(f=net2,interval=c(0,20), maximum= TRUE)

op
