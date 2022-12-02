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

net(2.5)
op<-optimize(f=net,interval=c(0,2.5), maximum= TRUE)

op
