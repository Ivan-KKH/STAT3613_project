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

net(2.5)
op<-optimize(f=net,interval=c(0,2.5), maximum= TRUE)

op
