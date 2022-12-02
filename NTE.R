library(ggplot2)

NTE <- read.csv(file = "NTE_sales.csv")

#sales <- c(NTE, NTE, KL, HKI)

NTE

norm_price = 10
norm_sales = 8


NTE$nprice<-NTE$price/norm_price
NTE$nsales<-NTE$sales/norm_sales

NTE


ggplot(data=NTE)+
  geom_line(mapping=aes(x=nsales,y=nprice,color="A"))+
  geom_point(mapping=aes(x=nsales,y=nprice),color="red")+
  labs(x="norm. price",y="norm. sales") +
  scale_color_manual(labels = c("A", "B"), values = c("red", "blue"))



fit_NTE <- nls(f=nsales~a * nprice^b,data=NTE, start = list(a = 1, b = -0.308))
summary(fit_NTE)
r2<-1-sum(residuals(fit_NTE)^2)/sum((fit_NTE$m$lhs()-mean(fit_NTE$m$lhs()))^2)
r2



ggplot(data=NTE)+
  geom_line(mapping=aes(x=nprice,y=predict(fit_NTE),color="A"))+
  geom_point(mapping=aes(x=nprice,y=nsales),color="red")


net<-function(x) {
  nd<-data.frame(nprice=x)
  nsales<-predict(fit_NTE,nd)
  # de-norm
  margin<- x * norm_price - 15.33 * nsales * norm_sales
  # de-norm
  sales <-nsales* norm_sales
  # de-norm
  price <- x * norm_price
  cost <- 15.33 * sales
  gross<-margin*sales
  net<-gross-cost
  print(cbind(x, nsales, margin, sales, price, cost, gross, net))
  
  
  
  #print(cbind(nsalesa,salesa,nsalesb,salesb,sfa,sfb,grossa,grossb,neta,netb,net))
  return(net)
}

net(2.5)
op<-optimize(f=net,interval=c(0,2.5), maximum= TRUE)

op
