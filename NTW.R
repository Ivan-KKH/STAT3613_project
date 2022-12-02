library(ggplot2)

NTW <- read.csv(file = "NTW_sales.csv")

#sales <- c(NTE, NTW, KL, HKI)


NTW

norm_price = 20
norm_sales = 6


NTW$nprice<-NTW$price/norm_price
NTW$nsales<-NTW$sales/norm_sales

NTW


ggplot(data=NTW)+
  geom_line(mapping=aes(x=nsales,y=nprice,color="A"))+
  geom_point(mapping=aes(x=nsales,y=nprice),color="red")+
  labs(x="norm. price",y="norm. sales") +
  scale_color_manual(labels = c("A", "B"), values = c("red", "blue"))



fit_ntw <- nls(f=nsales~a * nprice^b,data=NTW, start = list(a = 1, b = -1.66))
summary(fit_ntw)
r2<-1-sum(residuals(fit_ntw)^2)/sum((fit_ntw$m$lhs()-mean(fit_ntw$m$lhs()))^2)
r2



ggplot(data=NTW)+
  geom_line(mapping=aes(x=nprice,y=predict(fit_ntw),color="A"))+
  geom_point(mapping=aes(x=nprice,y=nsales),color="red")


net<-function(x) {
  nd<-data.frame(nprice=x)
  nsales<-predict(fit_ntw,nd)
  # de-norm
  margin<- x * norm_price - 18.33 * nsales * norm_sales
  # de-norm
  sales <-nsales* norm_sales
  # de-norm
  price <- x * norm_price
  cost <- 18.33 * sales
  gross<-margin*sales
  net<-gross-cost
  print(cbind(x, nsales, margin, sales, price, cost, gross, net))
  
  
  
  #print(cbind(nsalesa,salesa,nsalesb,salesb,sfa,sfb,grossa,grossb,neta,netb,net))
  return(net)
}

net(2.5)
op<-optimize(f=net,interval=c(0,2.5), maximum= TRUE)

op
