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


#Constant elasticity price model
fit_ntw <- nls(f=nsales~a * nprice^b,data=NTW, start = list(a = 1, b = -1.66))
summary(fit_ntw)
r2<-1-sum(residuals(fit_ntw)^2)/sum((fit_ntw$m$lhs()-mean(fit_ntw$m$lhs()))^2)
r2
ggplot(data=NTW)+
  geom_line(mapping=aes(x=nprice,y=predict(fit_ntw),color="A"))+
  geom_point(mapping=aes(x=nprice,y=nsales),color="red")

#Logistic model
a<-NTW$nsales[1] - NTW$nsales[9]
d<-NTW$nsales[9]
slope<-(NTW$nsales[3] - NTW$nsales[5])/(NTW$nprice[3] - NTW$nprice[5])
c<-4*slope/a
b<--c
fit2_ntw <- nls(nsales~a/(1+exp(-b-c*nprice))+d,data=NTW,start=list(a=a,b=b,c=c,d=d))
summary(fit2_ntw)
fit2_r2<-1-sum(residuals(fit2_ntw)^2)/sum((fit2_ntw$m$lhs()-mean(fit2_ntw$m$lhs()))^2)
fit2_r2

ggplot(data=NTW)+
  geom_line(mapping=aes(x=nprice,y=predict(fit2_ntw),color="A"))+
  geom_point(mapping=aes(x=nprice,y=nsales),color="red")


net<-function(x) {
  nd<-data.frame(nprice=x)
  nsales<-predict(fit2_ntw,nd)
  # de-norm
  margin<- x * norm_price - 19
  # de-norm
  sales <-nsales* norm_sales
  # de-norm
  price <- x * norm_price
  cost <- 19 * sales
  net<-margin*sales
  print(cbind(x, nsales, margin, sales, price, cost, net))
  
  #print(cbind(nsalesa,salesa,nsalesb,salesb,sfa,sfb,grossa,grossb,neta,netb,net))
  return(net)
}

net2<-function(x) {
  nd<-data.frame(nprice=x)
  nsales<-predict(fit_ntw,nd)
  # de-norm
  # revenue<- x * norm_price
  # de-norm
  sales <-nsales* norm_sales * 1000
  # de-norm
  price <- x * norm_price
  number<-ceiling(sales/60)
  cost <- 1100*number
  revenue<-price*sales
  net<-revenue-cost
  print(cbind(x, nsales, sales, number, price, cost, revenue, net))
  
  #print(cbind(nsalesa,salesa,nsalesb,salesb,sfa,sfb,grossa,grossb,neta,netb,net))
  return(net)
}

net2(2.5)
op<-optimize(f=net2,interval=c(0,20), maximum= TRUE)

op

