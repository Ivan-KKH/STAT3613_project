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


#Constant elasticity price model
fit_nte <- nls(f=nsales~a * nprice^b,data=NTE, start = list(a = 1, b = -0.308))
summary(fit_nte)
r2<-1-sum(residuals(fit_nte)^2)/sum((fit_nte$m$lhs()-mean(fit_nte$m$lhs()))^2)
r2

ggplot(data=NTE)+
  geom_line(mapping=aes(x=nprice,y=predict(fit_NTE),color="A"))+
  geom_point(mapping=aes(x=nprice,y=nsales),color="red")

#Logistic model
a<-NTE$nsales[1] - NTE$nsales[10]
d<-NTE$nsales[10]
slope<-(NTE$nsales[4] - NTE$nsales[6])/(NTE$nprice[4] - NTE$nprice[6])
c<-4*slope/a
b<--c
fit2_nte <- nls(nsales~a/(1+exp(-b-c*nprice))+d,data=NTE,start=list(a=a,b=b,c=c,d=d))
summary(fit2_nte)
fit2_r2<-1-sum(residuals(fit2_nte)^2)/sum((fit2_nte$m$lhs()-mean(fit2_nte$m$lhs()))^2)
fit2_r2

ggplot(data=NTE)+
  geom_line(mapping=aes(x=nprice,y=predict(fit2_nte),color="A"))+
  geom_point(mapping=aes(x=nprice,y=nsales),color="red")


net<-function(x) {
  nd<-data.frame(nprice=x)
  nsales<-predict(fit_nte,nd)
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

net2<-function(x) {
  nd<-data.frame(nprice=x)
  nsales<-predict(fit2_nte,nd)
  # de-norm
  # revenue<- x * norm_price
  # de-norm
  sales <-nsales* norm_sales * 1000
  # de-norm
  price <- x * norm_price
  number<-ceiling(sales/60)
  cost <- 950*number
  revenue<-price*sales
  net<-revenue-cost
  print(cbind(x, nsales, sales, number, price, cost, revenue, net))
  
  #print(cbind(nsalesa,salesa,nsalesb,salesb,sfa,sfb,grossa,grossb,neta,netb,net))
  return(net)
}


net2(2.5)
op<-optimize(f=net2,interval=c(0,20), maximum= TRUE)

op
