library(cmdpref)


mdsdata<-read.csv("MDS.csv")
mdsfit<-cmdpref(mdsdata[,2:62], ndim =2, monotone=F)
summary(mdsfit)
plot(mdsfit)


#cluster from CA
cluster

subject<-data.frame(cbind(mdsfit$corr, cluster, district = data$Living.District))
subject$cluster<-factor(subject$cluster)
subject$district<-factor(subject$district)

ggplot(subject, 
       aes(x = dim1, y = dim2, group = cluster, colour = cluster)) + 
  geom_point() +
  ylim(-1, 1) +
  xlim(-1, 0.5)
