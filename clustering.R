# 1: NTE, 2: NTW 3:KL 4: HK Island

library(psych)
library(ggplot2)
library(reshape2)
data <- read.csv("data.csv")



describe(data,skew=F,ranges=F)

data$Gender <- as.factor(data$Gender)
data$Year.of.Study <- as.factor(data$Year.of.Study)
data$Living.District <- as.factor(data$Living.District)
data$Transportation <- as.factor(data$Transportation)
#data$Monthly.Expense <- as.factor(data$Monthly.Expense)
#data$Num_transfer<- as.factor(data$Num_transfer)


h1<-cbind(
  model.matrix( ~ Num_transfer - 1, data),
  model.matrix( ~ Living.District - 1, data),
  model.matrix( ~ Transportation - 1, data), data$Monthly.Expense, data$Acceptable.price)


#exp = scale(subset(data, select = c(4, 7)))
h1 <- scale(h1)
dist<-dist(h1,method="euclidean")^2

fit <- hclust(dist, method="ward.D")
history<-cbind(fit$merge,fit$height)
history

ggplot(mapping=aes(x=1:length(fit$height),y=fit$height))+
  geom_line()+
  geom_point()+
  labs(x="stage",y="height")


plot(fit,labels=data$X,hang=-1,main="",axes=FALSE)
axis(side = 2, at = seq(0, 70))

cluster <- cutree(fit, k=6) #cluster index
sol <- data.frame(cluster=cutree(fit, k=6),id=h1)
sol[ order(sol$cluster),1:10 ]
# 1: NTE, 2: NTW 3:KL 4: HK Island
colnames(sol) <- c('cluster', 'Num_transfer', 'NT-E', 'NT-W', 'KL', 'HK ISLAND', 'MTR', 'NON-MTR', 'Monthly Exp', 'Acceptable pri')

table(sol[,1])


tb<-aggregate(x=sol[,], by=list(cluster=sol$cluster),FUN=mean)
print(tb,digits=2)


tbm<-melt(tb,id.vars='cluster')
tbm$cluster<-factor(tbm$cluster)
ggplot(tbm, 
       aes(x = variable, y = value, group = cluster, colour = cluster)) + 
  geom_line(aes(linetype=cluster))+
  geom_point(aes(shape=cluster)) +
  geom_hline(yintercept=0) +
  labs(x=NULL,y="mean")


## continue K-mean clustering based on centre pt with ward
## similar to Asm2 q3
clustered_tbm <- data.frame(cbind(h1, cluster))

cluster_1 <- clustered_tbm[clustered_tbm$cluster == 1, ][, 1:10]
cluster_2 <- clustered_tbm[clustered_tbm$cluster == 2, ][, 1:10]
cluster_3 <- clustered_tbm[clustered_tbm$cluster == 3, ][, 1:10]
cluster_4 <- clustered_tbm[clustered_tbm$cluster == 4, ][, 1:10]
cluster_5 <- clustered_tbm[clustered_tbm$cluster == 5, ][, 1:10]
cluster_6 <- clustered_tbm[clustered_tbm$cluster == 6, ][, 1:10]

centroids <- aggregate( .~cluster, data=clustered_tbm, mean)[, 2:10]

centroids
fit1<-kmeans(x=h1,centers=centroids)
fit1
centers<-fit1$centers
size <- fit1$size
centers
tb<-data.frame(cbind(centers,cluster=1: 6))
tb
colnames(tb) <- c('Num_transfer', 'NT-E', 'NT-W', 'KL', 'HK ISLAND', 'MTR', 'NON-MTR', 'Monthly Exp', 'Acceptable pri', 'cluster')
tbm<-melt(tb,id.vars='cluster')
tbm$cluster<-factor(tbm$cluster)
ggplot(tbm, 
       aes(x = variable, y = value, group = cluster, colour = cluster)) + 
  geom_line(aes(linetype=cluster))+
  geom_point(aes(shape=cluster)) +
  geom_hline(yintercept=0) +
  labs(x=NULL,y="mean")

