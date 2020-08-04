setwd("C:/Users/tchas/OneDrive/Dokumente/Analyse/data")
getwd()
dataset<-read.csv(file="NAICExpense.csv",header=TRUE)
dataset
View(dataset)
head(dataset)
dim(dataset)

dataset1<-dataset[ ,-c(1:4)]
dataset1<-na.omit(dataset1)
dataset1
dataset.scaled= scale(dataset1)
dim(dataset.scaled)
dm <- dist(dataset.scaled)
#round (dm,4)

help(hclust)
library(cluster)

cs<-hclust(dm, method = "complete")
plot(cs)


ca<-hclust(dm, method = "average")
plot(ca)


ncc<-agnes(dm, method = "complete")
plot(ncc)
clusplot(ncc,labels=3,col.p=ncc$clustering)

plot(cz <- hclust(dm, method = "centroid"))


plot(cs)
abline(h = 3.5, col = "lightgrey")
lab <- cutree(cs, h = 3.6)
lab

 for(i in 2:8)
 {
 clu<-pam(dm,k=i,diss=FALSE,stand=FALSE);
 print(i);
  print(clu$silinfo[3]);
 }

library(cluster)
library(fpc)
plotcluster(dm,clus$cluster)
clusplot(dm,clus$cluster,color=TRUE,schade=TRUE,labels=2,lines=0)

#################################################
####Kmea

library(MVA)
library(lattice)
km<-pam(dm, k=3)
km$clustering
km$silinfo
#clus<-kmeans(dm,centers=3 nstart=100)
#clus

#install.packages("factoextra")
#########################################regression############################

dataset1
cor(dataset1)

model<-lm(EXPENSES~RBC+STAFFWAGE+AGENTWAGE+LONGLOSS+SHORTLOSS+GPWPERSONAL+GPWCOMM+ASSETS+CASH+LIQUIDRATIO, data = dataset1)
print(model)
summary(model)
plot(model)
plot(EXPENSES,LONGLOSS)



