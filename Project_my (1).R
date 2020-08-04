
#by Tchassem Ngounou eric
#install
install.packages("StatDA")
library(StatDA) 
library(sgeostat)

#data loading
data(moss) 
data(ohorizon)
data(chorizon)

#1: Exploratory and graphical data analysis
attach(ohorizon)
summary(ohorizon)
cor(ohorizon)
hist(ohorizon$ca, freq = False)
ohorizonnew<-ohorizon[-c(5,6,10:12)] #non numeric varaiables
ohorizonNew<-na.omit(ohorizonnew) #removing na columns
data<-scale(ohorizonNew) #scaled data
data
hist(data)
cor(data)#bivariate choose 2 2
?pairs
plot(data)
pairs(data[1:12,10:14],main="Multivariate analysis",
      pch=21 )
boxplot(data)
#multivariate analysis
install.packages("car")
library(car)
scat
# ->4 groups of rock : subset and factor 1:4
newdata <- subset(ohorizon,
                  select=LITO) 
newdata
#Newdata<-subset(newdata,LITO==9 | LITO==10 | LITO==51 | LITO==52 |
 #                 LITO==81 | LITO==82 | LITO==83 | LITO==7
  #              LITO==na,
   #             select=c(LITO))
#Newdata
newdata$FactorLevel<-0

newdata
newdata[is.na(newdata)]<-0
newdata[newdata$LITO==9 | newdata$LITO==10,]$FactorLevel<-1
newdata[newdata$LITO==51 | newdata$LITO==52,]$FactorLevel<-2
newdata[newdata$LITO==81 | newdata$LITO==82 |
        newdata$LITO==83,]$FactorLevel<-3
newdata[newdata$LITO==7,]$FactorLevel<-4
levelRock<-subset(newdata, select = c(FactorLevel))#select levels
levelRock
#add to data the factor level 1 to 4 for avg conc purposes
disdata<-ohorizon
disdata$levelRock<-addedData
disdata
ohorizon$addedData<-levelRock
ohorizon$levelRock<-addedData
Disdata<-ohorizon$levelRock
ohorizon$addedData<-Disdata
cbind(ohorizon,Disdata)
ohorizon

plot(ohorizon$COUN,ohorizon$VEG_ZONE)
plot(ohorizon$COUN,ohorizon$addedData)
pairs(ohorizon[1:15,c(5,10,12,13)]
      ,main="Multivariate analysis",
      pch=21 )



#2 cluster Analysis
#yes it is necessary to standardize
ohorizonnew<-ohorizon[-c(5,6,10:12)] #non numeric varaiables
ohorizonNew<-na.omit(ohorizonnew) #removing na columns
data<-scale(ohorizonNew) #scaled data
cor(data)
plot(data)

#cluster Analysis
#cluster_data<-subset(data, select =c(Al),(Be),(As),(Bi),(Co),(Cr),(Fe),(Ni))
cluster_data<-data[c(10,11,17,12,18,26,27,29,30,34,35,41,50)]
c<-
  model<-lm(Bi~Ag+As+ELEV, data = ohorizon)#linear model with bi as response and Ag,Aa,ELEV as predictors.
print(model)
summary(model)# output of our model
#plot(model)
#plot(Bi,Ag)
#plot(Bi,As)
#plot(Bi,ELEV)
