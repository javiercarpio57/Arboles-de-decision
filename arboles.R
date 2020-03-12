library(dplyr) #Para usar select
library(fpc) #Para la gráfica 
library(cluster) #Parla silueta
library("ggpubr") #Para regresiones

setwd("~/Documents/Mineria/Arboles-de-decision")
houses <- read.csv("train.csv")
#Selected rows
housesFilter <-select(houses, LotFrontage, LotArea, YearBuilt, YearRemodAdd, MasVnrArea, BsmtFinSF1,BsmtFinSF2,BsmtUnfSF,TotalBsmtSF,X1stFlrSF,X2ndFlrSF,LowQualFinSF,GrLivArea,TotRmsAbvGrd,Fireplaces,GarageYrBlt,GarageCars,GarageArea,WoodDeckSF,OpenPorchSF,EnclosedPorch,ScreenPorch,PoolArea,MoSold,YrSold,SalePrice)
#Data cleanup
housesFilter <- na.omit(housesFilter)

# Resumen de variables
summary(housesFilter)

# Grafica de plot de numericos contra sale price
plot(housesFilter$LotFrontage, housesFilter$SalePrice) # NO
plot(housesFilter$LotArea, housesFilter$SalePrice) # NO
plot(housesFilter$YearBuilt,  housesFilter$SalePrice)
plot(housesFilter$YearRemodAdd,  housesFilter$SalePrice) # SI
plot(housesFilter$MasVnrArea,  housesFilter$SalePrice)
plot(housesFilter$BsmtFinSF1, housesFilter$SalePrice)
plot(housesFilter$BsmtFinSF2, housesFilter$SalePrice)
plot(housesFilter$BsmtUnfSF, housesFilter$SalePrice)
plot(housesFilter$TotalBsmtSF, housesFilter$SalePrice)
plot(housesFilter$X1stFlrSF, housesFilter$SalePrice)
plot(housesFilter$X2ndFlrSF, housesFilter$SalePrice)
plot(housesFilter$LowQualFinSF, housesFilter$SalePrice)
plot(housesFilter$GrLivArea, housesFilter$SalePrice)
plot(housesFilter$TotRmsAbvGrd, housesFilter$SalePrice) # SI
plot(housesFilter$Fireplaces, housesFilter$SalePrice)
plot(housesFilter$GarageYrBlt, housesFilter$SalePrice) # NO
plot(housesFilter$GarageCars, housesFilter$SalePrice) # NO
plot(housesFilter$GarageArea, housesFilter$SalePrice)
plot(housesFilter$WoodDeckSF, housesFilter$SalePrice)
plot(housesFilter$OpenPorchSF, housesFilter$SalePrice)
plot(housesFilter$EnclosedPorch, housesFilter$SalePrice)
plot(housesFilter$ScreenPorch, housesFilter$SalePrice)
plot(housesFilter$PoolArea, housesFilter$SalePrice) # NO
plot(housesFilter$MoSold, housesFilter$SalePrice) # NO
plot(housesFilter$YrSold, housesFilter$SalePrice) # NO


hist(housesFilter$LotFrontage) 
hist(housesFilter$LotArea) 
hist(housesFilter$YearBuilt)
hist(housesFilter$YearRemodAdd)
hist(housesFilter$MasVnrArea)
hist(housesFilter$BsmtFinSF1)
hist(housesFilter$BsmtFinSF2)
hist(housesFilter$BsmtUnfSF)
hist(housesFilter$TotalBsmtSF)
hist(housesFilter$X1stFlrSF)
hist(housesFilter$X2ndFlrSF)
hist(housesFilter$LowQualFinSF)
hist(housesFilter$GrLivArea)
hist(housesFilter$TotRmsAbvGrd)
hist(housesFilter$Fireplaces)
hist(housesFilter$GarageYrBlt) 
hist(housesFilter$GarageCars) 
hist(housesFilter$GarageArea)
hist(housesFilter$WoodDeckSF)
hist(housesFilter$OpenPorchSF)
hist(housesFilter$EnclosedPorch)
hist(housesFilter$ScreenPorch)
hist(housesFilter$PoolArea) 
hist(housesFilter$MoSold) 
hist(housesFilter$YrSold) 
hist(housesFilter$SalePrice)

qqnorm(housesFilter$LotFrontage)
qqline(housesFilter$LotFrontage, col='red')
qqnorm(housesFilter$LotArea)
qqline(housesFilter$LotArea, col='red')
qqnorm(housesFilter$YearBuilt)
qqline(housesFilter$YearBuilt, col='red')
qqnorm(housesFilter$YearRemodAdd)
qqline(housesFilter$YearRemodAdd, col='red')
qqnorm(housesFilter$MasVnrArea)
qqline(housesFilter$MasVnrArea, col='red')
qqnorm(housesFilter$BsmtFinSF1)
qqline(housesFilter$BsmtFinSF1, col='red')
qqnorm(housesFilter$BsmtFinSF2)
qqline(housesFilter$BsmtFinSF2, col='red')
qqnorm(housesFilter$BsmtUnfSF)
qqline(housesFilter$BsmtUnfSF, col='red')
qqnorm(housesFilter$TotalBsmtSF)
qqline(housesFilter$TotalBsmtSF, col='red')
qqnorm(housesFilter$X1stFlrSF)
qqline(housesFilter$X1stFlrSF, col='red')
qqnorm(housesFilter$X2ndFlrSF)
qqline(housesFilter$X2ndFlrSF, col='red')
qqnorm(housesFilter$LowQualFinSF)
qqline(housesFilter$LowQualFinSF, col='red')
qqnorm(housesFilter$GrLivArea)
qqline(housesFilter$GrLivArea, col='red')
qqnorm(housesFilter$TotRmsAbvGrd)
qqline(housesFilter$TotRmsAbvGrd, col='red')
qqnorm(housesFilter$Fireplaces)
qqline(housesFilter$Fireplaces, col='red')
qqnorm(housesFilter$GarageYrBlt) 
qqline(housesFilter$GarageYrBlt, col='red') 
qqnorm(housesFilter$GarageCars) 
qqline(housesFilter$GarageCars, col='red') 
qqnorm(housesFilter$GarageArea)
qqline(housesFilter$GarageArea, col='red')
qqnorm(housesFilter$WoodDeckSF)
qqline(housesFilter$WoodDeckSF, col='red')
qqnorm(housesFilter$OpenPorchSF)
qqline(housesFilter$OpenPorchSF, col='red')
qqnorm(housesFilter$EnclosedPorch)
qqline(housesFilter$EnclosedPorch, col='red')
qqnorm(housesFilter$ScreenPorch)
qqline(housesFilter$ScreenPorch, col='red')
qqnorm(housesFilter$PoolArea) 
qqline(housesFilter$PoolArea, col='red')
qqnorm(housesFilter$MoSold) 
qqline(housesFilter$MoSold, col='red') 
qqnorm(housesFilter$YrSold) 
qqline(housesFilter$YrSold, col='red')
qqnorm(housesFilter$SalePrice)
qqline(housesFilter$SalePrice, col='red')

#k-medias
cluster <- housesFilter
km<-kmeans(housesFilter,3)
housesFilter$grupo<-km$cluster

plotcluster(cluster,km$cluster) #grafica la ubicación de los clusters
#Método de la silueta para las k-medias
silkm<-silhouette(km$cluster,dist(housesFilter))
mean(silkm[,3]) #Silueta de 0.561677

g1<- housesFilter[housesFilter$grupo==1,]
prop.table(table(g1$Species))*100
g2<- housesFilter[housesFilter$grupo==2,]
prop.table(table(g2$Species))*100
g3<- housesFilter[housesFilter$grupo==3,]
prop.table(table(g3$Species))*100

summary(g1)
summary(g2)
summary(g3)
## Correlación entre variables independientes y el precio de venta 
cor(housesFilter$LotFrontage, housesFilter$SalePrice, method = c("pearson", "kendall", "spearman"))
cor(housesFilter$LotArea, housesFilter$SalePrice, method = c("pearson", "kendall", "spearman"))
cor(housesFilter$YearBuilt, housesFilter$SalePrice, method = c("pearson", "kendall", "spearman"))
cor(housesFilter$YearRemodAdd, housesFilter$SalePrice, method = c("pearson", "kendall", "spearman"))
cor(housesFilter$MasVnrArea, housesFilter$SalePrice, method = c("pearson", "kendall", "spearman"))
cor(housesFilter$BsmtFinSF1, housesFilter$SalePrice, method = c("pearson", "kendall", "spearman"))
cor(housesFilter$BsmtFinSF2, housesFilter$SalePrice, method = c("pearson", "kendall", "spearman"))
cor(housesFilter$BsmtUnfSF, housesFilter$SalePrice, method = c("pearson", "kendall", "spearman"))
cor(housesFilter$TotalBsmtSF, housesFilter$SalePrice, method = c("pearson", "kendall", "spearman"))
cor(housesFilter$X1stFlrSF, housesFilter$SalePrice, method = c("pearson", "kendall", "spearman"))
cor(housesFilter$X2ndFlrSF, housesFilter$SalePrice, method = c("pearson", "kendall", "spearman"))
cor(housesFilter$LowQualFinSF, housesFilter$SalePrice, method = c("pearson", "kendall", "spearman"))
cor(housesFilter$GrLivArea, housesFilter$SalePrice, method = c("pearson", "kendall", "spearman"))
cor(housesFilter$TotRmsAbvGrd, housesFilter$SalePrice, method = c("pearson", "kendall", "spearman"))
cor(housesFilter$Fireplaces, housesFilter$SalePrice, method = c("pearson", "kendall", "spearman"))
cor(housesFilter$GarageYrBlt, housesFilter$SalePrice, method = c("pearson", "kendall", "spearman"))
cor(housesFilter$GarageCars, housesFilter$SalePrice, method = c("pearson", "kendall", "spearman"))
cor(housesFilter$GarageArea, housesFilter$SalePrice, method = c("pearson", "kendall", "spearman"))
cor(housesFilter$WoodDeckSF, housesFilter$SalePrice, method = c("pearson", "kendall", "spearman"))
cor(housesFilter$OpenPorchSF, housesFilter$SalePrice, method = c("pearson", "kendall", "spearman"))
cor(housesFilter$EnclosedPorch, housesFilter$SalePrice, method = c("pearson", "kendall", "spearman"))
cor(housesFilter$ScreenPorch, housesFilter$SalePrice, method = c("pearson", "kendall", "spearman"))
cor(housesFilter$PoolArea, housesFilter$SalePrice, method = c("pearson", "kendall", "spearman"))
cor(housesFilter$MoSold, housesFilter$SalePrice, method = c("pearson", "kendall", "spearman"))
cor(housesFilter$YrSold, housesFilter$SalePrice, method = c("pearson", "kendall", "spearman"))
#----------Prueba
#Selected rows
#housesFilter <-select(houses, LotFrontage, LotArea, YearBuilt, YearRemodAdd, MasVnrArea, BsmtFinSF1,BsmtUnfSF,TotalBsmtSF,X1stFlrSF,X2ndFlrSF,GrLivArea,TotRmsAbvGrd,Fireplaces,GarageYrBlt,GarageCars,GarageArea,WoodDeckSF,OpenPorchSF,ScreenPorch,PoolArea,MoSold,SalePrice)
#Data cleanup
#housesFilter <- na.omit(housesFilter)
#k-medias
#cluster <- housesFilter
#km<-kmeans(housesFilter,3)
#housesFilter$grupo<-km$cluster

#plotcluster(cluster,km$cluster) #grafica la ubicación de los clusters
#Método de la silueta para las k-medias
#silkm<-silhouette(km$cluster,dist(housesFilter))
#mean(silkm[,3]) #Silueta de 0.561677
## Correlación entre variables independientes y el precio de venta 


## Se Elimina LotFrontage, LotArea, MasVnArea, BsmtFinSF1, BsmtFinSF2,
#BsmtUnfSF, X2ndFlrSF, LowQualFinSF, Fireplaces, WoodDeckSF, OpenPorchSF
#EnclosedPorch, ScreenPorch, PoolArea, MoSold, YrSold

#Selected rows
housesFilter <-select(houses, YearBuilt, YearRemodAdd,TotalBsmtSF,X1stFlrSF,GrLivArea,TotRmsAbvGrd,GarageCars,GarageArea,SalePrice)
#Data cleanup
housesFilter <- na.omit(housesFilter)
#k-medias
cluster <- housesFilter
km<-kmeans(housesFilter,3)
housesFilter$grupo<-km$cluster

plotcluster(cluster,km$cluster) #grafica la ubicación de los clusters
#Método de la silueta para las k-medias
silkm<-silhouette(km$cluster,dist(housesFilter))
mean(silkm[,3]) #Silueta de 0.5634391
g1<- housesFilter[housesFilter$grupo==1,]
prop.table(table(g1$Species))*100
g2<- housesFilter[housesFilter$grupo==2,]
prop.table(table(g2$Species))*100
g3<- housesFilter[housesFilter$grupo==3,]
prop.table(table(g3$Species))*100

summary(g1)
summary(g2)
summary(g3)
## Correlación entre variables independientes y el precio de venta 
cor(housesFilter$YearBuilt, housesFilter$SalePrice, method = c("pearson", "kendall", "spearman"))
cor(housesFilter$YearRemodAdd, housesFilter$SalePrice, method = c("pearson", "kendall", "spearman"))
cor(housesFilter$TotalBsmtSF, housesFilter$SalePrice, method = c("pearson", "kendall", "spearman"))
cor(housesFilter$X1stFlrSF, housesFilter$SalePrice, method = c("pearson", "kendall", "spearman"))
cor(housesFilter$GrLivArea, housesFilter$SalePrice, method = c("pearson", "kendall", "spearman"))
cor(housesFilter$TotRmsAbvGrd, housesFilter$SalePrice, method = c("pearson", "kendall", "spearman"))
cor(housesFilter$GarageCars, housesFilter$SalePrice, method = c("pearson", "kendall", "spearman"))
cor(housesFilter$GarageArea, housesFilter$SalePrice, method = c("pearson", "kendall", "spearman"))
#Selected rows
housesFilter <-select(houses,TotalBsmtSF,X1stFlrSF,GrLivArea,GarageCars,GarageArea,SalePrice)
#Data cleanup
housesFilter <- na.omit(housesFilter)
#k-medias
cluster <- housesFilter
km<-kmeans(housesFilter,3)
housesFilter$grupo<-km$cluster

plotcluster(cluster,km$cluster) #grafica la ubicación de los clusters
#Método de la silueta para las k-medias
silkm<-silhouette(km$cluster,dist(housesFilter))
mean(silkm[,3]) #Silueta de 0.562137


#----Prueba 2
#Selected rows
#housesFilter <-select(houses,LotFrontage, LotArea,BsmtFinSF1, BsmtUnfSF,TotalBsmtSF,X1stFlrSF,GrLivArea,TotRmsAbvGrd,OpenPorchSF, GarageArea,WoodDeckSF,MoSold, SalePrice)
#Data cleanup
#housesFilter <- na.omit(housesFilter)
#k-medias
#cluster <- housesFilter
#km<-kmeans(housesFilter,3)
#housesFilter$grupo<-km$cluster

#plotcluster(cluster,km$cluster) #grafica la ubicación de los clusters
#Método de la silueta para las k-medias
#silkm<-silhouette(km$cluster,dist(housesFilter))
#mean(silkm[,3]) #Silueta de 0.5634391
#---Prueba 3
cor(housesFilter$TotalBsmtSF, housesFilter$SalePrice, method = c("pearson", "kendall", "spearman"))
cor(housesFilter$X1stFlrSF, housesFilter$SalePrice, method = c("pearson", "kendall", "spearman"))
cor(housesFilter$GrLivArea, housesFilter$SalePrice, method = c("pearson", "kendall", "spearman"))
cor(housesFilter$GarageCars, housesFilter$SalePrice, method = c("pearson", "kendall", "spearman"))
cor(housesFilter$GarageArea, housesFilter$SalePrice, method = c("pearson", "kendall", "spearman"))
#Selected rows
housesFilter <-select(houses,TotalBsmtSF,X1stFlrSF,GrLivArea,GarageCars,GarageArea,SalePrice)
#Data cleanup
housesFilter <- na.omit(housesFilter)
#k-medias
cluster <- housesFilter
km<-kmeans(housesFilter,3)
housesFilter$grupo<-km$cluster

plotcluster(cluster,km$cluster) #grafica la ubicación de los clusters
#Método de la silueta para las k-medias
silkm<-silhouette(km$cluster,dist(housesFilter))
mean(silkm[,3]) #Silueta de 0.562137

#Mixture of gaussians
housesFilter <-select(houses, LotFrontage, LotArea, YearBuilt, YearRemodAdd, MasVnrArea,SalePrice)
#Data cleanup
housesFilter <- na.omit(housesFilter)
mc<-Mclust(housesFilter,3)
plot(mc, what = "classification", main="MClust Classification")
housesFilter$mxGau<-mc$classification
#Mixture of gaussians
housesFilter <-select(houses, BsmtFinSF1,BsmtFinSF2,BsmtUnfSF,TotalBsmtSF,SalePrice)
#Data cleanup
housesFilter <- na.omit(housesFilter)
mc<-Mclust(housesFilter,3)
plot(mc, what = "classification", main="MClust Classification")
housesFilter$mxGau<-mc$classification
#Mixture of gaussians
housesFilter <-select(houses,X1stFlrSF,X2ndFlrSF,GrLivArea,GarageCars,GarageYrBlt,SalePrice)
#housesFilter <-select(houses,GarageArea,WoodDeckSF,OpenPorchSF,EnclosedPorch,SalePrice)
#Data cleanup

housesFilter <- na.omit(housesFilter)
mc<-Mclust(housesFilter,3)
plot(mc, what = "classification", main="MClust Classification")
#housesFilter$mxGau<-mc$classification
housesFilter 
#Mixture of gaussians
#housesFilter <-select(houses,TotRmsAbvGrd,Fireplaces,GarageYrBlt,GarageCars,SalePrice)
#Data cleanup
#housesFilter <- na.omit(housesFilter)
#mc<-Mclust(housesFilter,3)
#plot(mc, what = "classification", main="MClust Classification")
#datos$mxGau<-mc$classification
#Mixture of gaussians
#housesFilter <-select(houses,ScreenPorch,PoolArea,MoSold,YrSold,SalePrice)
#Data cleanup
#housesFilter <- na.omit(housesFilter)
#mc<-Mclust(housesFilter,3)
#plot(mc, what = "classification", main="MClust Classification")
#datos$mxGau<-mc$classification
#Mixture of gaussians
#housesFilter <-select(houses,GarageArea,WoodDeckSF,OpenPorchSF,EnclosedPorch,SalePrice)
#Data cleanup
#housesFilter <- na.omit(housesFilter)
#mc<-Mclust(housesFilter,3)
#plot(mc, what = "classification", main="MClust Classification")
#datos$mxGau<-mc$classification
#Prueba
#housesFilter <-select(houses, GarageYrBlt, X1stFlrsSF ,SalePrice)
#Data cleanup
#housesFilter <- na.omit(housesFilter)
#mc<-Mclust(housesFilter,3)
#plot(mc, what = "classification", main="MClust Classification")
#housesFilter$mxGau<-mc$classification

cluster <- housesFilter
km<-kmeans(housesFilter,3)
housesFilter$grupo<-km$cluster

#Método de la silueta para las k-medias
silkm<-silhouette(km$cluster,dist(housesFilter))
mean(silkm[,3]) #0.55, no es la mejor partición pero no está mal

#Método de la silueta para mixture of gaussians
#silmg<-silhouette(mc$classification,dist(housesFilter))
#mean(silmg[,3]) #0.50, no es la mejor partición pero no está mal

library(rpart)
library(caret)
library(tree)
library(rpart.plot)
library(randomForest)
#housesFilter <-select(houses,GarageArea,WoodDeckSF,OpenPorchSF,EnclosedPorch,grupo)
#Data cleanup
housesFilter
housesFiltertree <- select(housesFilter,X1stFlrSF,X2ndFlrSF,GrLivArea,GarageCars,GarageYrBlt,grupo)
#<- na.omit(housesFilter)
# variable respuesta la clase de la flor
housesFiltertree
porciento <- 70/100

set.seed(123)
trainRowsNumber<-sample(1:nrow(housesFiltertree),porciento*nrow(housesFiltertree))
train<-housesFiltertree[trainRowsNumber,]
test<-housesFiltertree[-trainRowsNumber,]
train
#Clasiffication Tree
dt_model<-rpart(train$grupo~.,train,method = "class")
plot(dt_model);text(dt_model)
prp(dt_model)
rpart.plot(dt_model)

head(test)
prediccion <- predict(dt_model, newdata = test[1:5])
prediccion

columnaMasAlta<-apply(prediccion, 1, function(x) colnames(prediccion)[which.max(x)])
test$prediccion<-columnaMasAlta #Se le añade al grupo de prueba el valor de la predicción
test#Resultado de la predicción

cfm<-table(test$grupo,test$prediccion)
cfm
#con random forest
modeloRF1<-randomForest(train$grupo~.,data=train)
prediccionRF1<-predict(modeloRF1,newdata = test)
testCompleto<-test
testCompleto
testCompleto$predRF<-prediccionRF1
cfmRandomForest <- confusionMatrix(testCompleto$predRF, testCompleto$Species)
#Regresion Tree
dt_model<-rpart(train$grupo~.,train,method = "anova")
plot(dt_model);text(dt_model)
prp(dt_model)
rpart.plot(dt_model)

head(test)
prediccion <- predict(dt_model, newdata = test[1:5])
prediccion

columnaMasAlta<-apply(prediccion, 1, function(x) colnames(prediccion)[which.max(x)])
test$prediccion<-columnaMasAlta #Se le añade al grupo de prueba el valor de la predicción
test#Resultado de la predicción

cfm<-table(test$grupo,test$prediccion)
cfm
