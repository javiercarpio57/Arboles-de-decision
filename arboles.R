library(dplyr) #Para usar select
library(fpc) #Para la gráfica 
library(cluster) #Parla silueta
library(ggpubr) #Para regresiones

setwd("~/Documents/Mineria/Arboles-de-decision")
houses <- read.csv("train.csv")
#Selected rows
housesFilter <-select(houses, LotFrontage, LotArea, YearBuilt, YearRemodAdd, MasVnrArea, BsmtFinSF1,BsmtFinSF2,BsmtUnfSF,TotalBsmtSF,X1stFlrSF,X2ndFlrSF,LowQualFinSF,GrLivArea,TotRmsAbvGrd,Fireplaces,GarageYrBlt,GarageCars,GarageArea,WoodDeckSF,OpenPorchSF,EnclosedPorch,ScreenPorch,PoolArea,MoSold,YrSold,SalePrice)
#Data cleanup
housesFilter <- na.omit(housesFilter)
#k-medias
cluster <- housesFilter
km<-kmeans(housesFilter,3)
housesFilter$grupo<-km$cluster

plotcluster(cluster,km$cluster) #grafica la ubicación de los clusters
#Método de la silueta para las k-medias
silkm<-silhouette(km$cluster,dist(housesFilter))
mean(silkm[,3]) #Silueta de 0.561677
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
housesFilter <-select(houses, LotFrontage, LotArea, YearBuilt, YearRemodAdd, MasVnrArea, BsmtFinSF1,BsmtUnfSF,TotalBsmtSF,X1stFlrSF,X2ndFlrSF,GrLivArea,TotRmsAbvGrd,Fireplaces,GarageYrBlt,GarageCars,GarageArea,WoodDeckSF,OpenPorchSF,ScreenPorch,PoolArea,MoSold,SalePrice)
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
mean(silkm[,3]) #Silueta de 0.5634391


#----Prueba 2
#Selected rows
housesFilter <-select(houses,LotFrontage, LotArea,BsmtFinSF1, BsmtUnfSF,TotalBsmtSF,X1stFlrSF,GrLivArea,TotRmsAbvGrd,OpenPorchSF, GarageArea,WoodDeckSF,MoSold, SalePrice)
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
#---Prueba 3
housesFilter <-select(houses,MasVnrArea, SalePrice)
