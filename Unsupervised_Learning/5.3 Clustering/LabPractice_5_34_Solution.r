#################################################################################
##############       LabPractice 5.3_4  Clustering     ############################
##############     ----------- solution ---------    ############################
#################################################################################

library(ggplot2)
library(cclust)
library(dendextend)
library(stats)
library(factoextra)
library(NbClust)
library(MLTools)
library(cluster)

## Load dataset -------------------------------------------------------------------------------------------------------
fdata <- read.table("Aggregation.dat",header = FALSE, sep = "", stringsAsFactors = FALSE)
#PINTAMOS LOS DATOS
ggplot(fdata)+geom_point(aes(x=V1,y=V2)) #usamos solo V1 y V2



#2 DIMENSIONES#############################################################

##########################
## Hierarchical clustering -------------------------------------------------------------------------------------------------------
##########################

# Compute distances CALCULA LAS DISTANCIAS DE CADA PUNTO A OTRO
dd <- dist(scale(fdata[,1:2]), method = "euclidean") #V3 no lo queremos

# hierarchical clustering
hc <- hclust(dd, method = "ward.D2") #ward.D2 es la distancia de un punto al centroide del cluster
plot(hc,hang = -1)

# Convert to dendrogram object
#dend <- as.dendrogram(hc)

#CORTAMOS EL ARBOL PARA CONSEGUIR 4 CLUSTERS PORQUE ES LO QUE SE VE EN EL PLOT
cluster <-  cutree(hc, k = 4) #Use "h = " to cut by a given height 

table(cluster) #check number of observations assigned to each cluster
    #CLUSTER ES UN VECTOR QUE DICE A QUE CLUSTER PERTENECE CADA PUNTO

#PINTAMOS LOS CLUSTERS
ggplot(fdata)+geom_point(aes(x=V1,y=V2,color=as.factor(cluster)))

#MISMO PLOT QUE EL ANTERIOR - USAR ESTE
#plot the dendrogram changing the colors according to the number of clusters
hc.col=color_branches(as.dendrogram(hc),k=4)
plot(hc.col)
PlotClusters(NULL,cluster,fdata[,1:2]) #en NULL pondriamos la informacion de centroides




##############################################
## kmeans clustering = partitional clustering -------------------------------------------------------------------------------------------------------
#############################################
#optimiza los centros y asigna los clusters por pura distancia
#Numero especifico de clusters sin gerarquia
set.seed(101)

# Calculate clusters. "centers = " specifies the number of clusters to be obtained
km <- kmeans(fdata[,1:2], centers = 3) #QUEREMOS 3 CENTROS
  #dentro de km hay mazo cosas (los centros, los clusters, etc)

km$centers
km$cluster

# Plot the clusters and the centers
PlotClusters(km$centers,km$cluster,fdata[,1:2])
    #como lo hace por distancia al centro hay puntos verdes en la parte azul

###IMP
## Selecting the number of clusters for KMEANS -------------------------------------------------------

fviz_nbclust(fdata[,1:2], kmeans, method = "wss")+
  labs(subtitle = "Elbow method")
      #segun esto necesitamos 4 o 6
      #a partir de 6 empieza a desagregarse mucho

fviz_nbclust(fdata[,1:2], kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")
    #como de agrupados estan los clusters y como de separados con otros
    #queremos el maximo

##USAR ESTO PARA SABER CUANTOS CLUSTERS USAR!!!#####
nb <- NbClust(fdata[,1:2], distance = "euclidean", min.nc = 2,
              max.nc = 9, method = "kmeans")
    #esto prueba muchos metedos y nos dice el mejor numero de clusters
par(mfrow=c(1,1)) #reset axis
fviz_nbclust(nb)



#TRAS ELEGIR EL NUMERO OPTIMO DE CLUSTERS LO VOLVEMOS A HACER
#Calculated selected number of clusters 
km <- kmeans(fdata[,1:2], 4)
PlotClusters(km$centers,km$cluster,fdata[,1:2])


#PARA COMPARAR CON OTROS MODELOS!!!
#silhouette plot for each point
sil <- silhouette(km$cluster, dd) #valor de silhoutte para cada punto
fviz_silhouette(sil)
  #VALORES MUY CERCANOS A 1 SON PUNTOS BIEN ASOCIADOS A SU CLUSTER
  #VALOR NEGATIVO: MAS CERCA A OTRO CLUSTER QUE A SUS COMPANEROS DE CLUSTER
  #0: lies between two clusters
mean(sil[,3])



###############################
##neural gas - selection of k ---------------------------------------
###############################
#en este caso primero optimizamos el numero de clusters y luego lanzamos el modelo
library(cclust)
tunegrid <- seq(2,21,by = 1)
ssq <- matrix(ncol = length(tunegrid),nrow = 1)
silv <- matrix(ncol = length(tunegrid),nrow = 1) 

#CALCULAMOS EL ELBOW CURVE Y SHILOUTTE DE MENARA MANUAL
i <- 1
for(k in tunegrid){
  ng <- cclust(as.matrix(fdata[,1:2]),centers = k,method = "neuralgas")
  #Total sum of squares and silhouette
  ssq[i] <- sum(ng$withinss)
  sil <- silhouette(ng$cluster, dd)
  silv[i] <- mean(sil[,3])
  i <- i+1
}
par(mfrow=c(2,1))
plot(tunegrid,ssq,type="b")
plot(tunegrid,silv,type="b")
par(mfrow=c(1,1))

#PROBAMOS CON 4
#fit final model
ng <- cclust(as.matrix(fdata[,1:2]),centers = 4,method = "neuralgas")
PlotClusters(ng$centers,ng$cluster,fdata[,1:2])

#PARA COMPARAR CON OTROS MODELOS
#silhouette average
sil <- silhouette(ng$cluster, dd)
fviz_silhouette(sil)
mean(sil[,3]) #VALOR MEDIO QUE QUEREMOS CERCANO A 1!!! PARA COMPARAR CON OTROS MODELOS


###########################
#Prbfn - Gaussian Mixture --------------------------------------------
###########################
library(mclust)

#primero optimizamos el numero de clusters
tunegrid <- seq(2,18,by = 1)
ssq <- matrix(ncol = length(tunegrid),nrow = 1)
silv <- matrix(ncol = length(tunegrid),nrow = 1) 
i <- 1
for(k in tunegrid){
  Sys.sleep(0.2)
  prbfn <- Mclust(fdata[,1:2],G=k,modelNames = "VVI")
  #Total sum of squares and silhouette
  ssq[i] <- Reconstruction.Error(t(prbfn$parameters$mean),prbfn$classification,fdata[,1:2])
  sil <- silhouette(prbfn$classification, dd)
  silv[i] <- mean(sil[,3])
  i <- i+1
}
par(mfrow=c(2,1))
plot(tunegrid,ssq,type="b")
plot(tunegrid,silv,type="b")
par(mfrow=c(1,1))


#PROBAMOS CON 3 GAUSIANAS
#fit final model
prbfn <- Mclust(fdata[,1:2],G=3,modelNames = "VVI")
PlotClusters(t(prbfn$parameters$mean),prbfn$classification,fdata[,1:2])
    #CADA PUNTO ES EL CENTRO DE LA GAUSSIANA QUE SE HA USADO PARA ESTIMAR

#PARA COMPARAR MODELOS
#silhouette average
sil <- silhouette(prbfn$classification, dd)
fviz_silhouette(sil)
mean(sil[,3]) #un poco peor que el metodo





#####################################################################
################# High dimensional data #############################
#####################################################################
## Load dataset -------------------------------------------------------------------------------------------------------
fdata <- read.table("Dem_2011_2015_24h.dat",header = TRUE, sep = "", stringsAsFactors = FALSE)
matplot(t(as.matrix(fdata[1:5,3:26])),type="l") #COGEMOS LOS VALORES DE HORAS QUE ME INTERESAN



#########################
##Hierarchical clustering----------------------------------------------------------------------------
#########################

# Compute distances and hierarchical clustering
dd <- dist(scale(fdata[,3:26]), method = "euclidean") 
#clusetring
hc <- hclust(dd, method = "ward.D2")
plot(hc,hang = -1)

#ELEGIMOS 3 CLUSTERS
#Cut the tree to a given number of clusters
cluster <-  cutree(hc, k = 3) #Use "h = " to cut by a given height 
table(cluster) #check number of observations assigned to each cluster

#plot the dendrogram changing the colors according to the number of clusters
hc.col=color_branches(as.dendrogram(hc),k=3)
plot(hc.col)

#Color curves according to cluster
matplot(t(as.matrix(fdata[,3:26])),type="l",col = cluster)
    #PINAMOS CADA CURVA CON COMO SE ASOCIA CON EL CLUSTER


# Plot the cluster in Principal Component subspace
PlotClusters(NULL,cluster,fdata[,3:26])
    #COGE DOS PC DE MANERA AUTOMATICA

#Compare cluster and category
table(cluster,fdata$DAY)

########
##Kmeans-------------------------------------------------------------------------------------------
########
#Select optimum number of clusters
fviz_nbclust(fdata[,3:26], kmeans, method = "wss")+
  labs(subtitle = "Elbow method")
fviz_nbclust(fdata[,3:26], kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

#USAR ESTE QUE PRUEBA MUCHOS
nb <- NbClust(fdata[,3:26], distance = "euclidean", min.nc = 2,
              max.nc = 9, method = "kmeans")
fviz_nbclust(nb)


#Calculated selected number of clusters 3
km <- kmeans(fdata[,3:26], 3)
#Color curves according to cluster
matplot(t(as.matrix(fdata[,3:26])),type="l",col = km$cluster)

# Plot the cluster in Principal Component subspace and represent centers
PlotClusters(km$centers,km$cluster,fdata[,3:26])
  #NOS DA LOS CLUSTERS Y TB UN GRAFICO CON LAS CURVAS DE LOS CENTROIDES
  #los dias asociados al cluster 2 tienen un perfil de demanda mas bajo

#Compare cluster and category
table(km$cluster,fdata$DAY)


####VIMOS HASTA AQUI###


## neural gas - selection of k -------------------------------------------------------------
tunegrid <- seq(2,20,by = 1)
ssq <- matrix(ncol = length(tunegrid),nrow = 1)
silv <- matrix(ncol = length(tunegrid),nrow = 1) 
i <- 1
for(k in tunegrid){
  ng <- cclust(as.matrix(fdata[,3:26]),centers = k,method = "neuralgas")
  #Total sum of squares and silhouette
  ssq[i] <- sum(ng$withinss)
  sil <- silhouette(ng$cluster, dd)
  silv[i] <- mean(sil[,3])
  i <- i+1
}
par(mfrow=c(2,1))
plot(tunegrid,ssq,type="b")
plot(tunegrid,silv,type="b")
par(mfrow=c(1,1))

#fit final model
ng <- cclust(as.matrix(fdata[,3:26]),centers = 3,method = "neuralgas")
PlotClusters(ng$centers,ng$cluster,fdata[,3:26])
#silhouette average
sil <- silhouette(ng$cluster, dd)
mean(sil[,3])



##prbfn - Gaussian Mixture --------------------------------------------------
tunegrid <- seq(2,15,by = 1)
ssq <- matrix(ncol = length(tunegrid),nrow = 1)
silv <- matrix(ncol = length(tunegrid),nrow = 1) 
i <- 1
for(k in tunegrid){
  Sys.sleep(0.2)
  prbfn <- Mclust(fdata[,3:26],G=k,modelNames = "VVI")
  #Total sum of squares and silhouette
  ssq[i] <- Reconstruction.Error(t(prbfn$parameters$mean),prbfn$classification,fdata[,3:26])
  sil <- silhouette(prbfn$classification, dd)
  silv[i] <- mean(sil[,3])
  i <- i+1
}
par(mfrow=c(2,1))
plot(tunegrid,ssq,type="b")
plot(tunegrid,silv,type="b")
par(mfrow=c(1,1))

prbfn <- Mclust(fdata[,3:26], G=3, modelNames = "VVI")
PlotClusters(t(prbfn$parameters$mean),prbfn$classification,fdata[,3:26])
#silhouette average
sil <- silhouette(prbfn$classification, dd)
mean(sil[,3])



