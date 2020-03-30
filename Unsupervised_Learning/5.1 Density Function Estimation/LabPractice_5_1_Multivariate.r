#################################################################################
##############   LabPractice 5.1 Density Estimation  ############################
##############     ----------- solution ---------    ############################
#################################################################################

library(MLTools)
library(MASS)
library(stats)
library(ggplot2)
library(ggfortify)

#Load data
fdata <- read.table("MultivariateDensityEstimation.dat",header = TRUE, sep = "")
ggplot(fdata)+geom_point(aes(x=X1, y=X2))
    #cogemos solo x1 y x2 (y lo usaremos luego para clasificar)

# 2D density plot
ggplot(fdata)+geom_point(aes(x=X1,y=X2))+geom_density2d(aes(x=X1,y=X2))
    #para cada punto saca una gausiana



#########################
#Train Gaussian mixture##
#########################
library(mclust)
#G = NUMERO DE GAUSIANAS 
#modelNames = type of gaussians
    #VVI solo pueden variar en alto y ancho
    #VVV pueden ser inclinadas tambien

#*modelNames = NULL probara distintas combos de gausianas (VVV y VVI es enough)
#*G=NULL probara del 1 al 9



#######1############
densmod <- densityMclust(fdata[,c(1:2)], G=3, modelNames ="VVV") #prueba VVI tb
    #cogemos tres gausianas por ejemplo

#Pintamos los datos y anadimos las gausianas
plot(fdata$X1,fdata$X2)
par(new=T)
plot(densmod, what = "density",col="red")

summary(densmod, parameters = TRUE) 
  #centros de las gausianas y varianzas que se han estimado


#######2############
densmod <- densityMclust(fdata[,c(1:2)], G=1:30, modelNames = "VVI")
plot(densmod, what = "BIC")
    #indice VIC en funcion del numero de gausianas
    #el maximo = optimo = 14 gausianas -> lo coge automaticamente

#Pintamos los datos y anadimos las gausianas
plot(fdata$X1,fdata$X2)
par(new=T)
plot(densmod, what = "density", col="red")

summary(densmod, parameters = TRUE) #Fitted parameters


## Generate 2D dataset with one class
#fdata <- GenMat(1,100,0.25)



#########################################################
##DIVIDIENDO EL DATASET EN TRES - PROB DE CLASIFICACION##
#########################################################

## Load dataset -------------------------------------------------------------------------------------------------------
fdata <- read.table("MultivariateDensityEstimation.dat",header = TRUE, sep = "")
## Or generate dataset with three classes
#fdata <- GenMat(3,100,0.25)


# Divide dataset into the 3 classes
fdata$Y <- as.factor(fdata$Y)
class0 <- fdata[fdata$Y==0,c(1,2)] #c(1,2) coge las columnas x1 y x2
class1 <- fdata[fdata$Y==1,c(1,2)]
class2 <- fdata[fdata$Y==2,c(1,2)]

# Original data
ggplot(fdata)+geom_point(aes(x=X1,y=X2,colour = Y))

# 2D density plot
ggplot(fdata)+geom_point(aes(x=X1,y=X2,colour = Y))+geom_density2d(aes(x=X1,y=X2,colour = Y))
    #para cada clase hace una funcion de densidad para cada punto


# Create a grid of points (PARA PINTAR LAS CURVAS DE CONTORNO)
np_grid <- 100 #number of discretization points in each dimension
np.X1 <- seq(from = min(fdata$X1), to = max(fdata$X1), length.out = np_grid)
np.X2 <- seq(from = min(fdata$X2), to = max(fdata$X2), length.out = np_grid)
p.grid <- expand.grid(X1 = np.X1, X2 = np.X2) 


##VAMOS A AJUSTAR TRES GAUSIANAS PARA CADA CLASE que solo puedan variar en el eje x e y
# PRBFN model for each class
n.kern <- 3 
modNames ="VVI"
#Try changing the number of gaussians and the type to "VVV"

#Density estimation for class 0
prbfn.0 <- densityMclust(class0[,1:2], G=n.kern, modelNames = modNames)
p.grid$d0 <- predict(prbfn.0, p.grid[,1:2], what = "dens")

#Density estimation for class 1
prbfn.1 <- densityMclust(class1[,1:2], G=n.kern, modelNames = modNames)
p.grid$d1 <- predict(prbfn.1, p.grid[,1:2], what = "dens")

#Density estimation for class 2
prbfn.2 <- densityMclust(class2[,1:2], G=n.kern, modelNames = modNames)
p.grid$d2 <- predict(prbfn.2, p.grid[,1:2],what = "dens")


ggplot(p.grid)+geom_contour(aes(x=X1,y=X2,z=d0), colour= "red")+
  geom_contour(aes(x=X1,y=X2,z=d1), colour= "green")+
  geom_contour(aes(x=X1,y=X2,z=d2), colour= "blue")




#Probabilidad P(class=0)

