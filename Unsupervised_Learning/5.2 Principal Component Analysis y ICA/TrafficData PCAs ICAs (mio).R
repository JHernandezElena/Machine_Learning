#####################################################################################
### TRAFFIC DATA   ############################################################
#####################################################################################


fdatatot <- read.table("TrafficData.dat",header = TRUE, sep = "")
#cada DEMx es la demanda a una hora determinada

fdatatot$DAY <- as.factor(fdatatot$DAY)
fdatatot$YEAR <- as.factor(fdatatot$MONTH)

fdata <- fdatatot[,5:28] #VAMOS A USAR SOLO LOS DATOS DE DEMANDA DESDE LA HORA 1 HASTA LA 24
#Plot first five traffic profiles in the dataset
matplot(t(as.matrix(fdata[1:5,])),type="l",
        xlab = "Hours", ylab = "Traffic", main = "Five demand profiles")

fdata.pca<-prcomp(fdata, center = TRUE, scale. = FALSE) 
    #IMP!!!: NO LO QUEREMOS ESCALAR PORQUE TODAS LAS VARIABLES TIENEN LAS MISMAS UNIDADES
summary(fdata.pca)
  #Aqui vemos cuando cada PC explica la variable (de 0 a 1)
  #Para explicar el 95% cogeriamos 5 PC
  #COGEMOS 2 PARA EXPLICAR EL 90%

std_dev <- fdata.pca$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)

#POR COMPONENTE PRINCIPAL CUANTA VARIABILIDAD EXPLICO:
barplot(prop_varex[1:8], xlab = "Principal Component",
        ylab = "Proportion of Variance Explained",
        names.arg=dimnames(fdata.pca$rotation)[[2]][1:8],
        ylim = c(0,1))
lines(cumsum(prop_varex[1:8]), col="blue", lwd = 3)
legend(x=7,y=1,legend = "Cumulative Proportion",col = "blue",lty = 1)



#PESOS DE CADA VARIABLE A LOS DOS PRIMEROS PCS
#Plot principal component loadings
par(mfrow=c(2,1))
barplot(fdata.pca$rotation[,1],ylab="PC1") 
    #el trafico es mas alto antes de entrar al trabajo y sobre todo despues
barplot(fdata.pca$rotation[,2],ylab="PC2")
  #el PC2 le da mas importancia al resto de horas
par(mfrow=c(1,1))

#VALORES DE LOS COMPONENTES PRINCIPALES (1,2) PARA CADA DIA QUE TENEMOS:
#plot component scores
par(mfrow=c(3,1))
plot(fdata.pca$x[,1],type="l",ylab="PC1", xlab = "t(days)", main = "Time series of principal components")
plot(fdata.pca$x[,2],type="l",ylab="PC2", xlab = "t(days)")
par(mfrow=c(1,1))

#RECONSTRRUCCION DE LOS PRIMEROS 4 DIAS CON DOS PRIMEROS PCS
#Reconstruction of first profile with three components
for(i in 1:4){ #DIAS 1 AL 4
  plot(1:24, as.matrix(fdata[i,]),type="l", xlab = "Hours", ylab = "Traffic")
  fdata.rec <- fdata.pca$center + (fdata.pca$x[i,1:2] %*% t(fdata.pca$rotation[,1:2]))*ifelse(fdata.pca$scale,fdata.pca$scale,1) #el '3' es el numerp de PCs
  lines(1:24,fdata.rec[1,],col="red")
  legend(x=1,y=28000,legend = c("Real", "Reconstruction"), col = c("black", "red"),lty = 1)
  Sys.sleep(0.5)
}

#VALORES DE LOS 2 PRIMEROS PC PARA TODOS LOS DIAS
#plot data in space spanned by first 2 PC
df_plot <- data.frame(PC1 = fdata.pca$x[,1], PC2 =fdata.pca$x[,2], WEEKDAY=fdatatot$WEEKDAY, MONTH=fdatatot$MONTH, YEAR=fdatatot$YEAR)
ggplot(df_plot)+geom_point(aes(x=PC1,y=PC2)) #Vemos varias tendencias
ggplot(df_plot)+geom_point(aes(x=PC1,y=PC2, color=WEEKDAY)) #segun el tipo de dia tiene sentido
    #vemos que los findes tiene menos trafico (PC1 mas positivo)
    #el PC2 modela el trafico a la hora de la comida por eso los viernes es mucho el PC2 es mas bajo (MAS TRAFICO)
ggplot(df_plot)+geom_point(aes(x=PC1,y=PC2, color=MONTH))
ggplot(df_plot)+geom_point(aes(x=PC1,y=PC2, color=YEAR))



###### ICA
#2 COMPONENTES INDEPENDIENTS
library(fastICA)
fdata.ica <- fastICA(fdata, 2, alg.typ = "parallel", fun = "logcosh", alpha = 1,
                     method = "R", row.norm = FALSE, maxit = 200,
                     tol = 0.0001, verbose = TRUE)
summary(fdata.ica)

#plot component scores
par(mfrow=c(2,1))
plot(fdata.ica$S[1:50,1],type="l",ylab="IC1", xlab = "t(days)", main = "Time series of independent components")
plot(fdata.ica$S[1:50,2],type="l",ylab="IC2", xlab = "t(days)")
par(mfrow=c(1,1))


#plot data in space spanned by first 2 IC
df_plot <- data.frame(IC1 = fdata.ica$S[,1], IC2 =fdata.ica$S[,2], WEEKDAY=fdatatot$WEEKDAY, MONTH=fdatatot$MONTH, YEAR=fdatatot$YEAR)
ggplot(df_plot)+geom_point(aes(x=IC1,y=IC2,color=WEEKDAY))
  #SOLO SABIENDO EL VALOR DE IC2 PUEDO DECIR SI ESTO ES UN DIA LABORABLE O NO
ggplot(df_plot)+geom_point(aes(x=IC1,y=IC2,color=MONTH))
    

#Boxplot of independent component by DAY
ggplot(df_plot)+geom_boxplot(aes(x=WEEKDAY,y=IC2))
