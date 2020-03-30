#################################################################################
##############    Lab 5.2 Dimensionality reduction   ############################
#################################################################################

library(ggfortify)

#NOS SALTAMOS ESTO HASTA LA PARTE DE LA DEMANDA
## Load dataset -------------------------------------------------------------------------------------------------------
Countries <- read.table("Countries.dat",header = TRUE, sep = "", stringsAsFactors = FALSE)

#Perform principal component analysis
Countries.pca<-prcomp(Countries[,2:9], center = TRUE, scale. = TRUE) 
summary(Countries.pca)

#Plot eigenvalues obtained
plot(Countries.pca,type="b")

#Calculate and plot variance explained
std_dev <- Countries.pca$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)
barplot(prop_varex, xlab = "Principal Component",
        ylab = "Proportion of Variance Explained",
        names.arg=dimnames(Countries.pca$rotation)[[2]],
        ylim = c(0,1))
lines(cumsum(prop_varex), col="blue")
legend(x=0,y=1,legend = "Cumulative Proportion",col = "blue",lty = 1)

#Plot first 3 principal components
par(mfrow=c(3,1))
barplot(Countries.pca$rotation[,1],ylab="PC1")
barplot(Countries.pca$rotation[,2],ylab="PC2")
barplot(Countries.pca$rotation[,3],ylab="PC3")
par(mfrow=c(1,1))

#Plot data in pc axis
autoplot(Countries.pca, size= 0, #size = 0 to show only  the labels
         label = TRUE, label.label = Countries$Country, label.size = 2,
         loadings = TRUE, loadings.label = TRUE, loadings.label.size = 4)





#####################################################################################
### Electricity demand   ############################################################
#####################################################################################

fdatatot <- read.table("Dem_2011_2015_24h.dat",header = TRUE, sep = "")
        #cada DEMx es la demanda a una hora determinada
fdatatot$DAY <- as.factor(fdatatot$DAY)
fdatatot$MONTH <- as.factor(fdatatot$MONTH)

fdata <- fdatatot[,4:27] #VAMOS A USAR SOLO LOS DATOS DE DEMANDA DESDE LA HORA 1 HASTA LA 24
#Plot first five demand profiles in the dataset
matplot(t(as.matrix(fdata[1:5,])),type="l",
        xlab = "Hours", ylab = "Demand", main = "Five demand profiles")


fdata.pca<-prcomp(fdata, center = TRUE, scale. = FALSE) 
        #IMP!!!: NO LO QUEREMOS ESCALAR PORQUE TODAS LAS VARIABLES TIENEN LAS MISMAS UNIDADES
summary(fdata.pca)
        #Aqui vemos cuando cada PC explica la variable (de 0 a 1)
        #Para explicar el 95% cogeriamos 3 PC

std_dev <- fdata.pca$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)

#POR COMPONENTE PRINCIPAL CUANTA VARIABILIDAD EXPLICO:
barplot(prop_varex[1:5], xlab = "Principal Component",
        ylab = "Proportion of Variance Explained",
        names.arg=dimnames(fdata.pca$rotation)[[2]][1:5],
        ylim = c(0,1))
lines(cumsum(prop_varex[1:5]), col="blue", lwd = 3)
legend(x=3,y=0.8,legend = "Cumulative Proportion",col = "blue",lty = 1)


#PESOS DE CADA VARIABLE A LOS TRES PRIMEROS PCS
#Plot principal component loadings
par(mfrow=c(3,1))
barplot(fdata.pca$rotation[,1],ylab="PC1") 
        #donde la demanda es mas alta (manana y despues de comer) el peso en el PC1 es mayor
        #el perfil de estos pesos es bastante parecido al que tiene la demanda
        #se podria interpretar como la suma de la demanda
barplot(fdata.pca$rotation[,2],ylab="PC2")
        #el PC2 le da mas importancia a la noche
        #es normal porque los PC son ortogonales y el PC2 intenta recoger cosas que el PC1 no ha recogido
barplot(fdata.pca$rotation[,3],ylab="PC3")
par(mfrow=c(1,1))


#VALORES DE LOS COMPONENTES PRINCIPALES (1,2,3) PARA CADA DIA QUE TENEMOS:
#plot component scores
par(mfrow=c(3,1))
plot(fdata.pca$x[,1],type="l",ylab="PC1", xlab = "t(days)", main = "Time series of principal components")
plot(fdata.pca$x[,2],type="l",ylab="PC2", xlab = "t(days)")
plot(fdata.pca$x[,3],type="l",ylab="PC3", xlab = "t(days)")
par(mfrow=c(1,1))

#RECONSTRRUCCION DE LOS PRIMEROS 4 DIAS TRES PRIMEROS PCS
#Reconstruction of first profile with three components
for(i in 1:4){ #DIAS 1 AL 4
plot(1:24, as.matrix(fdata[i,]),type="l", xlab = "Hours", ylab = "Demand")
fdata.rec <- fdata.pca$center + (fdata.pca$x[i,1:3] %*% t(fdata.pca$rotation[,1:3]))*ifelse(fdata.pca$scale,fdata.pca$scale,1) #el '3' es el numerp de PCs
lines(1:24,fdata.rec[1,],col="red")
legend(x=1,y=28000,legend = c("Real", "Reconstruction"), col = c("black", "red"),lty = 1)
Sys.sleep(0.5)
}

#VALORES DE LOS 2 PRIMEROS PC PARA TODOS LOS DIAS
#plot data in space spanned by first 2 PC
df_plot <- data.frame(PC1 = fdata.pca$x[,1], PC2 =fdata.pca$x[,2], DAY=fdatatot$DAY, MONTH=fdatatot$MONTH)
ggplot(df_plot)+geom_point(aes(x=PC1,y=PC2)) #Vemos varias tendencias
ggplot(df_plot)+geom_point(aes(x=PC1,y=PC2, color=DAY)) #segun el tipo de dia tiene sentido
                #vemos que los findes y festivo tienen demanda mas alta (PC1 mas negativo)
ggplot(df_plot)+geom_point(aes(x=PC1,y=PC2, color=MONTH))
        
        #*ALGO MUY NEGATIVO EN EL PC1 ES QUE TENEMOS MUCHA DEMANDA (por la forma del grafico de los pesos)



