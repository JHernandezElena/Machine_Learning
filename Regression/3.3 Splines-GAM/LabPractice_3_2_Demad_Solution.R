#################################################################################
##############       LabPractice 3.3 Splines/GAM    ############################
##############     ----------- solution ---------   ############################
#################################################################################

## Set working directory -------------------------------------------------------------------------

## Load libraries --------------------------------------------------------------------------------
library(caret)
library(ggplot2)
library(GGally)
library(gam)
library(splines)
library(corrplot)
library(MLTools)
library(readxl)

## LoadData 
fdata <- read.csv("DAILY_DEMAND.csv", sep = ";")
    #LA FEHCA NOS DA IGUAL
    #DEM(demanda electrica en espana) ES NUESTRA Y
#Plot relation between temperature and demand
ggplot(fdata)+geom_point(aes(x=TEMP, y=DEM))
#We can see that there is a non-linear relation between them. (AL MENOS CUADRATICO)


# Set variable types -----------------------------------------
fdata$WD = as.factor(fdata$WD);
fdata$fecha <- NULL
str(fdata)

## Exploratory analysis -------------------------------------------------------------------------------------
PlotDataframe(fdata, output.name = "DEM")
    #el finde baja la demanda
    #el domingo cierran mas tiendas
    #vemos los outliers en DEM vs WD
        #en dias laborables hay muchos outliers con demandas bajas = festivos

#correlation plot of numeric variables
numvars <- sapply(fdata, class) %in% c("integer","numeric")
C <- cor(fdata[,numvars])
corrplot(C, method = "number")
    #La correlacion es baja
    #La correlacion es negativo (sube la temperatura baja la demanda)


## Divide the data into training and validation sets ---------------------------------------------------
set.seed(150) #For replication
ratioTR = 0.8 #Percentage for training
#create random 80/20 % split
trainIndex <- createDataPartition(fdata$DEM,     #output variable. createDataPartition creates proportional partitions
                                  p = ratioTR, #split probability
                                  list = FALSE, #Avoid output as a list
                                  times = 1) #only one partition
#obtain training and validation sets
fTR = fdata[trainIndex,]
fTS = fdata[-trainIndex,]


## Initialize trainControl -----------------------------------------------------------------------
#Use resampling for measuring generalization error
#K-fold with 10 folds
ctrl_tune <- trainControl(method = "cv",                     
                          number = 10,
                          summaryFunction = defaultSummary,    #Performance summary for comparing models in hold-out samples.
                          returnResamp = "final",              #Return final information about resampling
                          savePredictions = TRUE)              #save predictions

## Linear Regression -------------------------------------------------------------------------------------------
set.seed(150) #For replication
lm.fit = train(form = DEM ~ WD + TEMP, 
               data = fTR, 
               method = "lm", #Linear model
               preProcess = c("center","scale"),
               trControl = ctrl_tune, 
               metric = "RMSE")
lm.fit #information about the resampling settings
    
summary(lm.fit)  #information about the model trained
    #los viernes quiza los podriamos quitar
    #R2 muy bajo


#Evaluate the model with training sets and diagnosis
fTR_eval = fTR
fTR_eval$lm_pred = predict(lm.fit,  newdata = fTR)  


PlotModelDiagnosis(fTR, fTR$DEM, 
                   fTR_eval$lm_pred,
                   together = TRUE)
#Non-linear effect seen in TEMP

#Relation between DEM and TEMP
ggplot(fTR_eval)+geom_point(aes(x=TEMP, y = DEM))+geom_point(aes(x=TEMP, y = lm_pred,  color=WD))
    #por muchas rectas que metamos para cada dia no vamos a explicar una relacion cuadratica



#-------------------------------------------------------------------------------------------------
#----------------------- polynomial regression  ------------------------------------------------
#-------------------------------------------------------------------------------------------------
set.seed(150) #For replication
poly.fit = train(form = DEM ~ WD + poly(TEMP, degree = 2, raw = TRUE),
               data = fTR, 
               method = "lm", #Linear model
               preProcess = c("center","scale"),
               trControl = ctrl_tune, 
               metric = "RMSE")
poly.fit #information about the resampling settings
summary(poly.fit)  #information about the model trained


#Evaluate the model with training sets and diagnosis
fTR_eval$poly_pred = predict(poly.fit,  newdata = fTR)  


PlotModelDiagnosis(fTR, fTR$DEM, 
                   fTR_eval$poly_pred,
                   together = TRUE)
#Non-linear effect seen in TEMP

#Relation between DEM and TEMP
ggplot(fTR_eval)+geom_point(aes(x=TEMP, y = DEM))+geom_point(aes(x=TEMP, y = poly_pred,  color=WD))
    #es mejor, pero no es exactamente cuadratico




#-------------------------------------------------------------------------------------------------
#----------------------- GAM with splines   ------------------------------------------------
#-------------------------------------------------------------------------------------------------

set.seed(150) #For replication
gam.fit = train(form = DEM ~ WD + TEMP,
                data = fTR, 
                method = "gamSpline",
                tuneGrid = data.frame(df = seq(2,10,2)),  ##grados de libertad=como de suave sera el spline que ajustamos
                preProcess = c("center","scale"),
                trControl = ctrl_tune, 
                metric = "RMSE")
gam.fit #information about the resampling settings
ggplot(gam.fit)
summary(gam.fit)  #information about the model trained
    #la s se refiere a spline con un p-valor <2.2e-16 (MUY SIGNIFICATIVO!)
#Plot the fitted splines
#Careful, if library "car" is loaded, this plot gives an error
plot(gam.fit$finalModel, se=TRUE ,col ="blue ")


#Evaluate the model with training sets and diagnosis
fTR_eval$gam_pred = predict(gam.fit,  newdata = fTR)  
PlotModelDiagnosis(fTR, fTR$DEM, 
                   fTR_eval$gam_pred,
                   together = TRUE)
    #mejoran muchos los residuals 
    #residuals vs DEM no hay mas que pueda hacer (hay alguna variable que me falta)
    #residuals vs TEMP vemos un monton de outliers
#EXPLICACION (de oro ejercicio)
#*residuos vs variables explicativas estan mas o menos centrados en cero = las no-linealidades han sido modeladas
#*residuals vs output variable: los residuos ESTAN CORRELADOS CON LA SALIDA YA QUE NO ESTA CENTRADO EN 0

#Relation between DEM and TEMP
ggplot(fTR_eval)+geom_point(aes(x=TEMP, y = DEM))+geom_point(aes(x=TEMP, y = gam_pred,  color=WD))




#-------------------------------------------------------------------------------------------------
#----------------------- GAM with splines & Holidays   ------------------------------------------------
#-------------------------------------------------------------------------------------------------
# POSSIBLE IMPROVEMENTS
# - Holidays (How?)-Le decimos que los festivos son DOMINGOS!!
fTR$WD[(fTR_eval$DEM-fTR_eval$gam_pred)<(-80)] <- 7

set.seed(150) #For replication
gam.fit = train(form = DEM ~ WD + TEMP,
                data = fTR, 
                method = "gamSpline",
                tuneGrid = data.frame(df = seq(2,10,2)),
                preProcess = c("center","scale"),
                trControl = ctrl_tune, 
                metric = "RMSE")
gam.fit #information about the resampling settings
ggplot(gam.fit)
summary(gam.fit)  #information about the model trained
#Plot the fitted splines
#Careful, if library "car" is loaded, this plot gives an error
plot(gam.fit$finalModel, se=TRUE ,col ="blue ")


#Evaluate the model with training sets and diagnosis
fTR_eval$gam_pred = predict(gam.fit,  newdata = fTR)  
PlotModelDiagnosis(fTR, fTR$DEM, 
                   fTR_eval$gam_pred,
                   together = TRUE)

#Relation between DEM and TEMP
ggplot(fTR_eval)+geom_point(aes(x=TEMP, y = DEM))+geom_point(aes(x=TEMP, y = gam_pred,  color=WD))

