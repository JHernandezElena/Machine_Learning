#################################################################################
##############       LabPractice 3.1 Regression     ############################
############## ELEGIR LAS VARIABLES QUE MAS IMPORTAN  ############################
#################################################################################


#p-valores pequenos los quitamos
#VIF>10 los quitamos (mirar los p-valores!! mejor eliminar una que este relacionada linealmente con otra que esa otra)

## Set working directory -------------------------------------------------------------------------

## Load libraries --------------------------------------------------------------------------------
library(caret)
library(ggplot2)
library(GGally)
library(leaps)
library(glmnet)
library(pls)
library(car)
library(corrplot)
library(dplyr) # select, grouping levels in factor
library(MLTools)

## remove all epxisting variables--------------------------------------------------
rm(list=ls())
   

## LoadData 
fdataINI <- read.csv("TemperaturesSpainSmall.csv", sep = ";")
fdataTOT <- fdataINI[1:1000,] #Select 1000 for class purposes
fdataTOT = na.omit(fdataTOT) #Eliminate NA

# Contents of the dataset
str(fdataTOT)

# Output variable (TMIN MADRID-RETIRO)
summary(fdataTOT$TMIN230)
hist(fdataTOT$TMIN230, nclass = 40)

# Subsets variables
fdataTmax = select(fdataTOT, "WEEKDAY","MONTH", starts_with("TMAX"))
str(fdataTmax)
fdata = select(fdataTOT, "WEEKDAY","MONTH","TMIN230","TMAX230","TMAX229","TMAX237",
                "TMAX417","TMAX2969","TMAX3910","TMAX3918","TMAX3959")
str(fdata)

#correlation plot of numeric variables
numvars <- sapply(fdata, class) %in% c("integer","numeric")
C <- cor(fdata[,numvars])
corrplot(C, method = "circle")

## Exploratory analysis -------------------------------------------------------------------------------------
ggpairs(fdata,aes( alpha = 0.3))

## Filter outliers
fdata$TMAX417[fdata$TMAX417>35] = NA
fdata = na.omit(fdata) #Eliminate NA, i.e. the outliers





## Divide the data into training and validation sets ---------------------------------------------------
set.seed(150) #For replication
#create random 80/20 % split
trainIndex <- createDataPartition(fdata$TMIN230,      #output variable. createDataPartition creates proportional partitions
                                  p = 0.8,      #split probability for training
                                  list = FALSE, #Avoid output as a list
                                  times = 1)    #only one partition
#obtain training and validation sets
fTR <- fdata[trainIndex,]
fTR_eval <- fTR #
fTS <- fdata[-trainIndex,]


## Initialize trainControl (FOR ALL MODELS) -----------------------------------------------------------------------
#Use resampling for measuring generalization error
#K-fold with 10 folds
ctrl_tune <- trainControl(method = "cv",                     
                          number = 10,
                          summaryFunction = defaultSummary,    #Performance summary for comparing models in hold-out samples.
                          returnResamp = "final",              #Return final information about resampling
                          savePredictions = TRUE)              #save predictions




#################################################################################################
###################### MODEL SELECTION ################################################
#################################################################################################

#Convert month to factor
fdata$MONTH <- as.factor(fdata$MONTH)

#==================================================================
## FITH model for 230	MADRID - RETIRO (Collinearity ??) 
# 229	  BADAJOZ/TALAVERA LA REAL
# 237	  VALENCIA
# 417	  GRANADA
# 2969	BARCELONA/AEROPUERTO
# 3910	GIJON
# 3918	PALMA DE MALLORCA CMT
# 3959	STA. CRUZ DE TENERIFE
#==================================================================
set.seed(150) #For replication
lm5.fit = train(form = TMIN230 ~ .,
               data = fTR, 
               method = "lm", #Linear model
               #tuneGrid = data.frame(intercept = TRUE), 
               preProcess = c("center","scale"),
               trControl = ctrl_tune, 
               metric = "RMSE")
lm5.fit #information about the resampling settings
summary(lm5.fit)  #information about the model trained
    #aqui vemos los p-valores (pequenos = significativas)
    #pvalues suggest removing ...


#Identify correlated variables MULTICOLINEARIDAD DE LAS VARIABLES
    #Usamos cada variable de input y la intentamos explicar con el resto de manera lineal para ver como se comporta
vif(lm5.fit$finalModel)
  #Remove one by one correlated variables until VIF<10 for all
  #VIF MUY GRANDE -> eliminamos


#==================================================================
## SIXTH model for 230	MADRID - RETIRO (Removing la variable TMAX230 que tiene VIF muy alto) 
#==================================================================
set.seed(150) #For replication
lm6.fit = train(form = TMIN230 ~ . -TMAX230,
                data = fTR, 
                method = "lm", #Linear model
                #tuneGrid = data.frame(intercept = TRUE), 
                preProcess = c("center","scale"),
                trControl = ctrl_tune, 
                metric = "RMSE")
lm6.fit #information about the resampling settings
summary(lm6.fit)  #information about the model trained

# pvalues suggest removing ...
#Identify correlated variables
vif(lm6.fit$finalModel)



##############################################################
###### TECNICAS DE SELECCION DE VARIABLES AUTOMATICA!!! ######
##############################################################


##CROSS-VAL SELECTION
#============================================================================================
## SEVENTH model for 230	MADRID - RETIRO (Removing irrelevant + collinearity AUTOMATICALLY) 
## SELECCION DE VARIABLES USANDO CROSS VALIDATION
    #empiza por 10 y mira cual interesa menos y la quita
    #de las 9 que queda vuelve a ver y quita otra
    #esto puede quitar alguna variable que combinada con otra fuera muy relevante
#=============================================================================================
set.seed(150) #For replication

##PARA AJUSTAR HIPERPARAMETROS DE CADA MODELO
#The control function is similar to trainControl()
ctrl_none <- trainControl(method = "none",       #ESTE MODELO NO TIENE HIPERPARAMETROS por eso no necesitamos CV              
                          summaryFunction = defaultSummary,    #Performance summary for comparing models in hold-out samples.
                          returnResamp = "final",              #Return final information about resampling
                          savePredictions = TRUE)              #save predictions

## Specifies the cross validation method used for selecting the NUMERO OPTION DE VARIABLES
ctrl_rfe <- rfeControl(method = "cv",
                       number = 10,
                       verbose = TRUE,
                       functions = caretFuncs)

## rfe() function instead of train
set.seed(150)
subsets <- 1:20 #The number of features that should be retained (all, >7)
lm7.RFE <- rfe(form = TMIN230 ~ ., #nota: tenemos 10 variables de entrada
              data = fTR, 
              ### Arguments passed to train() function (lo normal de siempre)
              method = "lm",
              preProcess = c("center","scale"),
              trControl = ctrl_none, #como la regresion lineal no tiene ningun hyperparametro por lo que nuestro control no necesita cv
              ### Arguments for rfe (PARAMETROS QUE VAN A CONTROLAR LA SELECCION DE VARIABLES)
              sizes = subsets,
              metric = "RMSE",
              rfeControl = ctrl_rfe)

lm7.RFE # Cross validation results and variable selection

ggplot(lm7.RFE,metric = "RMSE")
  ##atencion que igual con 4 variables tb esta bastante bien! - un pelin mas de error pero mas simple
  ##podrias en en train del modelo sizes = 4

lm7.RFE$fit #Final caret train() object
lm7.RFE$fit$finalModel #Final model trained

vif(lm7.RFE$fit$finalModel)



##RIDGE REG
#==========================================================================================
## EIGHTH model for TMIN230 USING RIDGE REGULARIZATION 
  ##Metodo de Srinking que encoge los coeficcientes de las variables (no llegan a cero)
  ##Mejor LASSO que si elimina variables
#==========================================================================================
set.seed(150) #For replication

#With categorical variables, formula method should be used
lm8.fit = train(form = TMIN230 ~ .,
                  data = fTR, 
                  method = "glmnet",
                  nlambda = 500, # Number of lambda values for glmnet function 
                  tuneGrid = expand.grid(
                    lambda = 2*10^seq(-2,2, length =20), 
                    alpha = 0),  # <- RIDGE!!!!! vs LASSO (1)
                  preProcess = c("center","scale"),
                  trControl = ctrl_tune, 
                  metric = "RMSE")
lm8.fit #information about the resampling
ggplot(lm8.fit)+scale_x_log10()
    #regularization parameter (LAMBDA) cuanto MAS GRANDE->MAS SIMPLE EL MODELO

coef(lm8.fit$finalModel)  #information about the model trained

#Plot the evolution of the coefficients as a function of lambda
plot(lm8.fit$finalModel, xvar = "lambda")
    #para lambda bajo los coeficientes son grandes
    #cuanto mas grande es lambda, los coeficientes tienden mas a cero

#Coefs for high value of lambda
lm8.fit$finalModel$lambda[3]
coef(lm8.fit$finalModel)[,3]
#Coefs for low value of lambda
lm8.fit$finalModel$lambda[500]
coef(lm8.fit$finalModel)[,500]


##LASSO REG
#==================================================================
## EIGHTH model for TMIN230 USING LASSO REGULARIZATION
      #Lasso srinking que ELIMINA VARIABLES

##MEJOR MODELO LINEAL CON LA MEJOR SELECCION DE VARIABLES
#==================================================================
set.seed(150) #For replication
lm9.fit = train(form = TMIN230 ~ .,
                data = fTR, 
                method = "glmnet",
                nlambda = 500, # Number of lambda values for glmnet function 
                tuneGrid = expand.grid(
                  lambda = 2*10^seq(-2,0, length =20), 
                  alpha = 1), #LASSO!!!
                #tuneGrid = data.frame( lambda = 0.1,  alpha = 1), #Selecting one lambda value
                preProcess = c("center","scale"),
                trControl = ctrl_tune, 
                metric = "RMSE")
lm9.fit #information about the resampling
ggplot(lm9.fit)+scale_x_log10()

coef(lm9.fit$finalModel)  #information about the model trained
#Plot the evolution of the coefficients as a function of lambda

plot(lm9.fit$finalModel, xvar = "lambda")
    #para lambda bajo los coeficientes son grandes
    #cuanto mas grande es lambda, algunos de los coeficientes SON 0

#Coefs for high value of lambda
lm9.fit$finalModel$lambda[3]
coef(lm9.fit$finalModel)[,3]
#Coefs for low value of lambda
lm9.fit$finalModel$lambda[200]
coef(lm9.fit$finalModel)[,200]

#For lambda = 0.1
lm9.fit$finalModel$lambda[227]
coef(lm9.fit$finalModel)[,200]



#-------------------------------------------------------------------------------------------------
#--------------------------- Dimension reduction method ------------------------------------------
#-------------------------------------------------------------------------------------------------

###########################################################
#######Principal component regression
###########################################################
set.seed(150) #For replication
#With categorical variables, formula method should be used
pcr.fit = train(form = TMIN230 ~ .,
                data = fTR, 
                method = "pcr",
                tuneGrid = data.frame(ncomp = 1:(ncol(fTR)-1)),  #Numero de componentes (entradas) que queremos ajustar
                preProcess = c("center","scale"),
                trControl = ctrl_tune, 
                metric = "RMSE")
pcr.fit #information about the resampling
ggplot(pcr.fit)
#Variance explained
summary(pcr.fit$finalModel)
plot(pcr.fit$finalModel$projection)
pcr.fit$finalModel$loadings
#plot loadings of component 1
dataplot = data.frame(x = rownames(pcr.fit$finalModel$loadings), y =pcr.fit$finalModel$loadings[,1])
ggplot(dataplot)+ geom_col(aes(x=x,y=y))


###########################################################
#######Partial least squares regression
###########################################################
set.seed(150) #For replication
#With categorical variables, formula method should be used
plsr.fit = train(form = TMIN230 ~ .,
                 data = fTR, 
                 method = "pls",
                 tuneGrid = data.frame(ncomp = 1:(ncol(fTR)-1)),
                 preProcess = c("center","scale"),
                 trControl = ctrl_tune, 
                 metric = "RMSE")
plsr.fit #information about the resampling
ggplot(plsr.fit)+scale_x_log10()
summary(plsr.fit$finalModel)



#-------------------------------------------------------------------------------------------------
#--------------------------- Training results ------------------------------------------
#-------------------------------------------------------------------------------------------------
transformResults <- resamples(list(
  lm5.ALL = lm5.fit,
  lm6.BYHAND=lm6.fit,
  lm7.RFE=lm7.RFE,
  lm8.RIDGE=lm8.fit,
  lm9.LASSO = lm9.fit,
  pcr.fit = pcr.fit,
  plsr.fit = plsr.fit))
summary(transformResults)
dotplot(transformResults)


