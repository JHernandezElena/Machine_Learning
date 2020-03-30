#################################################################################
##############                 MLP & SVM            ############################
##############     ----------- example ---------    ############################
#################################################################################
##PRIMERO AJUSTAMOS UN MODELO LINEAL PARA TENERLO COMO REF Y VER QUE HAY COSAS NO LINEALES

library(readxl)
library(ggplot2)
library(caret)
library(GGally)
library(splines)
library(NeuralNetTools)
library(gridExtra)
library(MLTools)



maxn = 1000
set.seed(50)
Time = seq(1,maxn)
X1 = rnorm(maxn)
X2 = rnorm(maxn)
X3 = rnorm(maxn)
X4 = rnorm(maxn)
X5 = rnorm(maxn)

y = X1 + X2^2 + X3*X4 + 0.1*rnorm(maxn)

fdata <- data.frame(Time,X1,X2,X3,X4,X5,y)

## Exploratory analysis -------------------------------------------------------------------------------------
ggplot(fdata) + geom_line(aes(x=Time, y=y))
ggpairs(fdata,aes( alpha = 0.01))
  #vemos que entre la x2 e y puede haber una relacion quadratica
  #se percibe una relacion lineal en x1 y la y
  #x5 ningun tipo de relacion
      #lax3 y la x4 son muy parecidas a la x5 con su relacion con y
      #no se ve la relacion entre x3*x4 con la salida. Solo podriamos decir que no hay relacion lineal entre la x3, x4, x5 y la y

## Divide the data into training and validation sets ---------------------------------------------------
set.seed(150) #For replication
ratioTR = 0.8 #Percentage for training
#create random 80/20 % split
trainIndex <- createDataPartition(fdata$y,     #output variable. createDataPartition creates proportional partitions
                                  p = ratioTR, #split probability
                                  list = FALSE, #Avoid output as a list
                                  times = 1) #only one partition
#obtain training and validation sets
fTR = fdata[trainIndex,]
fTS = fdata[-trainIndex,]

#Create index to select input variables
varindex <- variable.names(fdata) != "y"

## Initialize trainControl -----------------------------------------------------------------------
#Use resampling for measuring generalization error
#K-fold with 10 folds
ctrl_tune <- trainControl(method = "cv",                     
                          number = 10,
                          summaryFunction = defaultSummary,    #Performance summary for comparing models in hold-out samples.
                          returnResamp = "final",              #Return final information about resampling
                          savePredictions = TRUE)              #save predictions

##Probamos un modelo de regression lineal
##----------------------------------------------------------------------------------------------------------------
## Linear Regression -------------------------------------------------------------------------------------------
##----------------------------------------------------------------------------------------------------------------
set.seed(150) #For replication
lm.fit = train(fTR[,varindex], 
               y = fTR$y, 
               method = "lm", #Linear model
               preProcess = c("center","scale"),
               trControl = ctrl_tune, 
               metric = "RMSE")
lm.fit #information about the resampling settings
summary(lm.fit)  #information about the model trained
      #esto me dice que la x1 es muy relevante para el modelo ajustado pero el resto no afectan a la salida ya que tienen un p-valor>0.05
      #desde el punto de vista lineal la unica variable relevante es x1

#Evaluate the model with training sets and diagnosis
fTR_eval = fTR
fTR_eval$lm_pred = predict(lm.fit,  newdata = fTR)  
fTS_eval = fTS
fTS_eval$lm_pred = predict(lm.fit,  newdata = fTS)  

PlotModelDiagnosis(fTR[,varindex], fTR$y,
                   fTR_eval$lm_pred, together = TRUE)


##podriamos ajutsar un modelo no lineal: probamos el perceptron multicapa
##-----------------------------------------------------------------------------------------
## MLP -------------------------------------------------------------------------------------------
##-----------------------------------------------------------------------------------------
set.seed(150) #For replication
mlp.fit = train(form = y~.,
                data = fTR, 
                method = "nnet",
                linout = TRUE, #true porque es problema con salida continua, si fuera de clasificacion pondriamos false
                maxit = 250,
                #tuneGrid = data.frame(size = 10, decay = 0),
                tuneGrid = expand.grid(size = seq(5,25,length.out = 5), decay=10^seq(-7,-2, length.out=6)),
                #tuneLength = 5,
                preProcess = c("center","scale"),
                trControl = ctrl_tune, 
                metric = "RMSE")
mlp.fit #information about the resampling settings
ggplot(mlp.fit) + scale_x_log10()
    ##buscamos el minimo rmse en cross-validation
    #el modelo 5 neuronas tiene un problema de sesgo(muy simple), con mas de 10 neuronas sobre-entrena

fTR_eval$mlp_pred = predict(mlp.fit,  newdata = fTR)  
fTS_eval$mlp_pred = predict(mlp.fit,  newdata = fTS)  

SensAnalysisMLP(mlp.fit) #Statistical sensitivity analysis
    #vemos que este modelo no usa ni la variable tiempo ni la x5 (en mean(sens^2))
    #en x1 vemos en density(sense) que x1 tiene una sensibilidad constante en torno a 1->NOS DICE QUE HAY UNA RELACION CONSTANTE/LINEAL CON LA SALIDA

PlotModelDiagnosis(fTR[,varindex], fTR$y, 
                   fTR_eval$mlp_pred, together = TRUE)
    
#Eliminamos el time y la x5
# Input variable selection -----------------------
set.seed(150) #For replication
mlp2.fit = train(form = y~X1+X2+X3+X4,
                data = fTR, 
                method = "nnet",
                linout = TRUE, 
                maxit = 300,
                #tuneGrid = data.frame(size =5, decay = 0),
                tuneGrid = expand.grid(size = seq(5,25,length.out = 5), decay=10^seq(-7,-2, length.out=6)),                #tuneLength = 5,
                preProcess = c("center","scale"),
                trControl = ctrl_tune, 
                metric = "RMSE")
mlp2.fit #information about the resampling settings
ggplot(mlp2.fit) + scale_x_log10()
    #vemos que en vez de la de 15 podriamos usar la de 10 ya que asi es mas sencillo
        #podriamos coger la de 10 y con menos decay (muy parecido a lo que tenemos)
SensAnalysisMLP(mlp2.fit) #Statistical sensitivity analysis

fTR_eval$mlp2_pred = predict(mlp2.fit,  newdata = fTR)  
fTS_eval$mlp2_pred = predict(mlp2.fit,  newdata = fTS)  
PlotModelDiagnosis(fTR[,c("X1","X2","X3","X4")], fTR$y,
                   fTR_eval$mlp2_pred, together = TRUE)

ggplot(fTR_eval) + geom_point(aes(x=y, y=lm_pred, color="lm"))+
  geom_point(aes(x=y, y=mlp2_pred, color="mlp2"))
    #variable real de salida vs prediccion con el modelo lineal y el mlp2 para el conjunto de entrenamiento
    #QUEREMOS UNA RECTA DE PENDIENTE 1, y=x

ggplot(fTS_eval) + geom_point(aes(x=y, y=lm_pred, color="lm"))+
  geom_point(aes(x=y, y=mlp2_pred, color="mlp2"))
    #lo mismo pero para el conjnto de test

########################################################
# Model comparison para los datos de validacion cruzada
#########################################################
transformResults <- resamples(list(lm=lm.fit, mlp=mlp.fit, mlp2=mlp2.fit))
summary(transformResults)
dotplot(transformResults)
    ##cuanto mas estrecha la banda mejor caracterizado tenemos el error



## svm -------------------------------------------------------------------------------------------
library(kernlab)
set.seed(150) #For replication
svm.fit = train(form = y~.,
                data = fTR,
                method = "svmRadial",
                #tuneLength = 5,
                #tuneGrid =  data.frame( sigma=10, C=1),  
                tuneGrid = expand.grid(C = 10^seq(-1,2,length.out = 6), sigma=10^seq(-3,1,length.out=5)),
                preProcess = c("center","scale"),
                trControl = ctrl_tune, 
                metric = "RMSE")
svm.fit #information about the resampling
ggplot(svm.fit) #plot the summary metric as a function of the tuning parameter


# Evaluate training performance -------------------------------------------------------------------------
transformResults <- resamples(list(lm=lm.fit, mlp=mlp.fit, mlp2=mlp2.fit, svm = svm.fit))
summary(transformResults)
dotplot(transformResults)


# Evaluate validation performance -------------------------------------------------------------------------
fTS_eval = fTS
fTS_eval$lm_pred = predict(lm.fit,  newdata = fTS)  
fTS_eval$mlp_pred = predict(mlp.fit,  newdata = fTS) 
fTS_eval$mlp2_pred = predict(mlp2.fit,  newdata = fTS) 
fTS_eval$svm_pred = predict(svm.fit,  newdata = fTS) 


caret::R2(fTS_eval$lm_pred,fTS_eval$y)
caret::R2(fTS_eval$mlp_pred,fTS_eval$y)
caret::R2(fTS_eval$mlp2_pred,fTS_eval$y)
caret::R2(fTS_eval$svm_pred,fTS_eval$y)

caret::RMSE(fTS_eval$lm_pred,fTS_eval$y)
caret::RMSE(fTS_eval$mlp_pred,fTS_eval$y)
caret::RMSE(fTS_eval$mlp2_pred,fTS_eval$y)
caret::RMSE(fTS_eval$svm_pred,fTS_eval$y)

ggplot(fTS_eval) + geom_point(aes(x=y, y=lm_pred, color="lm"))+
  geom_point(aes(x=y, y=mlp2_pred, color="mlp2"))+
  geom_point(aes(x=y, y=svm_pred, color="svm"))


# Input variable selection -----------------------
set.seed(150) #For replication
svm2.fit = train(form = y~X1+X2+X3+X4,
                 data = fTR,
                 method = "svmRadial",
                 #tuneLength = 5,
                 #tuneGrid =  data.frame( sigma=10, C=1),  
                 tuneGrid = expand.grid(C = 10^seq(-1,2,length.out = 6), sigma=10^seq(-3,1,length.out=5)),
                 preProcess = c("center","scale"),
                 trControl = ctrl_tune, 
                 metric = "RMSE")
svm2.fit #information about the resampling
ggplot(svm2.fit) #plot the summary metric as a function of the tuning parameter


fTS_eval$svm2_pred = predict(svm2.fit,  newdata = fTS) 

# Evaluate training performance -------------------------------------------------------------------------
transformResults <- resamples(list(lm=lm.fit, mlp=mlp.fit, mlp2=mlp2.fit, svm = svm.fit, svm2=svm2.fit))
summary(transformResults)
dotplot(transformResults)

caret::R2(fTS_eval$lm_pred,fTS_eval$y)
caret::R2(fTS_eval$mlp_pred,fTS_eval$y)
caret::R2(fTS_eval$mlp2_pred,fTS_eval$y)
caret::R2(fTS_eval$svm_pred,fTS_eval$y)
caret::R2(fTS_eval$svm2_pred,fTS_eval$y)

caret::RMSE(fTS_eval$lm_pred,fTS_eval$y)
caret::RMSE(fTS_eval$mlp_pred,fTS_eval$y)
caret::RMSE(fTS_eval$mlp2_pred,fTS_eval$y)
caret::RMSE(fTS_eval$svm_pred,fTS_eval$y)
caret::RMSE(fTS_eval$svm2_pred,fTS_eval$y)




###ELIMINAR ATIPICOS (APARTE RECORDADO DEL HACKATON)
oulierRes <- abs(fTR$WG-fTR_eval$gam_pred)<100
oulierRes2 <- abs(fTS$WG-fTS_eval$gam_pred)<100
fTRFilter<-fTR[oulierRes,]
fTSFilter<-fTS[oulierRes2,]
#A PARTIR DE QUI HAY QUE USAR FTRFILTER
