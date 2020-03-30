################################################################################
##############    Classification: Logistic Regression     ######################
################################################################################


## Load libraries --------------------------------------------------------------------------------
library(caret)
library(ggplot2)
library(ROCR) #for plotting ROC curves.
#########################################################
## load MLTools package. It contains useful functions for Machine Learning Course.
#install the package only the first time to be used. Set working directory to library folder
install.packages("MLTools_0.0.24.tar.gz", repos = NULL)
#load package and install dependencies only the first time the package has been installed.
library(MLTools)
install.dependencies()
#From now on, only library(MLTools) should be called to load the package
#########################################################

## Set working directory -------------------------------------------------------------------------


## Load file -------------------------------------------------------------------------------------
fdata <- read.table("SimData.dat", sep = "", header = TRUE, stringsAsFactors = FALSE)
str(fdata)
#Convert output variable to factor
fdata$Y <- as.factor(fdata$Y)
str(fdata)


## Exploratory analysis -------------------------------------------------------------------------------------
ggplot(fdata) + geom_point(aes(x = X1, y = X2, color = Y))


#For datasets with more than two inputs
library(GGally)
ggpairs(fdata,aes(color = Y, alpha = 0.3))
#Function for plotting multiple plots between the output of a data frame and the predictors of this output
#En el YY vemos que los datos estan balanceados, hay el mismo numero
PlotDataframe(fdata = fdata, 
              output.name = "Y")


## Divide the data into training and test sets ---------------------------------------------------
set.seed(150) #For replication
#create random 80/20 % split = DE LA LIBRERIA DE CARET
#con esta funcion aunque tengamos unbalace data se asegura de que haya los mismos porcentajes
trainIndex <- createDataPartition(fdata$Y,      #output variable. createDataPartition creates proportional partitions
                                  p = 0.8,      #porcentaje de datos que quiero usar para el entren
                                  list = FALSE, #Avoid output as a list
                                  times = 1)    #only one partition
#obtain training and test sets
fTR <- fdata[trainIndex,]
fTS <- fdata[-trainIndex,]
  

#plot training and test sets. 
#Try changing the value inside set.seed(). How does it affect the selected data?
ggplot(fTR) + geom_point(aes(x = X1, y = X2, color = Y))
ggplot(fTS) + geom_point(aes(x = X1, y = X2, color = Y)) #vemos que hay un hueco=problemilla


## Initialize trainControl (funcion de la libreria de caret) -----------------------------------------------------------------------
ctrl <- trainControl(method = "cv",                        #k-fold cross-validation
                     number = 10,                          #Number of folds
                     summaryFunction = defaultSummary,     #Performance summary for comparing models in hold-out samples.
                     classProbs = TRUE)                    #Compute class probs in Hold-out samples
 


#-------------------------------------------------------------------------------------------------
#---------------------------- LOGISTIC REGRESSION MODEL ----------------------------------------------
#-------------------------------------------------------------------------------------------------
## Train model -----------------------------------------------------------------------------------
set.seed(150) #For replication
#Train model using training data
LogReg.fit <- train(form = Y ~ ., #formula for specifying inputs and outputs (variable de salida~de que variables depende) (al poner un . coge todas las del conjunto de datos menos la Y)
                   data = fTR,               #Training dataset 
                   method = "glm",                   #Train logistic regression
                   preProcess = c("center","scale"), #preprocesamos: Center an scale inputs
                   trControl = ctrl,                 #trainControl Object (configuracion de la validacion cruzada)
                   metric = "Accuracy")              #summary metric used for selecting hyperparameters
LogReg.fit          #information about the resampling
summary(LogReg.fit) #detailed information about the fit of the final model 
                    #La tabla de coeficientes son los W0 e W1 del modelo
                    #Nos interesa el Pr(>|z|) de la tabla que es p-valor=probabilidad de que el valor estimado sea 0 (cuanto mas grande menos importante es es numero)
                    #por tanto la variable x2 es muy significativa pero la variable x1 tiene una probabilidad del 9% de que su coeficiente W sea 0 = casi lo podriamos descartar
str(LogReg.fit)     #all information stored in the model


## Understanding resampling methods -------------------------------------------
str(LogReg.fit$control$index)       #Training indexes
str(LogReg.fit$control$indexOut)    #Test indexes (los conjuntos de validacion son disjuntos!!)
LogReg.fit$resample                 #Resample test results (porcentajes de acierto que da con los distintos folds)
                                    #La media de todos los accuracys es el cross validation accuracy = medida para ver si el modelo es bueno  o malo
boxplot(LogReg.fit$resample$Accuracy, xlab = "Accuracy")
title("Boxplot for summary metrics of test samples")


## Evaluate model --------------------------------------------------------------------------------
#Evaluate the model with training and test sets
#training
fTR_eval <- fTR #hacemos esto para almacenar las predicciones
fTR_eval$LRprob <- predict(LogReg.fit, type="prob", newdata = fTR) # predict probabilities
fTR_eval$LRpred <- predict(LogReg.fit, type="raw", newdata = fTR) # predict classes 
#test
fTS_eval <- fTS
fTS_eval$LRprob <- predict(LogReg.fit, type="prob", newdata = fTS) # predict probabilities
fTS_eval$LRpred <- predict(LogReg.fit, type="raw", newdata = fTS) # predict classes 

#Plot predictions of the model
ggplot(fTR_eval) + geom_point(aes(x = X1, y = X2, color = LRpred)) + labs(title = "Predictions for training data")
ggplot(fTS_eval) + geom_point(aes(x = X1, y = X2, color = LRpred)) + labs(title = "Predictions for test data")


#Plot classification in a 2 dimensional space
Plot2DClass(fTR[,1:2], #Input variables of the model
            fTR$Y,     #Output variable
            LogReg.fit,#Fitted model with caret
            var1 = "X1", var2 = "X2", #variables that define x and y axis
            selClass = "YES")     #Class output to be analyzed 
    #aqui en classidication de input space podemos ver que la varaible x2 es MUY SIGNIFICATIVA COMO HABIAMOS VISTO ANTES
        #Tambien vemos que el valor de x1 no es nada significativo


## Performance measures --------------------------------------------------------------------------------

#######confusion matices
# Training
confusionMatrix(data = fTR_eval$LRpred, #Predicted classes
                reference = fTR_eval$Y, #Real observations
                positive = "YES") #Class labeled as Positive
    #da la tablita con la prediccion vs la referencia
    #precision = % de acierto
    #no information rate = como tenemos 50% de cada variable =0.5 (si fuera 80/20 nos daria un 0.8)
        #comparado con el accuracy, da una buena medicion de lo bueno que es el modelo

# test
confusionMatrix(fTS_eval$LRpred, 
                fTS_eval$Y, 
                positive = "YES")

#######Classification performance plots 
# Training
PlotClassPerformance(fTR_eval$Y,       #Real observations
                     fTR_eval$LRprob,  #predicted probabilities
                     selClass = "YES") #Class to be analyzed
# test
PlotClassPerformance(fTS_eval$Y,       #Real observations
                     fTS_eval$LRprob,  #predicted probabilities
                     selClass = "YES") #Class to be analyzed)

    #nos da: el calibration point, 
    #probability histograms
    #ROC curve
    #accuracy per cutoff = IGUAL NOS CONVIENE AJUSTAR EL CUTOFF VALUE (por ejemplo para asegurarnos que todos los enfermos si que es diganos que si, aunque muchis no enfermos tb les digamos que si)


#ANADIR VARIABLES AL MODELO
###################################################################################
## include X1 variable squared ####################################################
###################################################################################
fdata$X1sq <- fdata$X1^2 #SE VEIA CLARAMENTE QUE ENTRE LA X1 Y LA Y HABIA UNA RELACION CUADRATICA)
#obtain training and test sets
fTR <- fdata[trainIndex,]
fTS <- fdata[-trainIndex,]
## Train model
set.seed(150) #For replication
#Train model using training data
LogReg2.fit <- train(form = Y ~ X1sq + X2, #formula for specifying inputs and outputs.
                    data = fTR,               #Training dataset 
                    method = "glm",                   #Train logistic regression
                    preProcess = c("center","scale"), #Center an scale inputs
                    trControl = ctrl,                 #trainControl Object
                    metric = "Accuracy")              #summary metric used for selecting hyperparameters
LogReg2.fit          #information about the resampling
summary(LogReg2.fit) #detailed information about the fit of the final model

## Evaluate model --------------------------------------------------------------------------------
#Evaluate the model with training and test sets
#training
#fTR_eval <- fTR
fTR_eval$LRprob2 <- predict(LogReg2.fit, type="prob" , newdata = fTR) # predict probabilities
fTR_eval$LRpred2 <- predict(LogReg2.fit, type="raw" , newdata = fTR) # predict classes 
#test
#fTS_eval <- fTS
fTS_eval$LRprob2 <- predict(LogReg2.fit, type="prob" , newdata = fTS) # predict probabilities
fTS_eval$LRpred2 <- predict(LogReg2.fit, type="raw" , newdata = fTS) # predict classes 


#Plot classification results in input variables
Plot2DClass(fTR[,c("X1sq","X2")], #Input variables 
            fTR$Y,     #Output variable
            LogReg2.fit,#Fitted model with caret
            var1 = "X1sq", var2 = "X2", #variables to represent the plot
            selClass = "YES")     #Class output to be analyzed 


#plot in X1 space
#Plot classification results in input variables
Plot2DClass(fTR, #Input variables 
            fTR$Y,     #Output variable
            LogReg2.fit,#Fitted model with caret
            var1 = "X1", var2 = "X2", #variables to represent the plot
            selClass = "YES")     #Class output to be analyzed 



#-------------------------------------------------------------------------------------------------
#---------------------- COMPARATIVE ANALYSIS ----------------------------------------------
#-------------------------------------------------------------------------------------------------

## comparison of models in training and test set --------------------------------------------------------
#TRAINING
#resampling summary metric
transformResults <- resamples(list(lr = LogReg.fit, lr2 = LogReg2.fit))
summary(transformResults)
dotplot(transformResults)

#Overall accuracy
confusionMatrix(fTR_eval$LRpred, fTR_eval$Y, positive = "YES")$overall[1]
confusionMatrix(fTR_eval$LRpred2, fTR_eval$Y, positive = "YES")$overall[1]


#ROC curve
library(pROC)
reducedRoc <- roc(response = fTR_eval$Y, fTR_eval$LRprob$YES)
plot(reducedRoc, col="black")
auc(reducedRoc)
reducedRoc <- roc(response = fTR_eval$Y, fTR_eval$LRprob2$YES)
plot(reducedRoc, add=TRUE, col="red")
auc(reducedRoc)
legend("bottomright", legend=c("LR", "LR2"), col=c("black", "red"), lwd=2)



#TEST
#Overall accuracy
confusionMatrix(fTS_eval$LRpred, fTS_eval$Y, positive = "YES")$overall[1]
confusionMatrix(fTS_eval$LRpred2, fTS_eval$Y, positive = "YES")$overall[1]

#Calibration curve     
calPlotData <- calibration(Y ~ LRprob$YES+LRprob2$YES, data =fTS_eval, class = "YES",cuts = 6)
xyplot(calPlotData, auto.key = list(columns = 2))

#ROC curve
library(pROC)
reducedRoc <- roc(response = fTS_eval$Y, fTS_eval$LRprob$YES)
plot(reducedRoc, col="black")
auc(reducedRoc)
reducedRoc <- roc(response = fTS_eval$Y, fTS_eval$LRprob2$YES)
plot(reducedRoc, add=TRUE, col="red")
auc(reducedRoc)
legend("bottomright", legend=c("LR", "LR2"), col=c("black", "red"), lwd=2)

