#################################################################################
##############      Classification : kNN + DA       #############################
#################################################################################

## Set working directory -------------------------------------------------------------------------

## Load libraries --------------------------------------------------------------------------------
library(caret)
library(ggplot2)
library(ROCR)
library(pROC) #For function roc()
library(MLTools)


## Load file -------------------------------------------------------------------------------------
fdata <- read.table("SimData.dat", sep = "", header = TRUE, stringsAsFactors = FALSE)
str(fdata)
#Convert output variable to factor
fdata$Y <- as.factor(fdata$Y)
str(fdata)


## Exploratory analysis -------------------------------------------------------------------------------------
ggplot(fdata) + geom_point(aes(x = X1, y = X2, color = Y))

#For datasets with more than two inputs
#Function for plotting multiple plots between the output of a data frame and the predictors of this output.
PlotDataframe(fdata = fdata, 
              output.name = "Y")
          #Aqui vemos que la x1 no nos da mucha info de que tipo somos
          #La x2 vemos que si estamos por encima de 5 seremos seguramente rojo



## Divide the data into training and test sets ---------------------------------------------------
set.seed(150) #For replication IMPORTANTE!
#create random 80/20 % split
trainIndex <- createDataPartition(fdata$Y,      #output variable. createDataPartition creates proportional partitions
                                  p = 0.8,      #split probability for training
                                  list = FALSE, #Avoid output as a list
                                  times = 1)    #only one partition
#obtain training and test sets
fTR <- fdata[trainIndex,]  #De mil datos 800 para entrenar (los que conozco) y 200 para test (no sabemos que son)
fTS <- fdata[-trainIndex,]


## Initialize trainControl -----------------------------------------------------------------------
ctrl <- trainControl(method = "cv",                        #k-fold cross-validation
                     number = 10,                          #Number of folds y porcentaje para validacion (90-10)
                     summaryFunction = defaultSummary,     #Performance summary for comparing models in hold-out samples.
                                                           #Aqui especificas que tipo de medidas quieres comprobar (kappa, area bajo la curva, accuracy...) - NO CAMBIAR
                     classProbs = TRUE)                    #Compute class probs in Hold-out samples NO NOS INTERESA


          #de los 800 datos dejamos unos cuantos para validar y hacemos diferent folds
              #validar=hacer test pero sabiendo los resultados -> still es entrenamiento




#-------------------------------------------------------------------------------------------------
#-------------------LINEAR DISCRIMINANT ANALISIS -------------------------------------------------
#-------------------------------------------------------------------------------------------------
## Va a intentar hacer un corte lineal para separar estos datos
## De ante mano sabemos que esto no va a ser demasiado bueno
## Minimo debemos esperar un 50% de accuracy ya que esa es la distribucion de los datos y es lo que
            #obtendriamos al azar
#-------------------------------------------------------------------------------------------------

## Train LDA ------------------------------------------------------------------
set.seed(150) #For replication
#Train model using training data
lda.fit = train(form = Y ~ ., #formula for specifying inputs and outputs(usando "form" las variables de etrada de 
                       #tipo factor se convierten en variables dummy 0, 1 como el ejemplo del color de pelo)
                       #NO HACER PARA DECISION TREE
                data = fTR,     #Training dataset 
                method = "lda",  # Linear Discriminant Analysis
                preProcess = c("center","scale"), #Preprocesado de Center and scale
                trControl = ctrl,   #dentro del conjunto de entrenamiento haz toda la bateria de distintos folds
                metric = "Accuracy")
lda.fit #information about the resampling results


## Evaluate model --------------------------------------------------------------------------------
#Evaluate the model with training and test sets
#ya el modelo esta ajustado pero queremos ver tanto las probabilidades de training y test para comparar
#training
fTR_eval <- fTR
fTR_eval$lda_prob <- predict(lda.fit, type="prob", newdata = fTR) # predict probabilities
fTR_eval$lda_pred <- predict(lda.fit, type="raw", newdata = fTR) # predict classes 
#test
fTS_eval <- fTS
fTS_eval$lda_prob <- predict(lda.fit, type="prob", newdata = fTS) # predict probabilities
fTS_eval$lda_pred <- predict(lda.fit, type="raw", newdata = fTS) # predict classes 


#Plot classification in a 2 dimensional space
Plot2DClass(fTR[,1:2], #Input variables of the model
            fTR$Y,     #Output variable
            lda.fit,#Fitted model with caret
            var1 = "X1", var2 = "X2", #variables that define x and y axis
            selClass = "YES")     #Class output to be analyzed 


## Performance measures --------------------------------------------------------------------------------

#######confusion matrices
# Training
confusionMatrix(data = fTR_eval$lda_pred, #Predicted classes
                reference = fTR_eval$Y, #Real observations
                positive = "YES") #Class labeled as Positive
      #obvio nos da la misma accuracy que el lda.fit

# test
confusionMatrix(fTS_eval$lda_pred, 
                fTS_eval$Y, 
                positive = "YES")
      #vemos que con el test nuestra accuracy baja

#######Classification performance plots 
# Training
PlotClassPerformance(fTR_eval$Y,       #Real observations
                     fTR_eval$lda_prob,  #predicted probabilities
                     selClass = "YES") #Class to be analyzed
# test
PlotClassPerformance(fTS_eval$Y,       #Real observations
                     fTS_eval$lda_prob,  #predicted probabilities
                     selClass = "YES") #Class to be analyzed)




#-------------------------------------------------------------------------------------------------
#---------------QUADRATIC DISCRIMINANT ANALYSIS --------------------------------------------------
#-------------------------------------------------------------------------------------------------
## Ovbio esto es mejor
#-------------------------------------------------------------------------------------------------
## Train QDA -----------------------------------------------------------------------------------
set.seed(150) #For replication
#Train model using training data
qda.fit = train(form = Y ~ ., #formula for specifying inputs and outputs.
                data = fTR,               #Training dataset 
                method = "qda",   #Quadratic Discriminant Analysis
                preProcess = c("center","scale"), #Center and scale
                trControl = ctrl, 
                metric = "Accuracy")
qda.fit #information about the settings


## Evaluate model --------------------------------------------------------------------------------
#Evaluate the model with training and test sets
#training
fTR_eval$qda_prob <- predict(qda.fit, type="prob", newdata = fTR) # predict probabilities
fTR_eval$qda_pred <- predict(qda.fit, type="raw", newdata = fTR) # predict classes 
#test
fTS_eval$qda_prob <- predict(qda.fit, type="prob", newdata = fTS) # predict probabilities
fTS_eval$qda_pred <- predict(qda.fit, type="raw", newdata = fTS) # predict classes 


#Plot classification in a 2 dimensional space
Plot2DClass(fTR[,1:2], #Input variables of the model
            fTR$Y,     #Output variable
            qda.fit,#Fitted model with caret
            var1 = "X1", var2 = "X2", #variables that define x and y axis
            selClass = "YES")     #Class output to be analyzed 


## Performance measures --------------------------------------------------------------------------------

#######confusion matices
# Training
confusionMatrix(data = fTR_eval$qda_pred, #Predicted classes
                reference = fTR_eval$Y, #Real observations
                positive = "YES") #Class labeled as Positive
# test
confusionMatrix(fTS_eval$qda_pred, 
                fTS_eval$Y, 
                positive = "YES")

#######Classification performance plots 
# Training
PlotClassPerformance(fTR_eval$Y,       #Real observations
                     fTR_eval$qda_prob,  #predicted probabilities
                     selClass = "YES") #Class to be analyzed
# test
PlotClassPerformance(fTS_eval$Y,       #Real observations
                     fTS_eval$qda_prob,  #predicted probabilities
                     selClass = "YES") #Class to be analyzed)



#-------------------------------------------------------------------------------------------------
#---------------------------- KNN MODEL  ----------------------------------------------
#-------------------------------------------------------------------------------------------------
set.seed(150) #For replication
#Train knn model model.
#Knn contains 1 tuning parameter k (number of neigbors). Three options:
#  - Train with a fixed parameter: tuneGrid = data.frame(k = 5),
#  - Try with a range of values specified in tuneGrid: tuneGrid = data.frame(k = seq(2,120,4)),
#  - Caret chooses 10 values: tuneLength = 10,
knn.fit = train(form = Y ~ ., #formula for specifying inputs and outputs.
                data = fTR,   #Training dataset 
                method = "knn",
                preProcess = c("center","scale"),
                #tuneGrid = data.frame(k = 5), #BUENA IDEA QUE SEA UN NUMERO IMPAR!!
                tuneGrid = data.frame(k = seq(3,121,4)),  #cuantos vecinos voy a mirar, DE 3 a 121 con saltos de 4
                                              #Para cada uno de estos k, R hace toda la cross valiation y te da el mejor k
                #tuneLength = 10,
                trControl = ctrl, 
                metric = "Accuracy")

knn.fit #information about the settings
ggplot(knn.fit) #plot the summary metric as a function of the tuning parameter TIENE QUE SER COMO UNA U! Si no es que necesitamos mas vecinos
knn.fit$finalModel #information about final model trained


## Evaluate model --------------------------------------------------------------------------------
#Evaluate the model with training and test sets
#training
#fTR_eval <- fTR
fTR_eval$knn_prob <- predict(knn.fit, type="prob" , newdata = fTR) # predict probabilities
fTR_eval$knn_pred <- predict(knn.fit, type="raw" , newdata = fTR) # predict classes 
#test
#fTS_eval <- fTS
fTS_eval$knn_prob <- predict(knn.fit, type="prob" , newdata = fTS) # predict probabilities
fTS_eval$knn_pred <- predict(knn.fit, type="raw" , newdata = fTS) # predict classes 


#Plot classification in a 2 dimensional space
Plot2DClass(fTR[,1:2], #Input variables of the model
            fTR$Y,     #Output variable
            knn.fit,#Fitted model with caret
            var1 = "X1", var2 = "X2", #variables that define x and y axis
            selClass = "YES")     #Class output to be analyzed 



## Performance measures --------------------------------------------------------------------------------

#######confusion matices
# Training
confusionMatrix(data = fTR_eval$knn_pred, #Predicted classes
                reference = fTR_eval$Y, #Real observations
                positive = "YES") #Class labeled as Positive
# test
confusionMatrix(fTS_eval$knn_pred, 
                fTS_eval$Y, 
                positive = "YES")

#######Classification performance plots 
# Training
PlotClassPerformance(fTR_eval$Y,       #Real observations
                     fTR_eval$knn_prob,  #predicted probabilities
                     selClass = "YES") #Class to be analyzed
# test
PlotClassPerformance(fTS_eval$Y,       #Real observations
                     fTS_eval$knn_prob,  #predicted probabilities
                     selClass = "YES") #Class to be analyzed)




#-------------------------------------------------------------------------------------------------
#---------------------- COMPARISON BETWEEN MODELS   ----------------------------------------------
#-------------------------------------------------------------------------------------------------

## comparison of models in training and test set --------------------------------------------------------
#TRAINING
#resampling summary metric
transformResults <- resamples(list(lda = lda.fit, qda = qda.fit, knn = knn.fit))
summary(transformResults)
dotplot(transformResults)

#Calibration curve
calPlotData <- calibration(Y ~ lda_prob$YES + qda_prob$YES + knn_prob$YES , data =fTR_eval, class = "YES", cuts = 6)
xyplot(calPlotData, auto.key = list(columns = 2))

#ROC curve
reducedRoc <- roc(response = fTR_eval$Y, fTR_eval$lda_prob$YES)
plot(reducedRoc, col="black")
auc(reducedRoc)
reducedRoc <- roc(response = fTR_eval$Y, fTR_eval$qda_prob$YES)
plot(reducedRoc, add=TRUE, col="red")
auc(reducedRoc)
reducedRoc <- roc(response = fTR_eval$Y, fTR_eval$knn_prob$YES)
plot(reducedRoc, add=TRUE, col="green")
auc(reducedRoc)
legend("bottomright", legend=c("lda", "qda", "knn"), col=c("black", "red", "green"), lwd=2)



#test
#Overall accuracy
confusionMatrix(fTS_eval$lda_pred, fTS_eval$Y)$overall[1]
confusionMatrix(fTS_eval$qda_pred, fTS_eval$Y)$overall[1]
confusionMatrix(fTS_eval$knn_pred, fTS_eval$Y)$overall[1]

#Calibration curve     
calPlotData <- calibration(Y ~ lda_prob$YES+qda_prob$YES+knn_prob$YES, data =fTS_eval, class = "YES", cuts = 6)
xyplot(calPlotData, auto.key = list(columns = 2))

#ROC curve
reducedRoc <- roc(response = fTS_eval$Y, fTS_eval$lda_prob$YES)
plot(reducedRoc, col="black")
auc(reducedRoc)
reducedRoc <- roc(response = fTS_eval$Y, fTS_eval$qda_prob$YES)
plot(reducedRoc, add=TRUE, col="red")
auc(reducedRoc)
reducedRoc <- roc(response = fTS_eval$Y, fTS_eval$knn_prob$YES)
plot(reducedRoc, add=TRUE, col="green")
auc(reducedRoc)
legend("bottomright", legend=c("lda", "qda", "knn"), col=c("black", "red", "green"), lwd=2)
  
