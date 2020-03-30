library(tidyverse)
library(dbplyr)
library(caret)
library(ggplot2)
library(GGally)
library(ROCR)
library(pROC)
library(MLTools)
library(tidyverse)
library(kernlab)

#Read table
ts = read.delim("TrainingSet1001.dat", header = TRUE, sep = "")


#Outliers
X1_bp = boxplot(ts$X1)
X1_atip = which(ts$X1 %in% X1_bp$out)
X2_bp = boxplot(ts$X2)
X3_bp = boxplot(ts$X3)
X4_bp = boxplot(ts$X4)
X5_bp = boxplot(ts$X5)
X6_bp = boxplot(ts$X6)
X7_bp = boxplot(ts$X7)

X8_bp = boxplot(ts$X8)
X8_atip = which(ts$X8 %in% X8_bp$out)
ts = ts[-which(ts$X8 > 12), ]
X8_bp = boxplot(ts$X8)


X9_bp = boxplot(ts$X9)
X10_bp = boxplot(ts$X10)

# Correlaciones interesantes
cor(ts$X5, ts$X1)
cor(ts$X7, ts$X3)

ggpairs(ts,aes(color = Y, alpha = 0.3))
ts = ts[,-3]


##*************************************************************************
## DIVISION DE DATOS EN CONJUNTO DE ENTRENAMIENTO VS CONJUNTO DE TEST
##*************************************************************************
set.seed(2019) 

trainIndex = createDataPartition(ts$Y, #output variable. createDataPartition creates proportional partitions
                                 p = 0.8,      #split probability for training
                                 list = FALSE, #Avoid output as a list
                                 times = 1)    #only one partition

fTR = ts[trainIndex,]
fTS = ts[-trainIndex,]


##*************************************************************************
## CREACION DEL CTRL PARA CROSS VALIDATION
##*************************************************************************
ctrl = trainControl(method = "cv",                        #k-fold cross-validation
                    number = 10,                          #Number of folds
                    summaryFunction = defaultSummary,     #Performance summary for comparing models in hold-out samples.
                    classProbs = TRUE)




#-------------------------------------------------------------------------------------------------
#-------------------------------- MLP ------------------------------------------------------------
#-------------------------------------------------------------------------------------------------
library(NeuralNetTools) ##Useful tools for plotting and analyzing neural networks
library(nnet)
#Train MLP
#mlp contains 2 tuning parameters size and decay Three options:
#  - Train with a fixed parameter: tuneGrid = data.frame(size =5, decay = 0),
#  - Try with a range of values specified in tuneGrid: tuneGrid = expand.grid(size = seq(5,25,length.out = 5), decay=c(10^(-9),0.0001,0.001,0.01,0.1,1,10)),
#  - Caret chooses 10 values: tuneLength = 10,
set.seed(2019) #For replication
mlp.fit = train(form = Y ~ ., #formula for specifying inputs and outputs.
                data = fTR,   #Training dataset 
                method = "nnet",
                preProcess = c("center","scale"),
                maxit = 180,    # Maximum number of iterations
                #tuneGrid = data.frame(size =5, decay = 0),
                tuneGrid = expand.grid(size = seq(5,25,length.out = 5),
                                       #decay=c(10^(-9),0.0001,0.001,0.01,0.1,1)),
                                       decay=c(0.05,0,075,0.1,0.125,0.15, 0.175)),
                trControl = ctrl, 
                metric = "Accuracy")

mlp.fit #information about the resampling settings
ggplot(mlp.fit)+scale_x_log10()

mlp.fit$finalModel #information about the model trained
#summary(mlp.fit$finalModel) #information about the network and weights
plotnet(mlp.fit$finalModel) #Plot the network

#Statistical sensitivity analysis
library(NeuralSens)
SensAnalysisMLP(mlp.fit) 


## Evaluate model --------------------------------------------------------------------------------
#Evaluate the model with training and test sets
#training
fTR_eval = fTR
fTR_eval$mlp_prob = predict(mlp.fit, type="prob" , newdata = fTR) # predict probabilities
fTR_eval$mlp_pred = predict(mlp.fit, type="raw" , newdata = fTR) # predict classes 
#test
fTS_eval = fTS
fTS_eval$mlp_prob = predict(mlp.fit, type="prob" , newdata = fTS) # predict probabilities
fTS_eval$mlp_pred = predict(mlp.fit, type="raw" , newdata = fTS) # predict classes 





# Training
confusionMatrix(data = fTR_eval$mlp_pred, #Predicted classes
                reference = fTR_eval$Y, #Real observations
                positive = "YES") #Class labeled as Positive
# test
confusionMatrix(fTS_eval$mlp_pred, 
                fTS_eval$Y, 
                positive = "YES")



val = read.delim(file = "ValidationSet999.dat", header = TRUE, sep = "")
ValForecast = predict(mlp.fit, newdata = val)

write.table(ValForecast ,"T13.csv", col.names = FALSE, row.names = FALSE)






## Performance measures --------------------------------------------------------------------------------

#######confusion matices
# Training
confusionMatrix(data = fTR_eval$mlp_pred, #Predicted classes
                reference = fTR_eval$Default, #Real observations
                positive = "YES") #Class labeled as Positive
# test
confusionMatrix(fTS_eval$mlp_pred, 
                fTS_eval$Default, 
                positive = "YES")

#######Classification performance plots 
# Training
PlotClassPerformance(fTR_eval$Default,       #Real observations
                     fTR_eval$mlp_prob,  #predicted probabilities
                     selClass = "YES") #Class to be analyzed
# test
PlotClassPerformance(fTS_eval$Default,       #Real observations
                     fTS_eval$mlp_prob,  #predicted probabilities
                     selClass = "YES") #Class to be analyzed)  



















#-------------------------------------------------------------------------------------------------
#-------------------LINEAR DISCRIMINANT ANALISIS -------------------------------------------------
#-------------------------------------------------------------------------------------------------
## Va a intentar hacer un corte lineal para separar estos datos
## Minimo debemos esperar un 67% de accuracy ya que esa es la distribucion de los datos y es lo que
#obtendriamos al azar
#-------------------------------------------------------------------------------------------------

## Train LDA ------------------------------------------------------------------
set.seed(2019) #For replication
#Train model using training data
lda.fit = train(form = Y ~ X3+X10, #formula for specifying inputs and outputs(usando "form" las variables de etrada de 
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


## Performance measures --------------------------------------------------------------------------------

#######Confusion matrices
# Training
confusionMatrix(data = fTR_eval$lda_pred, #Predicted classes
                reference = fTR_eval$Y, #Real observations
                positive = "YES") #Class labeled as Positive
#obvio nos da la misma accuracy que el lda.fit

# test
confusionMatrix(fTS_eval$lda_pred, 
                fTS_eval$Y, 
                positive = "YES")
#vemos que con el test nuestra accuracy sube un poco, modelo bien calibrado con el training set


#######Classification performance plots
# Training
PlotClassPerformance(fTR_eval$Default,       #Real observations
                     fTR_eval$lda_prob,  #predicted probabilities
                     selClass = "YES") #Class to be analyzed
# test
PlotClassPerformance(fTS_eval$Default,       #Real observations
                     fTS_eval$lda_prob,  #predicted probabilities
                     selClass = "YES") #Class to be analyzed)


##Aandir variables

ts$X4sq <- ts$X4^2
ts$X8sq <- ts$X8^2#SE VEIA CLARAMENTE QUE ENTRE LA X1 Y LA Y HABIA UNA RELACION CUADRATICA)
#obtain training and test sets
fTR <- ts[trainIndex,]
fTS <- ts[-trainIndex,]


lda.fit = train(form = Y ~ X3+X10+X4sq+X8sq, #formula for specifying inputs and outputs(usando "form" las variables de etrada de 
                #tipo factor se convierten en variables dummy 0, 1 como el ejemplo del color de pelo)
                #NO HACER PARA DECISION TREE
                data = fTR,     #Training dataset 
                method = "lda",  # Linear Discriminant Analysis
                preProcess = c("center","scale"), #Preprocesado de Center and scale
                trControl = ctrl,   #dentro del conjunto de entrenamiento haz toda la bateria de distintos folds
                metric = "Accuracy")
lda.fit #information about the resampling results



#-------------------------------------------------------------------------------------------------
#---------------QUADRATIC DISCRIMINANT ANALYSIS --------------------------------------------------
#-------------------------------------------------------------------------------------------------

## Train QDA -----------------------------------------------------------------------------------
set.seed(2019) #For replication
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
PlotClassPerformance(fTR_eval$Default,       #Real observations
                     fTR_eval$qda_prob,  #predicted probabilities
                     selClass = "YES") #Class to be analyzed
# test
PlotClassPerformance(fTS_eval$Default,       #Real observations
                     fTS_eval$qda_prob,  #predicted probabilities
                     selClass = "YES") #Class to be analyzed)



#-------------------------------------------------------------------------------------------------
#---------------------------- SVM RADIAL ------------------------------------------------------
#-------------------------------------------------------------------------------------------------
set.seed(2019) #For replication
#Train model using training data
#Train radial  svm
#svm contains 2 tuning parameter C (Cost) and sigma. 
svmR.fit = train(form = Y ~ ., #formula for specifying inputs and outputs.
                 data = fTR,   #Training dataset 
                 method = "svmRadial",
                 preProcess = c("center","scale"),
                 tuneLength = 10,
                 #tuneGrid = expand.grid(C = c(0.001, 0.005,0.01, 0.1), sigma=c(0.025, 0.05, 0.075, 0.1, 0.25, 0.5)),
                 tuneGrid = expand.grid(C = seq(50, 70, by=5), sigma=seq(0.015, 0.03, by = 0.005)),
                 trControl = ctrl, 
                 metric = "Accuracy")
svmR.fit#information about the resampling settings
ggplot(svmR.fit) + scale_x_log10()
svmR.fit$finalModel #information about the model trained


## Evaluate model --------------------------------------------------------------------------------
#Evaluate the model with training and test sets
#training
fTR_eval <- fTR
fTR_eval$svmR_prob <- predict(svmR.fit, type="prob", newdata = fTR) # predict probabilities
fTR_eval$svmR_pred <- predict(svmR.fit, type="raw", newdata = fTR) # predict classes 

#test
fTS_eval <- fTS
fTS_eval$svmR_prob <- predict(svmR.fit, type="prob", newdata = fTS) # predict probabilities
fTS_eval$svmR_pred <- predict(svmR.fit, type="raw", newdata = fTS) # predict classes 



## Performance measures --------------------------------------------------------------------------------

#######confusion matices
# Training
confusionMatrix(data = fTR_eval$svmR_pred, #Predicted classes
                reference = fTR_eval$Y, #Real observations
                positive = "YES") #Class labeled as Positive
# test
confusionMatrix(fTS_eval$svmR_pred, 
                fTS_eval$Y, 
                positive = "YES")



