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

X8_bp = boxplot(ts$X8)
X8_atip = which(ts$X8 %in% X8_bp$out)
ts = ts[-which(ts$X8 > 12), ]
X8_bp = boxplot(ts$X8)

ts = ts[,-4]





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
                maxit = 200,    # Maximum number of iterations
                #tuneGrid = data.frame(size =5, decay = 0),
                tuneGrid = expand.grid(size = seq(5,25,by=5),
                                       #decay=c(10^(-9),0.0001,0.001,0.01,0.1,1)),
                                       decay=c(0,0.025,0.05,0.075,0.1)),
                trControl = ctrl, 
                metric = "Accuracy")

mlp.fit #information about the resampling settings
ggplot(mlp.fit)+scale_x_log10()

mlp.fit$finalModel #information about the model trained
#summary(mlp.fit$finalModel) #information about the network and weights
plotnet(mlp.fit$finalModel) #Plot the network






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



#Statistical sensitivity analysis
library(NeuralSens)
SensAnalysisMLP(mlp.fit) 


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
