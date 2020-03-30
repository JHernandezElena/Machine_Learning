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
library(MLTools)


## LoadData 
fdata <- read.csv("TRdataEEMhourly.csv",sep = ";")
str(fdata)
summary(data)
hist(fdata$WG)


fdata$TL2H80 = NULL
fdata$TL3H80 = NULL
fdata$TL4H80 = NULL
fdata$TL5H80 = NULL
fdata$TL6H80 = NULL
fdata$TL7H80 = NULL
fdata$TL8H80 = NULL
fdata$TL10H80 = NULL
fdata$WSL1H80 = NULL
fdata$WSL1H80 = NULL
fdata$WSL10H80 = NULL
fdata$WSL7H80 = NULL
fdata$WSL9H80 = NULL

fdata$Hour = as.factor(fdata$Hour)



## Divide the data into training and validation sets ---------------------------------------------------
set.seed(150) #For replication
ratioTR = 0.8 #Percentage for training
#create random 80/20 % split
trainIndex <- createDataPartition(fdata$WG,     #output variable. createDataPartition creates proportional partitions
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
                          savePredictions = TRUE) 



set.seed(150) #For replication
lm.fit = train(form = WG~.,
               data = fTR, 
               method = "lm", #Linear model
               tuneGrid = data.frame(intercept = TRUE), 
               #preProcess = c("center","scale"), LO QUITAMOS PARA INTERPRETAR MEJOR LOS COEFICIENTES
               trControl = ctrl_tune, 
               metric = "RMSE")
lm.fit #information about the resampling settings

fTR_eval = fTR
fTR_eval$lm_pred = predict(lm.fit,  newdata = fTR)  
fTS_eval = fTS
fTS_eval$lm_pred = predict(lm.fit,  newdata = fTS) 

caret::RMSE(fTR_eval$lm_pred,fTR_eval$WG)
caret::RMSE(fTS_eval$lm_pred,fTS_eval$WG)
