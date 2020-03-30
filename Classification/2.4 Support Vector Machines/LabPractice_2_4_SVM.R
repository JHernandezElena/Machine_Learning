#################################################################################
##############          Classification: SVM         ############################
#################################################################################

## Set working directory -------------------------------------------------------------------------

## Load libraries --------------------------------------------------------------------------------
library(caret)
library(ggplot2)
library(ROCR) #for plotting ROC curves.
library(MLTools)


## Load necessary functions --------------------------------------------------------------------


## Load file -------------------------------------------------------------------------------------
# 1) Generate a 2D daset which is linearly separable
#fdata <- GenMat(2,100,.5)
# 2) try this dataset for radial SVM
fdata <- read.table("SimData.dat", sep = "", header = TRUE, stringsAsFactors = FALSE)
str(fdata)
#Convert output variable to factor
fdata$Y <- as.factor(fdata$Y)
levels(fdata$Y)=c("YES","NO")
str(fdata)


## Exploratory analysis -------------------------------------------------------------------------------------
ggplot(fdata) + geom_point(aes(x = X1, y = X2, color = Y))
#For datasets with more than two inputs



## Divide the data into training and test sets ---------------------------------------------------
set.seed(150) #For replication
#create random 80/20 % split
trainIndex <- createDataPartition(fdata$Y,      #output variable. createDataPartition creates proportional partitions
                                  p = 0.8,      #split probability for training
                                  list = FALSE, #Avoid output as a list
                                  times = 1)    #only one partition
#obtain training and test sets
fTR <- fdata[trainIndex,]
fTS <- fdata[-trainIndex,]


## Initialize trainControl -----------------------------------------------------------------------
ctrl <- trainControl(method = "cv",                        #k-fold cross-validation
                     number = 10,                          #Number of folds
                     summaryFunction = defaultSummary,     #Performance summary for comparing models in hold-out samples.
                     classProbs = TRUE)                    #Compute class probs in Hold-out samples


#-------------------------------------------------------------------------------------------------
#---------------------------- SVM LINEAR ------------------------------------------------------
#-------------------------------------------------------------------------------------------------
library(kernlab)
set.seed(150) #For replication
#Train linear  svm
#svm contains 1 tuning parameter C (Cost). Three options:
#  - Train with a fixed parameter: tuneGrid = data.frame(C = 0.1),
#  - Try with a range of values specified in tuneGrid: tuneGrid = data.frame(cp = seq(0.1,10,0.5)),
#  - Caret chooses 10 values: tuneLength = 10,
svm.fit <- train(form = Y ~ ., #formula for specifying inputs and outputs.
                data = fTR,   #Training dataset 
                method = "svmLinear",
                preProcess = c("center","scale"),
                # 1) try C=0.1
                #tuneGrid = data.frame(C = 0.1),
                # 2) try C=10 and compare with C=0.1
                #tuneGrid = data.frame(C = 10),
                # 3) find the optimal value of C
                #tuneGrid = expand.grid(C = c(0.00001,0.0001,0.001,0.01,0.1,1,10,100,1000)),
                tuneGrid = data.frame(C = seq(0.1,10,1)),
                #tuneLength = 10,
                trControl = ctrl, 
                metric = "Accuracy")
svm.fit #information about the resampling settings
ggplot(svm.fit) + scale_x_log10()
svm.fit$finalModel #information about the model trained
#Plot the svm support vectors:
isupvect <- alphaindex(svm.fit$finalModel)[[1]] #indexes for support vectors
#plot support vectors
ggplot() + geom_point(data = fTR[isupvect,],aes(x = X1, y = X2), color = "red") +
  geom_point(data = fTR[-isupvect,], aes(x = X1, y = X2))



## Evaluate model --------------------------------------------------------------------------------
#Evaluate the model with training and test sets
#training
fTR_eval <- fTR
fTR_eval$svm_prob <- predict(svm.fit, type="prob", newdata = fTR) # predict probabilities
fTR_eval$svm_pred <- predict(svm.fit, type="raw", newdata = fTR) # predict classes 
#test
fTS_eval <- fTS
fTS_eval$svm_prob <- predict(svm.fit, type="prob", newdata = fTS) # predict probabilities
fTS_eval$svm_pred <- predict(svm.fit, type="raw", newdata = fTS) # predict classes 


#Plot classification in a 2 dimensional space
Plot2DClass(fTR[,1:2], #Input variables of the model
            fTR$Y,     #Output variable
            svm.fit,#Fitted model with caret
            var1 = "X1", var2 = "X2", #variables that define x and y axis
            selClass = "YES")     #Class output to be analyzed 




#-------------------------------------------------------------------------------------------------
#---------------------------- SVM RADIAL ------------------------------------------------------
#-------------------------------------------------------------------------------------------------
library(kernlab)
set.seed(150) #For replication
#Train model using training data
#Train radial  svm
#svm contains 2 tuning parameter C (Cost) and sigma. Three options:
#  - Train with a fixed parameter: tuneGrid = data.frame( sigma=100, C=1),
#  - Try with a range of values specified in tuneGrid: tuneGrid = expand.grid(C = seq(0.1,100,length.out = 8), sigma=seq(0.01,50,length.out = 4)),
#  - Caret chooses 10 values: tuneLength = 10,
svm.fit = train(form = Y ~ ., #formula for specifying inputs and outputs.
                data = fTR,   #Training dataset 
                method = "svmRadial",
                preProcess = c("center","scale"),
                tuneGrid = expand.grid(C = c(0.001,0.01,0.1,1,10,100,1000), sigma=c(0.0001,0.001,0.01,0.1,1,10)),
                #tuneGrid =  data.frame(sigma = 0.01, C = 0.1),  
                #tuneGrid = expand.grid(C = seq(0.1,1000,length.out = 8), sigma=seq(0.01,50,length.out = 4)),
                #tuneLength = 10,
                trControl = ctrl, 
                metric = "Accuracy")
svm.fit #information about the resampling settings
ggplot(svm.fit) + scale_x_log10()
svm.fit$finalModel #information about the model trained
#Plot the svm support vectors:
isupvect <- alphaindex(svm.fit$finalModel)[[1]] #indexes for support vectors
#plot support vectors
ggplot() + geom_point(data = fTR[isupvect,], aes(x = X1, y = X2), color = "red") +
  geom_point(data = fTR[-isupvect,], aes(x = X1, y = X2))
plot(svm.fit$finalModel, data = as.matrix(predict(svm.fit$preProcess,fTR[,1:2])))


## Evaluate model --------------------------------------------------------------------------------
#Evaluate the model with training and test sets
#training
fTR_eval <- fTR
fTR_eval$svm_prob <- predict(svm.fit, type="prob", newdata = fTR) # predict probabilities
fTR_eval$svm_pred <- predict(svm.fit, type="raw", newdata = fTR) # predict classes 
#test
fTS_eval <- fTS
fTS_eval$svm_prob <- predict(svm.fit, type="prob", newdata = fTS) # predict probabilities
fTS_eval$svm_pred <- predict(svm.fit, type="raw", newdata = fTS) # predict classes 


#Plot classification in a 2 dimensional space
Plot2DClass(fTR[,1:2], #Input variables of the model
            fTR$Y,     #Output variable
            svm.fit,#Fitted model with caret
            var1 = "X1", var2 = "X2", #variables that define x and y axis
            selClass = "YES")     #Class output to be analyzed 



## Performance measures --------------------------------------------------------------------------------

#######confusion matices
# Training
confusionMatrix(data = fTR_eval$svm_pred, #Predicted classes
                reference = fTR_eval$Y, #Real observations
                positive = "YES") #Class labeled as Positive
# test
confusionMatrix(fTS_eval$svm_pred, 
                fTS_eval$Y, 
                positive = "YES")

#######Classification performance plots 
# Training
PlotClassPerformance(fTR_eval$Y,       #Real observations
                     fTR_eval$svm_prob,  #predicted probabilities
                     selClass = "YES") #Class to be analyzed
# test
PlotClassPerformance(fTS_eval$Y,       #Real observations
                     fTS_eval$svm_prob,  #predicted probabilities
                     selClass = "YES") #Class to be analyzed)

