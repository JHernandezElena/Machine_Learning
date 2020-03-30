###############################################################
#####   Classification:  Decision trees                    ####
###############################################################

## Set working directory -------------------------------------------------------------------------

## Load libraries --------------------------------------------------------------------------------
library(caret)
library(ggplot2)
library(ROCR) #for plotting ROC curves.
library(MLTools)


## Load file -------------------------------------------------------------------------------------
fdata <- read.table("SimData.dat", sep = "", header = TRUE, stringsAsFactors = FALSE)
str(fdata); head(fdata)
#Convert output variable to factor
fdata$Y <- as.factor(fdata$Y)
str(fdata)


## Exploratory analysis -------------------------------------------------------------------------------------
ggplot(fdata) + geom_point(aes(x = X1, y = X2, color = Y))



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
inputs <- 1:2  #las primeras dos columnas de la tabla de datos son los inputs

## Initialize trainControl -----------------------------------------------------------------------
ctrl <- trainControl(method = "cv",                        #k-fold cross-validation
                     number = 10,                          #Number of folds
                     summaryFunction = defaultSummary,     #Performance summary for comparing models in hold-out samples.
                     classProbs = TRUE)                    #Compute class probs in Hold-out samples


#-------------------------------------------------------------------------------------------------
#---------------------------- DECISION TREE ------------------------------------------------------
#-------------------------------------------------------------------------------------------------
library(rpart)
library(rpart.plot)
library(partykit)
set.seed(150) #For replication

#Train decision tree
#rpart contains 1 tuning parameter cp (Complexity parameter). Three options:
#  - Train with a fixed parameter: tuneGrid = data.frame(cp = 0.1),
#  - Try with a range of values specified in tuneGrid: tuneGrid = data.frame(cp = seq(0,0.4,0.05))),
#  - Caret chooses 10 values: tuneLength = 10,

#NOTE: Formula ("form= ")method could be used, but it will automatically create dummy variables. 
# Decision trees can work with categorical variables as theey are. Then, x and y arguments are used
tree.fit <- train(x = fTR[,inputs],  #Input variables. (no ponemos "form= " porque no queremos crear variables dummy)
                 y = fTR$Y,   #Output variable
                 method = "rpart",   #Decision tree with cp as tuning parameter = HIPERPARAMETRO
                 control = rpart.control(minsplit = 5,  # Minimum number of obs in node to keep cutting
                                        minbucket = 5), # Minimum number of obs in a terminal node
                 parms = list(split = "gini"),          # impuriry measure
                 #tuneGrid = data.frame(cp = 0.1), # TRY this: tuneGrid = data.frame(cp = 0.25), #Si ponemos cp=0 sera un arbol enorme
                 #tuneLength = 10,
                 tuneGrid = data.frame(cp = seq(0,0.1,0.0005)),
                 trControl = ctrl, 
                 metric = "Accuracy")
tree.fit #information about the resampling settings
ggplot(tree.fit) #plot the summary metric as a function of the tuning parameter
summary(tree.fit)  #information about the model trained
                        #EJ: nodo 2 hace un split                          
                        #nodo 3 es un leaf
                    
tree.fit$finalModel #Cuts performed and nodes. Also shows the number and percentage of cases in each node.
                          #n=numero de datos en cada nod, #loss las veces que me equivoco, #yval = clase que predecimos
                          #si hace tabs es porque esos nodos cuelgan del anterior (el 4 y 5 cuelgan del 2)
#Basic plot of the tree:
plot(tree.fit$finalModel, uniform = TRUE, margin = 0.1)
text(tree.fit$finalModel, use.n = TRUE, all = TRUE, cex = .8)
        #NOes/YESes
#Advanced plots
rpart.plot(tree.fit$finalModel, type = 2, fallen.leaves = FALSE, box.palette = "Oranges")
        #los 0.xx son las probabilidades de que salgan YESs
tree.fit.party <- as.party(tree.fit$finalModel)
plot(tree.fit.party)

#Measure for variable importance
varImp(tree.fit,scale = FALSE)
plot(varImp(tree.fit,scale = FALSE))

## Evaluate model --------------------------------------------------------------------------------
#Evaluate the model with training and test sets
#training
fTR_eval <- fTR
fTR_eval$tree_prob <- predict(tree.fit, type="prob", newdata = fTR) # predict probabilities
fTR_eval$tree_pred <- predict(tree.fit, type="raw", newdata = fTR) # predict classes 
#test
fTS_eval <- fTS
fTS_eval$tree_prob <- predict(tree.fit, type="prob", newdata = fTS) # predict probabilities
fTS_eval$tree_pred <- predict(tree.fit, type="raw", newdata = fTS) # predict classes 



#Plot classification in a 2 dimensional space
Plot2DClass(fTR[,1:2], #Input variables of the model
            fTR$Y,     #Output variable
            tree.fit,#Fitted model with caret
            var1 = "X1", var2 = "X2", #variables that define x and y axis
            selClass = "YES")     #Class output to be analyzed 


## Performance measures --------------------------------------------------------------------------------

#######confusion matices
# Training
confusionMatrix(data = fTR_eval$tree_pred, #Predicted classes
                reference = fTR_eval$Y, #Real observations
                positive = "YES") #Class labeled as Positive
# test
confusionMatrix(fTS_eval$tree_pred, 
                fTS_eval$Y, 
                positive = "YES")

#######Classification performance plots 
# Training
PlotClassPerformance(fTR_eval$Y,       #Real observations
                     fTR_eval$tree_prob,  #predicted probabilities
                     selClass = "YES") #Class to be analyzed
# test
PlotClassPerformance(fTS_eval$Y,       #Real observations
                     fTS_eval$tree_prob,  #predicted probabilities
                     selClass = "YES") #Class to be analyzed)


