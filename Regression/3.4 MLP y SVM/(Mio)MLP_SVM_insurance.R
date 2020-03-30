library(readxl)
library(ggplot2)
library(caret)
library(GGally)
library(splines)
library(NeuralNetTools)
library(gridExtra)
library(MLTools)

fdata <- read.csv("insurance.csv",sep = ";")

## Exploratory analysis -------------------------------------------------------------------------------------

# Contents of the dataset
str(fdata)

#Children has whole values from 0-5. 
#The the order has meaning (i.e. more children could mean greater charges) 
#It could be factor or numeric. If we choose factor, the model will be more detailed.
fdata$children <- as.factor(fdata$children)


summary(fdata)
#we can see there are no outliers

#Analyze output variable
hist(fdata$charges)
#skewed right distribution. The ideal would be to have a normal distibution.


#Explore relationships between variables
ggpairs(fdata,aes( alpha = 0.3))
#we can see that there does not seem to be any relation between numeric inputs (age and bmi)
#and the distribution of those per category of factor variables seem normal.
#Doesnt seem to be any outliers in the data.


#correlation plot of numeric variables
numvars <- sapply(fdata, class) %in% c("integer","numeric")
C <- cor(fdata[,numvars])
corrplot::corrplot(C, method = "circle")
#We can confirm that there is not a high LINEAR correlation between numeric inputs. 
#we can also see that age is more LINEARLY correlated to the output than bmi.


#To analyze in greater detail the relation between inputs and output, use PlotDataframe function
PlotDataframe(fdata, output.name = "charges")
#We can observe serveral effects from these plots:
# - Smoker seems a relevant variable looking at the boxplots. If smoker=yes, charges are higher.
# - We confirm the linear correlation between age and charges, although we can see that there seems 
#   to be 3 different behaviors. These should be explained by a factor variable. Also, the effects
#   seem to be a displacement on the level of charges. Therefore, interactions might not be needed in this case.
# - We see that bmi is related to charges, but there also appear to be different behaviors.
#   In this case, the effect of bmi on charges changes slope between behaviors. Interaction might be needed.


#plotting bmi and ages against charges and grouping by smoker, 
#we can see that these different behavior are greatly explained by this variable
#(we have included regression lines per category)
#AGE contra CHARGES diferenciado si se fuma o no
ggplot(fdata,aes(x=age,y=charges, color = smoker))+geom_point()+geom_smooth(method="lm")
#BMI contra CHARGES diferenciado si se fuma o no
#aqui vemos que cuando no fumas la variable BMI no importa pero cuando fumas si
ggplot(fdata,aes(x=bmi,y=charges, color = smoker))+geom_point()+geom_smooth(method="lm")




## Divide the data into training and validation sets ---------------------------------------------------
set.seed(150) #For replication
#create random 80/20 % split
trainIndex <- createDataPartition(fdata$charges,     #output variable. createDataPartition creates proportional partitions
                                  p = 0.8, #split probability
                                  list = FALSE, #Avoid output as a list
                                  times = 1) #only one partition
#obtain training and validation sets
fTR = fdata[trainIndex,]
fTS = fdata[-trainIndex,]

#Create index to select input variables
varindex <- variable.names(fdata) != "charges"

## Initialize trainControl -----------------------------------------------------------------------
#Use resampling for measuring generalization error
#K-fold with 10 folds
ctrl_tune <- trainControl(method = "cv",                     
                          number = 10,
                          summaryFunction = defaultSummary,    #Performance summary for comparing models in hold-out samples.
                          returnResamp = "final",              #Return final information about resampling
                          savePredictions = TRUE)              #save predictions


##-----------------------------------------------------------------------------------------
## MLP -------------------------------------------------------------------------------------------
##-----------------------------------------------------------------------------------------
set.seed(150) #For replication
mlp.fit = train(form = charges~.,
                data = fTR, 
                method = "nnet",
                linout = TRUE, #true porque es problema con salida continua, si fuera de clasificacion pondriamos false
                maxit = 250,
                #tuneGrid = data.frame(size = 10, decay = 0),
                tuneGrid = expand.grid(size = seq(25,30,length.out = 5), decay=10^seq(-9,-7, length.out=6)),
                #tuneLength = 5,
                preProcess = c("center","scale"),
                trControl = ctrl_tune, 
                metric = "RMSE")
mlp.fit #information about the resampling settings
ggplot(mlp.fit) + scale_x_log10()
##buscamos el minimo rmse en cross-validation

fTR_eval = fTR
fTS_eval = fTS

fTR_eval$mlp_pred = predict(mlp.fit,  newdata = fTR)  
fTS_eval$mlp_pred = predict(mlp.fit,  newdata = fTS)  

SensAnalysisMLP(mlp.fit) #Statistical sensitivity analysis
#vemos que este modelo no usa ni la variable tiempo ni la x5 (en mean(sens^2))
#en x1 vemos en density(sense) que x1 tiene una sensibilidad constante en torno a 1->NOS DICE QUE HAY UNA RELACION CONSTANTE/LINEAL CON LA SALIDA

PlotModelDiagnosis(fTR[,varindex], fTR$y, 
                   fTR_eval$mlp_pred, together = TRUE)
