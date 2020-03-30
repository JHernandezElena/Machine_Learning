##PRACTICA INSURANCE

##EN ESTE PROBLEMA NO TENEMOS QUE HACER SELECCION DE VARIABLES
  #son pocas y no estan correlacionadas (hecho a posta)

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

fdata <- read.csv("insurance.csv", sep = ";")
fdata = na.omit(fdata)
str(fdata)

#CORRELACION LINEALES plot of numeric variables
numvars <- sapply(fdata, class) %in% c("integer","numeric")
C <- cor(fdata[,numvars])
corrplot(C, method = "number")

#RELACIONES ENTRE VARIABLES
ggpairs(fdata,aes( alpha = 0.3))
PlotDataframe(fdata,output.name = "charges") 
    #vemos que hay dos tres grupos en age (posiblemente relacionado con fumar que es muy influyente!)
    #vemos que BMI tiene que estar relacionado con otra variable (fumador) y con una tercera a partir de los 30 la cosa cambia (age)



## Divide the data into training and validation sets ---------------------------------------------------
set.seed(150) #For replication
#create random 80/20 % split
trainIndex <- createDataPartition(fdata$charges,      #output variable. createDataPartition creates proportional partitions
                                  p = 0.8,      #split probability for training
                                  list = FALSE, #Avoid output as a list
                                  times = 1)    #only one partition
#obtain training and validation sets
fTR <- fdata[trainIndex,]
fTR_eval <- fTR #
fTV <- fdata[-trainIndex,]


## Initialize trainControl -----------------------------------------------------------------------
#Use resampling for measuring generalization error
#K-fold with 10 folds
ctrl_tune <- trainControl(method = "cv",                     
                          number = 10,
                          summaryFunction = defaultSummary,    #Performance summary for comparing models in hold-out samples.
                          returnResamp = "final",              #Return final information about resampling
                          savePredictions = TRUE)              #save predictions


#==================================================================
## MODELO 1 - todas las variables
#==================================================================
set.seed(150) #For replication
lm1.fit = train(form = charges ~ ., 
                data = fTR, 
                method = "lm", #Linear model
                #tuneGrid = data.frame(intercept = TRUE), 
                preProcess = c("center","scale"),
                trControl = ctrl_tune, 
                metric = "RMSE")
lm1.fit #information about the resampling settings
#RMSE es que nos podemos equivocar 2.15 grados y R2 muy cerca de 1 lo que es bien

summary(lm1.fit)  #information about the model trained
#sex da un p-valor muy alto por lo que igual no es relevante
#children y smoker yes igual related


#Evaluate the model with training sets and diagnosis
fTR_eval$lm_pred1 = predict(lm1.fit,  newdata = fTR)  

PlotModelDiagnosis(fTR
                   , fTR$charges, fTR_eval$lm_pred1,
                   together = TRUE)
#IDEALMENRE QUIERO UN HISTOGRAMA NORMAL
#REAL VS FORECAST QUEREMOS UNA NUBE ALEATORIA 
#EN RESIDUALS VS ... QUEREMOS MEDIA 0!!!


#==================================================================
## MODELO 2 - mirando en plotdata frame y estableciendo relaciones 
#==================================================================
set.seed(150) #For replication
lm2.fit = train(form = charges ~ smoker + region +smoker*age + smoker*bmi, 
                data = fTR, 
                method = "lm", #Linear model
                #tuneGrid = data.frame(intercept = TRUE), 
                preProcess = c("center","scale"),
                trControl = ctrl_tune, 
                metric = "RMSE")
lm2.fit #information about the resampling settings
#RMSE es que nos podemos equivocar 2.15 grados y R2 muy cerca de 1 lo que es bien

summary(lm2.fit)  #information about the model trained
#sex da un p-valor muy alto por lo que igual no es relevante
#children y smoker yes igual related


#Evaluate the model with training sets and diagnosis
fTR_eval$lm_pred2 = predict(lm2.fit,  newdata = fTR)  

PlotModelDiagnosis(fTR
                   , fTR$charges, fTR_eval$lm_pred1,
                   together = TRUE)
#IDEALMENRE QUIERO UN HISTOGRAMA NORMAL
#REAL VS FORECAST QUEREMOS UNA NUBE ALEATORIA 
#EN RESIDUALS VS ... QUEREMOS MEDIA 0!!!



