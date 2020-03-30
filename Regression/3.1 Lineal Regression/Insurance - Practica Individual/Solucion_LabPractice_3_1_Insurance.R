#################################################################################
##############           Insurance dataset          ############################
##############     ----------- solution ---------   ############################
#################################################################################

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
fdata <- read.csv("insurance.csv",sep = ";")

## Exploratory analysis -------------------------------------------------------------------------------------
# - Types of variables
# - Identification of NA
# - Identification of outliers
# - Relations between input variables
# - Relation between the inputs and the output

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


## Model training ------------------------------------------------------------------------------

## Divide the data into training and validation sets ---------------------------------------------------
set.seed(150) #For replication
#create random 80/20 % split
trainIndex <- createDataPartition(fdata$charges,      #output variable. createDataPartition creates proportional partitions
                                  p = 0.8,      #split probability for training
                                  list = FALSE, #Avoid output as a list
                                  times = 1)    #only one partition
#obtain training and validation sets
fTR <- fdata[trainIndex,]
fTS <- fdata[-trainIndex,]

## Initialize trainControl -----------------------------------------------------------------------
#Use resampling for measuring generalization error
#K-fold with 10 folds
ctrl_tune <- trainControl(method = "cv",                     
                          number = 10,
                          summaryFunction = defaultSummary,    #Performance summary for comparing models in hold-out samples.
                          returnResamp = "final",              #Return final information about resampling
                          savePredictions = TRUE)              #save predictions



## Process for model identification in regression:
# 1) Train model
# 2) Analyze variable importance
# 3) Analyze multicolinearity
# 4) Analyze residuals against input variables (All plots should be centered in 0)
# 5) Identify nonlinearities and interactions
# Repeat from 1) with selected variables and nonlinear and interaction effects


## Linear Regression -------------------------------------------------------------------------------------------

##ALL INPUT VARIABLES
#Lets begin with an initial model using ALL INPUT VARIABLES
#It is usually recomended to center and scale variables. PERO LOS QUITAMOS PARA VER MEJOR LA IMP DE LAS VARIABLES
#For teaching purposes, this transformation will not be applied.
set.seed(150) #For replication
lm.fit = train(form = charges~.,
               data = fTR, 
               method = "lm", #Linear model
               tuneGrid = data.frame(intercept = TRUE), 
               #preProcess = c("center","scale"), LO QUITAMOS PARA INTERPRETAR MEJOR LOS COEFICIENTES
               trControl = ctrl_tune, 
               metric = "RMSE")
lm.fit #information about the resampling settings

summary(lm.fit)  #information about the model trained
#Reference group: sex=female, smoker=no, region=northeast, children=0
#We observed several variables and categories that are not significant 
#EL ESTIMATED PARA FACTORES DICE CUANTO SUBE/BAJA EL COSTE SI YO FUMO
    #PARA CONTINUOS DICE QUE SI INCREMENTO EN UNO LA EDAD (POR EJEMPLO) SUBE/BAJA ESE COSTE


#Identify correlated variables
vif(lm.fit$finalModel)
#No vif>10, there is not multicolineality present in the model


#Evaluate the model with training sets and diagnosis
fTR_eval = fTR
fTR_eval$lm_pred = predict(lm.fit,  newdata = fTR)  
fTS_eval = fTS
fTS_eval$lm_pred = predict(lm.fit,  newdata = fTS) 


###MUY IMPORTANTE!!!! - ANALISIS DE RESIDUALS
#residuos = diferencia entre el valor real y el valor de la prediccion
#queremos todos los residuos CENTRADOS EN 0
#queremos que sean HISTOGRAMAS NORMALES 
#en real vs forecast vemos que a partir de 2000 tenemos error siempre
#en residuals vs variable continua queremos NUBES DE PUNTOS UNIFORMES
#en residuals vs age "se ve" una pequena parabloa-> relacion quadratica
#residuals vs charges(lo que quiero de predecir)
    #en torno a 20000 siempre nos equivocamos para arriba y para abajo
    #en torno a 40000 siempre nos equivocamos para abajo
    #nos falta alguna variable o meter algo
PlotModelDiagnosis(fTR, fTR$charges, fTR_eval$lm_pred,
                  together = TRUE)
#We can see that residuals depend on bmi values. The behavior with bmi<30 seems to be different than for bmi>30
#The residuals vs age suggests that age might have a cuadratic behavior.


#Plot charges vs bmi
ggplot(fTR_eval)+geom_point(aes(x=bmi,y=charges),alpha = 0.5)+
  geom_point(aes(x=bmi,y=lm_pred), alpha =0.5, color="red")
#Notice that, as there is no interaction, smoker only changes the level, but not the slope


#Training and validation errors
caret::R2(fTR_eval$lm_pred,fTR_eval$charges)
caret::R2(fTS_eval$lm_pred,fTS_eval$charges)




#We want to create square variables for age 
#we could add the square to the dataset or use poly() in the formula (See later)
#fTR$age2 <- fTR_DV$age^2

#Second model with squared variable 
set.seed(150) #For replication
lm2.fit = train(form = charges ~ poly(age,2,raw = TRUE) + children + bmi + smoker, 
               data = fTR, 
               method = "lm", #Linear model
               tuneGrid = data.frame(intercept = TRUE), 
               #preProcess = c("center","scale"),
               trControl = ctrl_tune, 
               metric = "RMSE")
lm2.fit #information about the resampling settings
summary(lm2.fit$finalModel)  #information about the model trained
#age is not significant, only age^2

fTR_eval$lm2_pred <- predict(lm2.fit,newdata = fTR)
fTS_eval$lm2_pred <- predict(lm2.fit,newdata = fTS)

PlotModelDiagnosis(fTR, (fTR$charges), fTR_eval$lm2_pred,together = TRUE)
#bmi still shows different behaviors

#Not much change appreciated
ggplot(fTR_eval)+geom_point(aes(x=bmi,y=charges))+geom_point(aes(x=bmi,y=lm2_pred), color="red")
#independientemente de si fumas o no, el bmi tiene una tendencia de que cuando sube sube el charge


#Training and validation errors
caret::R2(fTR_eval$lm2_pred,fTR_eval$charges)
caret::R2(fTS_eval$lm2_pred,fTS_eval$charges)




# Next model introducing interaction
set.seed(150) #For replication
lm3.fit = train(form = charges ~ poly(age,2,raw = TRUE) + children + bmi*smoker, 
               data = fTR, 
               method = "lm", #Linear model
               tuneGrid = data.frame(intercept = TRUE), 
               #preProcess = c("center","scale"),
               trControl = ctrl_tune, 
               metric = "RMSE")
lm3.fit #information about the resampling settings
summary(lm3.fit$finalModel)  #information about the model trained
#interaction is significant


fTR_eval$lm3_pred <- predict(lm3.fit,newdata = fTR)
fTS_eval$lm3_pred <- predict(lm3.fit,newdata = fTS)

PlotModelDiagnosis(fTR, (fTR$charges), fTR_eval$lm3_pred,together = TRUE)
#bmi still shows different behaviors

#interaction changes slope with smoker, but same for all bmi
ggplot(fTR_eval)+geom_point(aes(x=bmi,y=charges))+geom_point(aes(x=bmi,y=lm3_pred), color="red")
    

#Training and validation errors
caret::R2(fTR_eval$lm3_pred,fTR_eval$charges)
caret::R2(fTS_eval$lm3_pred,fTS_eval$charges)



##Vemos que el comportamiento cambia cuado el BMI es mayor que 30
#We want to create a 0-1 variable for determining if we are in the case when bmi>30 or not
fTR$bmi30 <- ifelse(fTR$bmi >= 30, 1 , 0)
fTS$bmi30 <- ifelse(fTS$bmi >= 30, 1 , 0)


# Next model
set.seed(150) #For replication
lm4.fit = train(form = charges ~ poly(age,2,raw = TRUE) + children + bmi*smoker + bmi30*smoker, 
               data = fTR, 
               method = "lm", #Linear model
               tuneGrid = data.frame(intercept = TRUE), 
               #preProcess = c("center","scale"),
               trControl = ctrl_tune, 
               metric = "RMSE")
lm4.fit #information about the resampling settings
summary(lm4.fit$finalModel)  #information about the model trained
#second interaction is significant


fTR_eval$lm4_pred <- predict(lm4.fit,newdata = fTR)
fTS_eval$lm4_pred <- predict(lm4.fit,newdata = fTS)

PlotModelDiagnosis(fTR, (fTR$charges), fTR_eval$lm4_pred,together = TRUE)
#bmi shows a different behavior, but this one is not explained by any input variable 
  ##EN EL HISTOGRAMA VEMOS PUNTOS CON RESIDUOS MUY ALTOS

#interaction changes slope with smoker and bmi30
ggplot(fTR_eval)+geom_point(aes(x=bmi,y=charges))+geom_point(aes(x=bmi,y=lm4_pred), color="red")

#Training and validation errors
caret::R2(fTR_eval$lm4_pred,fTR_eval$charges)
caret::R2(fTS_eval$lm4_pred,fTS_eval$charges)



#if we consider those extrange values as outliers:
oulierRes <- abs(fTR$charges-fTR_eval$lm4_pred)<3000
fTRFilter<-fTR[oulierRes,]

# Next model
set.seed(150) #For replication
lm5.fit = train(form = charges ~ poly(age,2,raw = TRUE) + children + bmi*smoker + bmi30*smoker, 
                data = fTRFilter, 
                method = "lm", #Linear model
                tuneGrid = data.frame(intercept = TRUE), 
                #preProcess = c("center","scale"),
                trControl = ctrl_tune, 
                metric = "RMSE")
lm5.fit #information about the resampling settings
summary(lm5.fit)
#Our model would obtain a perfect fit (Remember, these are sinthetic data)

#Review fit. All points are modeled except those that are outliers
fTR_eval$lm5_pred <- predict(lm5.fit,newdata = fTR)
ggplot(fTR_eval)+geom_point(aes(x=bmi,y=charges))+geom_point(aes(x=bmi,y=lm5_pred), color="red")


#Here is a visualization the different regression lines
ggplot(fTR,aes(x=bmi,y=charges, color = interaction(smoker,bmi30)))+geom_point()+geom_smooth(method="lm")


#As there are some categories that did not seem significant, we could separate categorical variables
#using dummyVars()
dummyModel <- dummyVars(charges~., data = fdata, fullRank = TRUE) #fullRank=TRUE eliminates base category from factor variables
#dummyModel contains the transformation to be applied to a dataset. Use predict() to apply it.
fTR_DV <- as.data.frame(predict(dummyModel, fTR))
fTS_DV <- as.data.frame(predict(dummyModel, fTS))

#watch the effect. We would have to add the output
str(fTR_DV)
fTR_DV$charges <- fTR$charges
fTS_DV$charges <- fTS$charges


fTR_eval = fTR_DV
fTS_eval = fTS_DV

fTR_eval$lm5_pred <- predict(lm5.fit,newdata = fTR)
fTS_eval$lm5_pred <- predict(lm5.fit,newdata = fTS)


#Training and validation errors
caret::R2(fTR_eval$lm5_pred,fTR_eval$charges)
caret::R2(fTS_eval$lm5_pred,fTS_eval$charges)








