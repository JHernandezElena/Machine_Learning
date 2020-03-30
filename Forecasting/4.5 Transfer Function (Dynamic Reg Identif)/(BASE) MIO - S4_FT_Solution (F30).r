#################################################################################
##############      Lab 4.5:   Transfer Function     ############################
#################################################################################

library(MLTools)
library(fpp2)
library(ggplot2)
library(TSA)
library(lmtest)  
library(tseries)
library(Hmisc) 



## Load dataset -------------------------------------------------------------------------------------------------------
fdata <- read.table("F30.dat",header = TRUE, sep = "")
colnames(fdata) <- c("Y","X")

# Convert to time series object
fdata_ts <- ts(fdata)
autoplot(fdata_ts, facets = TRUE)

# Create time series and scale values LAS ESCALAMOS PARA QUE SEAN DE ORDEN 1
y <- fdata_ts[,1]/10000 #PORQUE SON VALORES DE HASTA 70000 = 10^4 EN EL AUTOPLOT
x <- fdata_ts[,2]/10000


## Identification and fitting process -------------------------------------------------------------------------------------------------------

###*Estabilizar Varianza Box-Cox??

##########################################
#### 1. Fit initial FT model with large s#
##########################################
# This arima function belongs to the TSA package
TF.fit <- arima(y,
                order=c(1,0,0), #ARIMA de la v(t) lo pondre,os un 1 o un 2
                seasonal = list(order=c(1,0,0),period=24), #la serie original era periodica y tambien usamos AR1
                xtransf = x, #B=0 
                transfer = list(c(0,15)), #List with (r,s) orders #S=9 MUCHOS RETARDOS DE LA X PARA VER EL RETARDO IMPULSIONAL
                include.mean = TRUE,
                method="ML")
summary(TF.fit) 
coeftest(TF.fit) 
    #Pusimos s=9 y todos parecian relvantes, ponemos un 15 a ver

# Check regression error to see the need of differentiation
TF.RegressionError.plot(y,x,TF.fit,lag.max = 100)
#NOTE: If this regression error is not stationary in variance,boxcox should be applied to input and output series.
    #PARECE QUE NECESITAMOS APLICAR UNA DIFERENCIA REGULAR PORQUE SALEN RELEVANTES LOS PERIODOS


#APLICAMOS LA DIFERENCIA REGULAR###
TF.fit <- arima(y,
                order=c(2,1,0), #ARIMA de la v(t) PROVAMOS UN 2
                seasonal = list(order=c(1,0,0),period=24), 
                xtransf = x, #B=0 
                transfer = list(c(0,12)), #List with (r,s) orders #S=12 porque 15 eran muchos
                include.mean = TRUE,
                method="ML")
summary(TF.fit) #summary of training errors and estimated coefficients
coeftest(TF.fit) #Irrelevante ya que hemos puesto la b,s,r a voleo


###########################
#2a. MODELAMOS EL RUIDO####
###########################
TF.RegressionError.plot(y,x,TF.fit,lag.max = 100)
    #la SERIE YA ES ESTACIONAL YA QUE LA ACF SE VA RAPIDAMENTE A 0
    #Aun asi no es ruido blanco
    #Tenemos una componente regular (PACF dec exp, ACF dos relvante -> MA2)
    #NO PARECE HABER hay componente estacional

##########################
#2b. MODELAMOS B, S Y R###
##########################
# Check numerator coefficients of explanatory variable
TF.Identification.plot(x,TF.fit)
    #B = as the number of samples it takes for the output to respond to the input.
      #B NUMERO DE PICOS = 0 (1,2.)
    #R = determines the pattern of decay in the impulse response weights
      #R=0 NO HAY DECRECIMIENTO
      #R=1 DECECIMIENTO EXP
      #R=2 DECREC SINOIDAL
    #S = determines where the pattern of decay in the impulse response weights begins
      #S = # DE MUESTRAS HASTA EL PICO MAS ALTO (1,2)
  


#######################################################################
#### 3. Fit arima con las b, s y r y el modelado del ruido (2a, 2b)####
#### quitando la componente estacional si no es necesaria #############
#####################################################
##*Habra que reajustar el ruido
xlag = Lag(x,0)   # b= 0
xlag[is.na(xlag)]=0
arima.fit <- arima(y,
                   order=c(0,0,2), #MA2 regular
                   xtransf = xlag,
                   transfer = list(c(2,0)), #r=2 y s = 0
                   include.mean = FALSE,
                   method="ML")
summary(arima.fit) 
coeftest(arima.fit) 
CheckResiduals.ICAI(arima.fit)
    


################################
##Ajuste FINAL ruido###########
################################
arima.fit <- arima(y,
                   order=c(1,0,2), #ARMA,1,2
                   xtransf = xlag,
                   transfer = list(c(2,0)), #r=2 y s = 0
                   include.mean = FALSE,
                   method="ML")
summary(arima.fit) 
coeftest(arima.fit) 
CheckResiduals.ICAI(arima.fit) #**1


### Cross correlation residuals - expl. variable
res <- residuals(arima.fit)
res[is.na(res)] <- 0
ccf(y = res, x = x) #**2
    

#.	MAL TRANSFER Y ARIMA: correlación mirando la ACF cruzada entre las x(t) y el residuo final del modelo #**2
#.	MAL TRANSFER: correlación mirando la ACF cruzada entre las x(t) y el residuo final del modelo y correlación en residuos #**1 y **2
#.	MAL ARIMA: solo correlación en los residuos #**1



