#################################################################################
##############           S3 ARIMA II Example         ############################
##############     ----------- solution ---------    ############################
#################################################################################

library(MLTools)
library(fpp2)
library(ggplot2)
library(readxl)
library(lmtest)  #contains coeftest function
library(tseries) #contains adf.test function

## Set working directory ---------------------------------------------------------------------------------------------


## Load dataset -------------------------------------------------------------------------------------------------------
fdata <- read_excel("ARIMA_II.xls")
# Convert to time series object
fdata_ts <- ts(fdata)
# index to select a time series
y <- fdata_ts[,2]

## Load custom functions ---------------------------------------------------------------
# source("ForecastingTools.R")


## Identification and fitting frocess -------------------------------------------------------------------------------------------------------
autoplot(y)
    #vemos que ni la media ni la varianza son estables

# Box-Cox transformation
Lambda <- BoxCox.lambda.plot(y,10)  #pinta boxcox usando ventana de 10
                                    #si fuese serie diaria 7 o 14
                                    #si fuese mensual 12
    #VEMOS QUE HAY RELACION CRECIENTE ENTRE LOG(MEDIA) Y LOG(SD)
    #aunque hay bastante ruido
    #R2 = 0.5 (se explica el 50% de la varianza)
    #lamba cerca de 0 = TRANFORMACION DE BOX-COX CORRESPONDE CON EL LOG


#ESTABILIZAR VARIANZA
# Lambda <- BoxCox.lambda(y) #other option
z <- BoxCox(y,Lambda) #APlica transformacion de BOX-COX (log porque lamba es casi 0) PARA ESTABILIZAR LA VAR
autoplot(z)

#CHEQUEAR SI HAY QUE ESTABILIZAR LA MEDIA
# Differentiation: if the ACF decreases very slowly -> needs differenciation
ggtsdisplay(z,lag.max = 25) #Serie de logaritmos vemos que ACF SLOW DECREASE = DIFFERENCIA!
# Alternative test
adf.test(y, alternative = "stationary")
      #p-value>0.05 = NEEDS DIFFERENCIATION
ndiffs(y) #NOS DICE EL NUMERO DE DIFERENCIACIONES NECESARIAS PARA ESTABILIZAR

#ESTABILIZAR MEDIA
# If differencing is needed
Bz <- diff(z,differences = 1)

# ACF and PACF of the time series -> identify significant lags and order
ggtsdisplay(Bz,lag.max = 25) 
      #tenemos media y varianza constante  = SERIE CONSTANTE = MODELAR CON ARMA
          #No es AR puro ya que en PACF hay decrecimiento exponencial AR1
          #ACF el segundo es mayor que el primero MA = 2 
     

##################
##MODELOS#########
##################

#1
#SI NOS EQUIVOCAMOS ARMA2,2
# Fit model with estimated order
arima.fit <- Arima(y, #pasamos la serie original!!
                   order=c(2,1,2), 
                   lambda = Lambda,
                   include.constant = TRUE) #CUANDO SE DIFERENCIA LA CONST DESAPARECE
summary(arima.fit) #summary of training errors and estimated coefficients
coeftest(arima.fit) #statistical significance of estimated coefficients
  #Nos dice que nos sobra el AR2 y la media del modelo (drif=include constant )
  #QUEREMOS p-valor por debajo de 0.05
  #obvio nos hemos equivocado

#2
#MODELO CORRECTO!
# Fit model with estimated order
arima.fit <- Arima(y, #pasamos la serie original!!
                   order=c(1,1,2), 
                   lambda = Lambda,
                   include.constant = FALSE)
summary(arima.fit) #summary of training errors and estimated coefficients
coeftest(arima.fit) #statistical significance of estimated coefficients
  #todos relevantes

autoplot(arima.fit) #root plot
      #QUEREMOS PUNTOS DENTRO = estacionario
    
# Check residuals
CheckResiduals.ICAI(arima.fit, bins = 100)
      #como el p-valor>0.05 rechazamos que sea ruid blanco (creo?)

#######

#Check  forecast
autoplot(y, series = "Real")+
  forecast::autolayer(arima.fit$fitted, series = "Fitted")

#Perform future forecast
y_est <- forecast(arima.fit, h=12)
autoplot(y_est)


