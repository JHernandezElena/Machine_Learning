#################################################################################
##############       LabPractice 4.5 Forecasting     ############################
##############     ----------- solution ---------    ############################
#################################################################################

library(MLTools)
library(fpp2)
library(ggplot2)
library(readxl)
library(lmtest)  #contains coeftest function
library(tseries) #contains adf.test function

## Set working directory ---------------------------------------------------------------------------------------------

## Load custom functions ---------------------------------------------------------------
# source("ForecastingTools.R")

## Load dataset -------------------------------------------------------------------------------------------------------
fdata <- read_excel("CarRegistrations.xls")
# fdata <- read.table("CarRegistrations.dat",header = TRUE)
# Convert to time series object (coGEMOS LA TS DE CARREG)
y <- ts(fdata$CarReg,start = 1960, frequency = 12) #variable a predecir, ano de inicio y periodo
# for daily data
autoplot(y)


## Training and validation ------------------------------------------------------------
y.TR <- subset(y, end = length(y)-5*12) #Leave 5 years for validation
y.TV <- subset(y, start = length(y)-5*12+1)

## Identification and fitting frocess -------------------------------------------------------------------------------------------------------
autoplot(y.TR)

# Box-Cox transformation
Lambda <- BoxCox.lambda.plot(y.TR,12)
      #r2 muy alta (varianza muy explicada) y relacion lineal = NECESITA ESTABILIZAR VAR
      #lamba es practicamente 0 = transformacion logaritmica = trans de Box-Cox

#ESTABILIZAMOS LA VARIANZA
z <- BoxCox(y.TR,Lambda)
autoplot(z)

#DIFERENCIACION
ggtsdisplay(z,lag.max = 100)
    #ACF decreases very slowly -> needs differenciation

      #REGULAR PRIMERO (da igual el orden)
      # If differencing is needed
      Bz <- diff(z,differences = 1)
      ggtsdisplay(Bz,lag.max = 100) #differences contains the order of differentiation
      
      #ESTACIONAL SEGUNDO
      # Seasonal Differentiation
      # If differencing is needed
      B12Bz <- diff(Bz, lag = 12, differences = 1) #si pusieramos una y en vez de BZ hariamos solo la estacional
      ggtsdisplay(B12Bz,lag.max = 100)


#COMPRPBAMOS QUE TENEMOS UNA SERIE ESTABLE EN MEDIA Y VARIANZA      
# ACF and PACF of the time series -> identify significant lags and order
ggtsdisplay(B12Bz,lag.max = 100)
        #ACF Y PACF DESAPARECEN MUY RAPIDO - NO HAY QUE DIFERENCIAR MAS
        #Aparecen unos picos en la primera grafica que puden ser OUTLIERS
              #aprenderemos a eliminarlas o sustituirlas 
    
  #PARA DECIDIR MODELO ESTACIONAL NOS FIJAMOS SOLO EN LSO PERIODOS
      #En acf 12 y 24 son significativos [MA1(si solo hubiese uno rel)]
              #entonces es MA2 o ARMA 1,1 )
      #En pacf 12, 24 y 36 significativos


#############
##MODELOS ###
#############

##MODELOS PARA PARTE ESTACIONAL##

#MA2 estacional?
arima.fit <- Arima(y.TR,
                   order=c(0,1,0),
                   seasonal = list(order=c(0,1,2),period=12),
                   lambda = Lambda,
                   include.constant = FALSE)
summary(arima.fit) # summary of training errors and estimated coefficients
coeftest(arima.fit) # statistical significance of estimated coefficients
      #AMBOS COEFICIENTES RELEVANTES
autoplot(arima.fit) # root plot

# Check residuals
CheckResiduals.ICAI(arima.fit, bins = 100)
    #NOS FIJAMOS EN LOS MUTIPLOS DEL PERIODO
    #ggtsdisplay(residuals(arima.fit),lag.max = 100) para anadir eje
        #HAN DEJADO DE SER RELVANRES EN ACF Y PACF
    #EVALUAMOS PARTE REGULAR
        #PACF decrecimiento exponencial
        #ACF un relevante
        #MA1 regular?
  
#MA1 refular MA2 estacional
arima.fit <- Arima(y.TR,
                   order=c(0,1,1),
                   seasonal = list(order=c(0,1,2),period=12),
                   lambda = Lambda,
                   include.constant = FALSE)
summary(arima.fit) # summary of training errors and estimated coefficients
coeftest(arima.fit) # statistical significance of estimated coefficients
    #TODOS COEFICIENTES RELEVANTES
autoplot(arima.fit) # root plot
      #p-valor>0.05 = bien

# Check residuals
CheckResiduals.ICAI(arima.fit, bins = 100)
      #todos dentro = bien

#Podriamos probar ARMA11 en la parte estacional

#######################
##EVALUAR BUEN MODELO##
#######################
# Check fitted
autoplot(y.TR, series = "Real")+
  forecast::autolayer(arima.fit$fitted, series = "Fitted")


# Perform FUTURE FORECAST
y_est <- forecast(arima.fit, h=12)
autoplot(y_est)

## Validation error for h = 1 (ERROR DE PREDICCION A 1 PASO CON EL CONJUNTO DE VAL) -------------------------------------------------------------------------------------------------------
# Obtain the forecast in validation for horizon = 1 using the trained parameters of the model
y.TV.est <- y*NA
for (i in seq(length(y.TR)+1, length(y), 1)){# loop for validation period
  y.TV.est[i] <- forecast(subset(y,end=i-1), # y series up to sample i 
                    model = arima.fit,       # Model trained (Also valid for exponential smoothing models)
                    h=1)$mean                # h is the forecast horizon
}

#Plot series and forecast 
autoplot(y)+
  forecast::autolayer(y.TV.est)

#Compute validation errors
accuracy(y.TV.est,y)
