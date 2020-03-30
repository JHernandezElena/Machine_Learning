#################################################################################
##############       Forecasting:      ARMA          ############################
##############    CLASIFICACION DE LOS DISTINTOS TS (YO)   ###########################
#################################################################################



library(MLTools)
library(fpp2)
library(ggplot2)
library(readxl)
library(lmtest) #contains coeftest function

## Set working directory ---------------------------------------------------------------------------------------------

## Load custom functions ---------------------------------------------------------------
# source("ForecastingTools.R")

## Load dataset -------------------------------------------------------------------------------------------------------
fdata <- read_excel("ARMA_series.xls")
# Convert to time series object (HAY MUCHAS SERIES)
fdata_ts <- ts(fdata)


##############
##TERCERA TS##
##############
y <- fdata_ts[,3]

## Identification and fitting frocess -------------------------------------------------------------------------------------------------------
# ACF and PACF of the time series -> identify significant lags and order
ggtsdisplay(y,lag.max = 25)
      #mirar diap 36, 4.2 (PARECE ARMA11)


# Fit model with estimated order
arima.fit <- Arima(y, order=c(1,0,1), include.mean = FALSE)  
summary(arima.fit) # summary of training errors and estimated coefficients
  #EL AIC CUANTO MAS PEQUENO SEA MEJOR
coeftest(arima.fit) # statistical significance of estimated coefficients
  #NOS DICE QUE EL COEF AR1  TIENE UN P-VALOR MUY BAJO(muy relevante)
autoplot(arima.fit) # root plot
  #EL INVERSO DE LA RAIZ TIENE QUE ESTAR DENTRO
  #COMO EL PUNTO ROJO ESTA DENTRO - PROCESO INVERTIBLE
  #SI ESTUVIERA FUERA SERIA NO INVERTIBLE


# Check residuals
CheckResiduals.ICAI(arima.fit, bins = 100, lag=20)
  #en el test Ljung-Box test no podemos rechazar la hipotesis de que el resiudo sea ruido blanco (p-valor<0.5)
  #POR TANTO EL RUIDO NO PRESENTA CORRELACIONES 
  
  #BIEN (=ruido sin correlacion) -> grafico dentro de lineas + p-valor>0.05
  # If residuals are not white noise, change order of ARMA

ggtsdisplay(residuals(arima.fit),lag.max = 25)


############
##SEXTA TS##
############
y <- fdata_ts[,6]

## Identification and fitting frocess -------------------------------------------------------------------------------------------------------
# ACF and PACF of the time series -> identify significant lags and order
ggtsdisplay(y,lag.max = 25)
#mirar diap 36, 4.2


# Fit model with estimated order
arima.fit <- Arima(y, order=c(0,0,1), include.mean = FALSE)  
summary(arima.fit) # summary of training errors and estimated coefficients
#EL AIC CUANTO MAS PEQUENO SEA MEJOR
coeftest(arima.fit) # statistical significance of estimated coefficients
#NOS DICE QUE EL COEF AR1  TIENE UN P-VALOR MUY BAJO(muy relevante)
autoplot(arima.fit) # root plot
#EL INVERSO DE LA RAIZ TIENE QUE ESTAR DENTRO
#COMO EL PUNTO ROJO ESTA DENTRO - PROCESO INVERTIBLE
#SI ESTUVIERA FUERA SERIA NO INVERTIBLE


# Check residuals
CheckResiduals.ICAI(arima.fit, bins = 100, lag=20)
#en el test Ljung-Box test no podemos rechazar la hipotesis de que el resiudo sea ruido blanco (p-valor<0.5)
#POR TANTO EL RUIDO NO PRESENTA CORRELACIONES 
#BIEN (=ruido sin correlacion) -> grafico dentro de lineas + p-valor>0.05
# If residuals are not white noise, change order of ARMA

ggtsdisplay(residuals(arima.fit),lag.max = 25)



##############
##RESULTADOS##
##############
#serie 1: AR1
#serie 2: MA1
#serie 3: ARMA(1,1)
#serie 4: AR2
#serie 5: ARMA(1,1)
#serie 6: MA1