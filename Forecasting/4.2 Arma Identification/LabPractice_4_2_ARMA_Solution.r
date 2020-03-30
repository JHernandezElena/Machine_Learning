#################################################################################
##############       Forecasting:      ARMA          ############################
##############     ----------- solution ---------    ############################
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
# index to select a time series (COGEMOS LA SEGUNDA SERIE)
y <- fdata_ts[,2]

autoplot(y)
## Identification and fitting frocess -------------------------------------------------------------------------------------------------------
# ACF and PACF of the time series -> identify significant lags and order
ggtsdisplay(y,lag.max = 25)
    #mirar diap 36, 4.2
    #el proceso parece estacionario en media (media 0)
    #la varianza tambien tiene una variabilidad constante
    #la ACF tiene 1 coeficiente significativo y la PACF tiene decrecimiento exponencial 
        #este proceso Puede corresponder con MA1 

###################################
##MODELO INCORRECTO - ARMA (1,1)###
###################################
# Fit model with estimated order (AR, arima, MA)
arima.fit <- Arima(y, order=c(1,0,1), include.mean = TRUE) #deberia ser 0,0,1, con media (termino constante 0) 
summary(arima.fit) # summary of training errors and estimated coefficients
    #EL AIC CUANTO MAS PEQUENO SEA MEJOR
coeftest(arima.fit) # statistical significance of estimated coefficients
    #NOS DICE SOLO HAY UN COEF CON MUY POCO



########################
##MODELO CORRECTO- MA1##
########################

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
#CONCLUSIONES#
##############
#BAJO AIC (comparado con modelos erroneos)
#COEFICIENTES RELEVANTES (bajo p-valor)
#PUNTO DENTRO DEL CIRCULO = proceso invertible
#RESIUDOS = RUIDO BLANCO
    #DENTRO DE LOS MARGENES
    #TEST DE L-BOX P-VALOR>0.05 (PROBABILIDAD DE QUE LA Q (SUNA DE LOS COEFICIENTES) SEA IGUAL A 0)
    #MEDIA 0

#UN PROCESO DE MEDIA MOVIL LO PUEDO FORZAR A SER AR METIENDO MUCHOS PARAMETROS
    #SI HACEMOS ESTE EJEMPLO CON c(10,0,0) obtenemos muy buenos resultados
    #LA COSA ES QUE HAY QUE HACER EL MODELO MUCHO MAS COMPLEJO


###############################################

# Check fitted forecast
autoplot(y, series = "Real")+
  forecast::autolayer(arima.fit$fitted, series = "Fitted")

#Perform future forecast
y_est <- forecast(arima.fit, h=5)
autoplot(y_est)

#PARA GENERAR DISINTOS PORCESOS ARMA
## Simulate ARMA time series -------------------------------------------------------------------------------------------------------
sim_ts <- arima.sim(n = 250, #numero de ts
                 list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488)),
                 sd = sqrt(0.1796)) #desviacion tipica del ruido
