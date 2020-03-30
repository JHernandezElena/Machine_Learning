library(MLTools)
library(fpp2)
library(ggplot2)
library(readxl)
library(lmtest)  #contains coeftest function
library(tseries) #contains adf.test function

fdata <- read_excel("SARIMA.xls")

#PASAMOS LAS DOS SERIES A TS
fdata_ts <- ts(fdata)
#ELEGIMOS LA 1 TS
y <- fdata_ts[,1]
#PLOT DE LA TS1
autoplot(y)

ggtsdisplay(y,lag.max = 1000)

##########################
##BUSCAMOS PERIODICIDAD###
##########################
#Hacemos transformaciones y miramos ACF 
#Donde haya picos en ACF = periodo

# Box-Cox transformation
Lambda <- BoxCox.lambda.plot(y,12)
    #r2 muy alta (varianza muy explicada) y relacion lineal = NECESITA ESTABILIZAR VAR
    #lamba es practicamente 0 = transformacion logaritmica = trans de Box-Cox

#ESTABILIZAMOS LA VARIANZA
z <- BoxCox(y,Lambda)
autoplot(z)
ggtsdisplay(z,lag.max = 100) #parece haber periodicidad 7!!
        #ACF decreases very slowly -> needs differenciation

#ESTABILIZAR MEDIA
# If differencing is needed
Bz <- diff(z,differences = 1)

#ACF and PACF of the time series -> identify significant lags and order
ggtsdisplay(Bz,lag.max = 25) 
    #tenemos media y varianza constante
    #VEMOS PICOS EN CADA 7!!  PERIODO 7




###########################
##PERIODO 7 IDENTIFICADO###
###########################

# Box-Cox transformation
Lambda <- BoxCox.lambda.plot(y,7)
  #r2 muy alta (varianza muy explicada) y relacion lineal = NECESITA ESTABILIZAR VAR
  #lamba es practicamente 0 = transformacion logaritmica = trans de Box-Cox

#ESTABILIZAMOS LA VARIANZA
z <- BoxCox(y,Lambda)
ggtsdisplay(z,lag.max = 100) 
      #ACF disminuye lentamente

#ESTABILIAR MEDIA - DIFERENCIAR
#REGULAR PRIMERO (da igual el orden)
# If differencing is needed
Bz <- diff(z, differences = 1)
ggtsdisplay(Bz,lag.max = 100) 
    #Vemos cada 7 hay picos - diferenciamos estacional

# If differencing is needed
B7Bz <- diff(Bz, lag = 7, differences = 1) 
ggtsdisplay(B7Bz,lag.max = 30)

###########################
###Ajuste parte estacional#
###########################

##CADA 7:
#Vemos en ACF que se escapa 1 MA1
#Vemos que PACF decrece exponencial
#MA2 estacional
arima.fit <- Arima(y,
                   order=c(0,1,0),
                   seasonal = list(order=c(0,1,1),period=7),
                   lambda = Lambda,
                   include.constant = FALSE)
summary(arima.fit) # summary of training errors and estimated coefficients
coeftest(arima.fit) # statistical significance of estimated coefficients
#COEFICIENTES RELEVANTES
autoplot(arima.fit) # root plot

# Check residuals
CheckResiduals.ICAI(arima.fit, bins = 100)
      #LOS DE ORDEN 7 ESTAN TODOS BIEN
      #EL REGULR PODRIA SER MA3


###########################
###Ajuste parte regular#
###########################
#MA3 regular?
arima.fit <- Arima(y,
                   order=c(0,1,3),
                   seasonal = list(order=c(0,1,1),period=7),
                   lambda = Lambda,
                   include.constant = FALSE)
summary(arima.fit) # summary of training errors and estimated coefficients
coeftest(arima.fit) # statistical significance of estimated coefficients
#COEFICIENTES RELEVANTES
autoplot(arima.fit) # root plot

# Check residuals
CheckResiduals.ICAI(arima.fit, bins = 100)
#ESTAN TODOS BIEN!


########
#Podriamos porbar 112 en la parte regular
########











