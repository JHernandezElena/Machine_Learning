#################################################################################
##############      Lab 4.5:   Transfer Function     ############################
##############     ----------- solution ---------    ############################
#################################################################################

library(MLTools)
library(fpp2)
library(ggplot2)
library(TSA)
library(lmtest)  #contains coeftest function
library(tseries) #contains adf.test function
library(Hmisc) # for computing lagged variables

## Set working directory ---------------------------------------------------------------------------------------------


## Load dataset -------------------------------------------------------------------------------------------------------
fdata <- read.table("F50.dat",header = TRUE, sep = "")
colnames(fdata) <- c("Y","X")
# Convert to time series object
fdata_ts <- ts(fdata)
autoplot(fdata_ts, facets = TRUE)
# Create time series and scale values LAS ESCALAMOS PARA QUE SEAN DE ORDEN 1
y <- fdata_ts[,1]/10000 #PORQUE SON VALORES DE HASTA 70000 = 10^4 EN EL AUTOPLOT
x <- fdata_ts[,2]/10000



## Identification and fitting process -------------------------------------------------------------------------------------------------------

##########################################
#### 1. Fit initial FT model with large s#
##########################################
# This arima function belongs to the TSA package
TF.fit <- arima(y,
                order=c(1,0,0), #ARIMA de la v(t) lo pondre,os un 1 o un 2
                seasonal = list(order=c(1,0,0),period=24), #la serie original era periodica y tambien usamos AR1
                xtransf = x, #B=0 
                transfer = list(c(0,9)), #List with (r,s) orders #S=9 MUCHOS RETARDOS DE LA X PARA VER EL RETARDO IMPULSIONAL
                include.mean = TRUE,
                method="ML")
summary(TF.fit) # summary of training errors and estimated coefficients
coeftest(TF.fit) # statistical significance of estimated coefficients
        #Irrelevante ya que hemos puesto la b,s,r a voleo

# Check regression error to see the need of differentiation
TF.RegressionError.plot(y,x,TF.fit,lag.max = 100)
#NOTE: If this regression error is not stationary in variance,boxcox should be applied to input and output series.
      #PARECE QUE NECESITAMOS APLICAR UNA DIFERENCIA ESTACIONAL PORQUE SALEN RELEVANTES LOS PERIODOS


#APLICAMOS LA DIFERENCIA ESTACIONAL# (*O REG SI FUERA EL CASO)
TF.fit <- arima(y,
                order=c(1,0,0), #ARIMA de la v(t) lo pondre,os un 1 o un 2
                seasonal = list(order=c(1,1,0),period=24), #la serie original era periodica y tambien usamos AR1
                xtransf = x, #B=0 
                transfer = list(c(0,9)), #List with (r,s) orders #S=9 MUCHOS RETARDOS DE LA X PARA VER EL RETARDO IMPULSIONAL
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
            #Tenemos una componente Estacional(ACF dec exp, PACF un relvante -> AR1)
            #Y una componente regular (ACF tiene un pico -> MA1)


#########################
#2b. MODELAMOS B, S Y R##
#########################
# Check numerator coefficients of explanatory variable
TF.Identification.plot(x,TF.fit)
  #B = #coef inciales que son 0
  #r=2 decrece sinoidal mente, r=1 lineal, r=0 no decrece
  #s = a partir de cual empieza a descender **SIN contar los 0s**
      #B=0 (no hay coef = 0 al principio) 
      #s=0 
      #r=2 decrece sinoidal mente, 


######################################################################
####3. Fit arima con las b, s y r y el modelado del ruido (2a, 2b)####
######################################################################
##*QUiza habra que reajustar el ruido
xlag = Lag(x,0)   # b= 0
xlag[is.na(xlag)]=0
arima.fit <- arima(y,
                   order=c(0,0,1), #MA1 regular 
                   seasonal = list(order=c(1,1,0),period=24), #AR1 estacional
                   xtransf = xlag,
                   transfer = list(c(0,0)), #r y s = 0
                   include.mean = FALSE,
                   method="ML")
summary(arima.fit) 
coeftest(arima.fit) 
    #Todos relevantes
CheckResiduals.ICAI(arima.fit)
    #Todas las varras dentro
    #p-valor Ljung-box>0.05 = no puedo rechazar la hipotesis de que sea ruido blanco = bien
ggtsdisplay(residuals(arima.fit),lag.max = 50) #EL LAG DEVE SER CERCANO A UN MULTIPLO DEL PERIODO (48)
    #misma que la anterior


### Cross correlation residuals - expl. variable
res <- residuals(arima.fit)
res[is.na(res)] <- 0
ccf(y = res, x = x)
      #Todos dentro por lo que no hay auto-correlacion = BIEN TB


# Check fitted
autoplot(y, series = "Real")+
  forecast::autolayer(fitted(arima.fit), series = "Fitted")

