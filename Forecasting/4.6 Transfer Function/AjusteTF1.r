#################################################################################
##############      Lab 4.6:   Transfer Function     ############################
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
fdata <- read.table("TF1.dat",header = TRUE, sep = "")
# Convert to time series object
fdata_ts <- ts(fdata)
autoplot(fdata_ts, facets = TRUE)
# Create time series and scale values 
y <- ts(fdata$y)/10 #porque y esta entre +-20
x <- ts(fdata$x)/1 #porque x esta entre +-2



## Identification and fitting process -------------------------------------------------------------------------------------------------------

#############################################
#### 1. Fit initial FT model with large s#######
#############################################
# This arima function belongs to the TSA package
TF.fit <- arima(y,
                order=c(1,1,0), #*(1,0,0) inicialmente
                #seasonal = list(order=c(1,0,0),period=24),
                xtransf = x,
                transfer = list(c(0,9)), #List with (r,s) orders
                include.mean = TRUE,
                method="ML")
summary(TF.fit) # summary of training errors and estimated coefficients
coeftest(TF.fit) # statistical significance of estimated coefficients
# Check regression error to see the need of differentiation
TF.RegressionError.plot(y,x,TF.fit,lag.max = 100)
    #*PARECE QUE NECESITAMOS APLICAR UNA DIFERENCIA ESTACIONAL PORQUE ACF DECRECE LENTAMENE (1,1,0)
                        

###########################
#2a. MODELAMOS EL RUIDO####
###########################
TF.RegressionError.plot(y,x,TF.fit,lag.max = 100)
  #NOTE: If this regression error is not stationary in variance,boxcox should be applied to input and output series.
  #*miramos y parece arima (2,1,1) *hemos hecho mil pruebas en el paso 3


##########################
#2b. MODELAMOS B, S Y R###
##########################
TF.Identification.plot(x,TF.fit)
    #b=1 (un coeficiente 0)
    #r=1 (decrece linealmente)
    #s=0 (decrece desde el principio) 


#######################################################################
#### 3. Fit arima con las b, s y r y el modelado del ruido (2a, 2b)####
#######################################################################
##*Habra que reajustar el ruido
xlag = Lag(x,1)   # b
xlag[is.na(xlag)]=0
arima.fit <- arima(y,
                   order=c(2,1,1), #ajustado desde 2a y haciendo ajustes posteriores
                   xtransf = xlag,
                   transfer = list(c(1,0)), #List with (r,s) orders
                   include.mean = FALSE,
                   method="ML")
summary(arima.fit) # summary of training errors and estimated coefficients
coeftest(arima.fit) # statistical significance of estimated coefficients
# Check residuals
CheckResiduals.ICAI(arima.fit)
# If residuals are not white noise, change order of ARMA
ggtsdisplay(residuals(arima.fit),lag.max = 50)


### Cross correlation residuals - expl. variable
res <- residuals(arima.fit)
res[is.na(res)] <- 0
ccf(y = res, x = x)
########

# Check fitted
autoplot(y, series = "Real")+
  forecast::autolayer(fitted(arima.fit), series = "Fitted")

