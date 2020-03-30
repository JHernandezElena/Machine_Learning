library(MLTools)
library(fpp2)
library(ggplot2)
library(TSA)
library(lmtest)  #contains coeftest function
library(tseries) #contains adf.test function
library(Hmisc) # for computing lagged variables

## Set working directory ---------------------------------------------------------------------------------------------


## Load dataset -------------------------------------------------------------------------------------------------------
fdata <- read.table("TF2.dat",header = TRUE, sep = "")
# Convert to time series object
fdata_ts <- ts(fdata)
autoplot(fdata_ts, facets = TRUE)

# Create time series and scale values 
y <- ts(fdata$y)/10 #porque y esta entre +-20
x <- ts(fdata$x)/1 #porque x esta entre +-2


#############################################
#### 1. Fit initial FT model with large s#######
#############################################
TF.fit <- arima(y,
                order=c(1,1,0), #(1,0,0) 
                #seasonal = list(order=c(1,0,0),period=24),
                xtransf = x,
                transfer = list(c(0,9)), #List with (r,s) orders
                include.mean = TRUE,
                method="ML")
summary(TF.fit) # summary of training errors and estimated coefficients
coeftest(TF.fit) # statistical significance of estimated coefficients
# Check regression error to see the need of differentiation (en el 2a)
TF.RegressionError.plot(y,x,TF.fit,lag.max = 100)
    #*PARECE QUE NECESITAMOS APLICAR UNA DIFERENCIA ESTACIONAL PORQUE ACF DECRECE LENTAMENE (1,1,0)


###########################
#2a. MODELAMOS EL RUIDO####
###########################
TF.RegressionError.plot(y,x,TF.fit,lag.max = 100)
  #NOTE: If this regression error is not stationary in variance,boxcox should be applied to input and output series.
 


##########################
#2b. MODELAMOS B, S Y R###
##########################
TF.Identification.plot(x,TF.fit)
    #b=2 (dos coeficientes 0)
    #r=0 (no decrece ni lineal ni sinoidal, no decrece) *podria ser r=2
    #s=1 (decrece desde el segundo relevante) 
