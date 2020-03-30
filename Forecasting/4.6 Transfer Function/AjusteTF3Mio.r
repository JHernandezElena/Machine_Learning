library(MLTools)
library(fpp2)
library(ggplot2)
library(TSA)
library(lmtest)  #contains coeftest function
library(tseries) #contains adf.test function
library(Hmisc) # for computing lagged variables

## Set working directory ---------------------------------------------------------------------------------------------


## Load dataset -------------------------------------------------------------------------------------------------------
fdata <- read.table("TF3.dat",header = TRUE, sep = "")
# Convert to time series object
fdata_ts <- ts(fdata)
autoplot(fdata_ts, facets = TRUE)

# Create time series and scale values 
y <- ts(fdata$y)/1 #porque y esta entre +-3
x <- ts(fdata$x)/1 #porque x esta entre +-2

#############################################
#### 1. Fit initial FT model with large s#######
#############################################
TF.fit <- arima(y,
                order=c(1,0,0), 
                #seasonal = list(order=c(1,0,0),period=24),
                xtransf = x,
                transfer = list(c(0,9)), #List with (r,s) orders
                include.mean = TRUE,
                method="ML")
summary(TF.fit) # summary of training errors and estimated coefficients
coeftest(TF.fit) # statistical significance of estimated coefficients
# Check regression error to see the need of differentiation (en el 2a)
TF.RegressionError.plot(y,x,TF.fit,lag.max = 100)
    #no diferenciariamos porque no es TAN LENTO EL DESCENSO


###########################
#2a. MODELAMOS EL RUIDO####
###########################
TF.RegressionError.plot(y,x,TF.fit,lag.max = 100)
#NOTE: If this regression error is not stationary in variance,boxcox should be applied to input and output series.
#no hecho


##########################
#2b. MODELAMOS B, S Y R###
##########################
TF.Identification.plot(x,TF.fit)
#b=1 (dos coeficientes 0)
#r=1 (no decrece ni lineal ni sinoidal, no decrece) *podria ser r=2
#s=0 (decrece desde el principio) 


