#################################################################################
##############   LabPractice 5.1 Density estimation  ############################
##############     ----------- solution ---------    ############################
#################################################################################

library(MASS)
library(stats)
library(ggplot2)
library(ggfortify)

## Set working directory ---------------------------------------------------------------------------------------------


## Load dataset -------------------------------------------------------------------------------------------------------
fdata <- read.table("ParametricDensityEstimation.dat",header = TRUE, sep = "")


# plot density distribution -------------------------------------------------------------------------------------------------------
ggdistribution(norm)
ggdistribution(dnorm, seq(-3, 3, 0.1), mean = 0, sd = 1)
ggdistribution(ppois, seq(0, 30), lambda = 20)


autoplot(density(rnorm(1:50)), fill = 'green')



#### FIT PARAMETRIC DISTRIBUTIONS
x <- fdata[,1]

#Plot density histogram of data for identifying possible distributions
hist(x,freq = FALSE,breaks = 50)


#Fit distribution to data and obtain fitted shape
######### Normal
#Fit
fit.norm <- fitdistr(x, "normal") #fit distribution
fit.norm          #Show fitted parameters
fit.norm$loglik   #Show log-likelihood of estimation
#plot histogram and fitted distibution
xgrid <- seq(range(x)[1], range(x)[2], length.out = 500)
dgrid.norm <- dnorm(xgrid,mean = coef(fit.norm)[1], sd = coef(fit.norm)[2])
hist(x,freq = FALSE, breaks = 50)   #Plot density histogram
lines(x.grid,dgrid.norm,col="red")  #Plot fitted on top
#Validate fit with qqplot
x.norm <- rnorm(1e4,mean = coef(fit.norm)[1], sd = coef(fit.norm)[2]) #Generate random sample
qqplot(x.norm,x)
abline(0,1,col="red",lty = 2,lwd = 3)
###################


######### Exponential
#Fit
fit.exp <- fitdistr(x, "exponential")
fit.exp
#plot fit
d.gr.exp <- dexp(xgrid,rate = coef(fit.exp))
x.exp <- rexp(1e5,rate = coef(fit.exp))
hist(x,freq = FALSE, breaks = 100)
lines(xgrid,d.gr.exp,col="red")
#qqplot
qqplot(x.exp,x)
abline(0,1,col="red",lty = 2)
###################


######### Beta
#Fit distribution to data
fit.beta <- fitdistr(x, "beta",start = list( shape1=2, shape2=5 )) #fit distribution
fit.beta          #Show fitted parameters
fit.beta$loglik   #Show log-likelihood of estimation
#plot histogram and fitted distibution
xgrid <- seq(range(x)[1], range(x)[2], length.out = 500)
dgrid.beta <- dbeta(xgrid,shape1 = coef(fit.beta)[1], shape2 = coef(fit.beta)[2])
hist(x,freq = FALSE, breaks = 50)   #Plot density histogram
lines(xgrid,dgrid.beta,col="red")  #Plot fitted on top
#Validate fit with qqplot
x.beta <- rbeta(1e4,shape1 = coef(fit.beta)[1], shape2 = coef(fit.beta)[2])
qqplot(x.beta,x)
abline(0,1,col="red",lty = 2,lwd = 3)
                         

#Fit
fit.beta <- fitdistr(x, "beta",start = list( shape1=2, shape2=5 ))
fit.beta
#plot fit
d.gr.beta <- dbeta(xgrid,shape1 = coef(fit.beta)[1],shape2 = coef(fit.beta)[2])
x.beta <- rbeta(1e5,shape1 = coef(fit.beta)[1],shape2 = coef(fit.beta)[2])
hist(x,freq = FALSE)
lines(x.grid,d.gr.beta,col="red")
#qqplot
qqplot(x.beta,x)
abline(0,1,col="red",lty = 2)
###################


######### Gamma
#Fit
fit.gamma <- fitdistr(x, "gamma")
fit.gamma
#plot fit
d.gr.gamma <- dgamma(xgrid,shape = coef(fit.gamma)[1],rate = coef(fit.gamma)[2])
x.gamma <- rgamma(1e5,shape = coef(fit.gamma)[1], rate = coef(fit.gamma)[2])
hist(x,freq = FALSE)
lines(x.grid,d.gr.gamma,col="red")
#qqplot
qqplot(x.gamma,x)
abline(0,1,col="red",lty = 2)
###################


######### Weibull
#Fit
fit.weib <- fitdistr(x, "weibull")
fit.weib
#plot fit
d.gr.weib <- dweibull(xgrid,shape = coef(fit.weib)[1],scale = coef(fit.weib)[2])
x.weib <- rweibull(1e5,shape = coef(fit.weib)[1],scale = coef(fit.weib)[2])
hist(x,freq = FALSE)
lines(x.grid,d.gr.weib,col="red")
#qqplot
qqplot(x.weib,x)
abline(0,1,col="red",lty = 2)
###################


# non-parametric kernel estimation
ker.fit <- density(x,bw = 0.2,kernel = "gaussian")
plot(ker.fit,ylim=range(0,2.5))
ker.fit <- density(x,bw = 0.005,kernel = "gaussian")
lines(ker.fit,col="red")
ker.fit <- density(x,kernel = "gaussian")
plot(ker.fit)

################# FIT NONPARAMETRIC multivariate DISTRIBUTIONS
## Load dataset -------------------------------------------------------------------------------------------------------
fdata <- read.table("MultivariateDensityEstimation.dat",header = TRUE, sep = "")
fdata$class <- as.factor(fdata$class)
class0 <- fdata[fdata$class==0,c(1,2)]
class1 <- fdata[fdata$class==1,c(1,2)]
class2 <- fdata[fdata$class==2,c(1,2)]

ggplot(class0)+geom_density2d(aes(x=x1,y=x2))
ggplot(class1)+geom_density2d(aes(x=x1,y=x2))
ggplot(class2)+geom_density2d(aes(x=x1,y=x2))

ggplot(fdata)+geom_density2d(aes(x=x1,y=x2,colour = class))

#Barrido para 

kd.class0 <- kde2d(class0$x1, class0$x2,lims = c(range(fdata$x1),range(fdata$x2)))
kd.class1 <- kde2d(class1$x1, class1$x2,lims = c(range(fdata$x1),range(fdata$x2)))
kd.class2 <- kde2d(class2$x1, class2$x2,lims = c(range(fdata$x1),range(fdata$x2)))

ggplot(kd.class0)+geom_point(aes(x=x,y=y,color=z))

library(ks)
kd.class0 <- kde(class0[,1:2])
plot(kd.class0)
kd.class1 <- kde(class1[,1:2])
plot(kd.class1)
kd.class2 <- kde(class2[,1:2])
plot(kd.class2)


np_grid <- 50 #number of discretization points in each dimension
np.X1 <- seq(from = min(fdata$x1), to = max(fdata$x1), length.out = np_grid)
np.X2 <- seq(from = min(fdata$x2), to = max(fdata$x2), length.out = np_grid)
#Create data.frame with grid points
p.grid <- expand.grid(X1 = np.X1, X2 = np.X2) 

p.grid$d0 <- predict(kd.class0,x=p.grid[,1:2])
p.grid$d1 <- predict(kd.class1,x=p.grid[,1:2])
p.grid$d2 <- predict(kd.class2,x=p.grid[,1:2])

ggplot(p.grid)


kd.est <- predict(kd.class1,x=fdata[,1:2])
