library(ggplot2)
#Generate sinthetic data
Temp <- 15+rnorm(500)*2
y <- Temp^2-30*Temp+250+rnorm(500)
dt <- data.frame(y=y,Temp=Temp) #este es el conjunto de datos
ggplot(dt)+geom_point(aes(x=Temp, y=y))


#Fit regression model
lm.fit <- lm(y~Temp)
dt <- cbind(dt,fit0=lm.fit$fitted.values)
ggplot(dt)+geom_point(aes(x=Temp, y=y))+geom_point(aes(x=Temp, y=fit0),color="blue")
  #parece que a partir de 16 sube y que baja hasta 14


#Divide temperature (CREAR VISAGRA PAR LM)
T.cold <- sapply(Temp,min,14)
T.hot <- sapply(Temp,max,16)
dt <- cbind(dt,T.cold,T.hot) #anadimos al conjunto de datos las columnas tcold y thot

#Fit regression model CON TCOLD,THOT Y DT
lm.fit <- lm(y~T.cold+T.hot,dt)
dt <- cbind(dt,fit=lm.fit$fitted.values)
ggplot(dt)+geom_point(aes(x=Temp, y=y))+geom_point(aes(x=Temp, y=fit),color="blue")


#Add complexity
T.verycold <- sapply(Temp,min,12)
dt <- cbind(dt,T.verycold) #anadimos tverycold al conjunto de datos
lm.fit <- lm(y~T.cold+T.hot+T.verycold,dt)
dt <- cbind(dt,fit2=lm.fit$fitted.values)
ggplot(dt)+geom_point(aes(x=Temp, y=y))+geom_point(aes(x=Temp, y=fit2),color="blue")

