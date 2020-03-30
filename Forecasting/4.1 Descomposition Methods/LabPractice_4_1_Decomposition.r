#################################################################################
##############   Forecasting: Decomposition Methods   ###########################
##############   ------------- solution -----------   ###########################
#################################################################################

library(fpp2)
library(ggplot2)

## Set working directory ---------------------------------------------------------------------------------------------


## Load dataset -------------------------------------------------------------------------------------------------------
fdata <- read.table("Unemployment.dat",header = TRUE, stringsAsFactors = FALSE)

#Convert to time series object
fdata_ts <- ts(fdata$TOTAL,start = 2009, frequency = 12)
      #Le pasamos un indice para el comienzo  
      #es una serie mensual (frecuencia 12) se repite cada 12 meses
      


#################################################################################
# Functions for plotting a time series
#################################################################################

#Plot time series
autoplot(fdata_ts) +
  ggtitle("Unemployment in Spain") +
  xlab("Year") + ylab("Number unemployed")
    #vemos que en el utlimo mes ha sudido
    #esta subiendo por lo estacional? eso es lo que queremos saber

#If fdata_ts contains several time series, the plot can be faceted 
#autoplot(fdata_ts, facet = TRUE)


#################################################################################
# Functions for extracting a portion of a time series
#################################################################################

fdata_ts_cut <- window(fdata_ts, start=2013)
fdata_ts_cut <- window(fdata_ts, end=c(2013,12))
fdata_ts_cut <- window(fdata_ts, start = c(2012,5), end=c(2016,3))

#subset allows for more types of subsetting. It allows the use of indices to choose a subset.
fdata_ts_cut <- subset(fdata_ts, start=length(fdata_ts)-5)
#Head and tail are useful for extracting the first few or last few observations.
fdata_ts_cut <- tail(fdata_ts, 5)




#################################################################################
# Decomposition methods
#################################################################################
#****Use seasonal(), trendcycle() and remainder() functions to extract the individual components.
#****Use seasadj()  QUITA LA COMPONENTE ESTACIONAL!! to compute the seasonally adjusted time series. 


## Classical additive decomposition -----------------------------------------------------------
fdata_ts_dec_add <- decompose(fdata_ts,type="additive")
autoplot(fdata_ts_dec_add) + xlab("Year") +
  ggtitle("Classical additive decomposition")
        #el recuadrito es la escala
        #hay mucho ruido que no se puede explicar
        #se obliga a que la componente estacional sea la misma para todo los anos

#EXTRACT THE SEASONAL COMPONENT
seasonal = seasonal(fdata_ts_dec_add)      
autoplot(seasonal) + xlab("Year")
#ADJUST THE TS WITHOUT STACIONAL COMPONENT
adjusted_ts = seasadj(fdata_ts_dec_add)
plot(adjusted_ts)


## Classical Multiplicative decomposition --------------------------------------------------
fdata_ts_dec_mult <- decompose(fdata_ts,type="multiplicative")
autoplot(fdata_ts_dec_mult) + xlab("Year") +
  ggtitle("Classical multiplicative decomposition")
        #se obliga a que la componente estacional sea la misma para todo los anos


## SEATS -------------------------------------------------------------------------------------
#PERMITE QUE EL COMPONENTE ESTACIONAL VARIE CADA ANO
library(seasonal)
fdata_ts_dec_seas <- seas(fdata_ts)
autoplot(fdata_ts_dec_seas)+ xlab("Year") +
  ggtitle("SEATS decomposition")
  #las diferencias de paro entre verano e inverno cambian
  #tambien cambia la tendencia en diciembre
  




#Compare seasonal components ---------------------------------------------------------------------------------
#SOLO FUNCIONA DE DOS EN DOS
autoplot(seasonal(fdata_ts_dec_add), series = "Additive")+
  forecast::autolayer(seasonal(fdata_ts_dec_mult), series = "Multiplicative") +
  forecast::autolayer(seasonal(fdata_ts_dec_seas),series = "SEATS")

autoplot(seasonal(fdata_ts_dec_mult), series = "Multiplicative") +
  forecast::autolayer(seasonal(fdata_ts_dec_seas),series = "SEATS")
  


#TS SIN LA COMPONENTE ESTACIONAL
#Compare seasonal adjustment components (i.e. substracting the seasonal component from the raw series)---------------------
autoplot(seasadj(fdata_ts_dec_add), series = "Additive")+
  forecast::autolayer(seasadj(fdata_ts_dec_mult), series = "Multiplicative") +
  forecast::autolayer(seasadj(fdata_ts_dec_seas),series = "SEATS")



#Seasonal subseries plot
ggsubseriesplot(seasonal(fdata_ts_dec_add)) 
ggsubseriesplot(seasonal(fdata_ts_dec_mult)) 
ggsubseriesplot(seasonal(fdata_ts_dec_seas)) 

