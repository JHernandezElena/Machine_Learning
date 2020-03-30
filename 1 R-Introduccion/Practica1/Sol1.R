library(tidyverse)

#problema de clasificacion
sim5 = read.table(file = "SimData5.csv",sep = ";", header = TRUE) 
View(sim5)

#dibujo
ggplot(sim5) + geom_point(aes(x=X1, y = X2, color = Y))
#tablero de ajedrez
