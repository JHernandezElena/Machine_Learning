
#################################################################################
##############          LabPractice 5.5  SOM         ############################
##############     ----------- solution ---------    ############################
#################################################################################

# Load the kohonen package 
require(kohonen)

## custom palette (from http://www.shanelynn.ie/self-organising-maps-for-customer-segmentation-using-r/)
source('coolBlueHotRed.R')
# Colour palette definition
pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2')




###############################################################################
#----------------- Countries ----------------------------------------------------
###############################################################################

## Load dataset -------------------------------------------------------------------------------------------------------
fdata <- read.table("Countries.dat",header = TRUE, sep = "", stringsAsFactors = FALSE)
# Change the data frame to a matrix. Also center and scale
fdata_matrix <- as.matrix(scale(fdata[,-1]))
##############################################################################

# Create the SOM Grid - size and topology (Hexagonal and Circular) 
som_grid <- somgrid(xdim = 6, ydim = 5, topo="hexagonal", toroidal = FALSE)

# Train the SOM
som_model <- som(fdata_matrix, 
                 grid=som_grid, 
                 rlen=2000, #iterations
                 alpha=c(0.05,0.01), #learning rates
                 keep.data = TRUE ) #store data in model
#Training error
plot(som_model, type="changes")
#Population of nodes
plot(som_model, type="count", shape = "straight", main="Node Counts",palette.name=coolBlueHotRed)
  #Numero de elementos que han caido en cada nodo

#Distance matrix U-Matrix
plot(som_model, type="dist.neighbours", shape = "straight", main = "SOM neighbour distances",palette.name=coolBlueHotRed)
    #distancia a media de los vecionos
      #los azules son muy pareciods, hay menos homogeneidad a la izq que a la derecha

#plot centers
plot(som_model, type="codes", shape = "straight", palette.name=coolBlueHotRed) #use argument codeRendering="lines" or codeRendering="segments" to fix the type of representation
# Plot the heatmap for a variable at scaled / normalised values
ivar <- 2
plot(som_model, type = "property", property = getCodes(som_model)[,ivar],
     palette.name=coolBlueHotRed, main = colnames(getCodes(som_model))[ivar])
#show labels of the points inside each cell
plot(som_model, type="mapping",
     labels = fdata$Country, cex =1, #cex for changing label size
     main = "mapping plot",shape = "straight")

#plot a variable from the original data set 
#(If there are nodes with missing data might produce errors)
# This function produces a menu for multiple heatmaps if a factor or character is chosen
source('plotHeatMap.R')
# A menu of all variables should be displayed if variable=0 
# (note on Mac this will required working XQuartz installation.)
plotHeatMap(som_model, fdata, variable=0)



## use hierarchical clustering to cluster the codebook vectors
hmodel <- hclust(dist(som_model$codes[[1]]))
plot(hmodel,hang=-1)
som_cluster <- cutree(hmodel,3)
# plot these results:
plot(som_model, type="mapping", 
     bgcol = pretty_palette[som_cluster],
     main = "Clusters",
     labels = "",
     shape = "straight") 
add.cluster.boundaries(som_model, som_cluster)
#Plot centers and cluster boundaries
plot(som_model, type="codes",palette.name=coolBlueHotRed, shape = "straight")
add.cluster.boundaries(som_model, som_cluster)



###############################################################################
#----------------- Demand ----------------------------------------------------
###############################################################################

## Load dataset -------------------------------------------------------------------------------------------------------
fdata <- read.table("Dem_2011_2015_24h.dat",header = TRUE, sep = "", stringsAsFactors = FALSE)
fdata$DAY <- as.factor(fdata$DAY)
# Change the data frame to a matrix. Also center and scale
fdata_matrix <- as.matrix(scale(fdata[,3:26]))


# Create the SOM Grid - size and topology (Hexagonal and Circular) 
som_grid <- somgrid(xdim = 5, ydim = 5, topo="hexagonal", toroidal = FALSE)

# Train the SOM
som_model <- som(fdata_matrix, 
                 grid=som_grid, 
                 rlen=2000, #iterations
                 alpha=c(0.05,0.01), #learning rates
                 keep.data = TRUE ) #store data in model
#Training error
plot(som_model, type="changes")
#Population of nodes
plot(som_model, type="count", shape = "straight", main="Node Counts",palette.name=coolBlueHotRed)
#Distance matrix U-Matrix
plot(som_model, type="dist.neighbours", shape = "straight", main = "SOM neighbour distances",palette.name=coolBlueHotRed)
#plot centers
plot(som_model, type="codes", shape = "straight", palette.name=coolBlueHotRed) #use argument codeRendering="lines" or codeRendering="segments" to fix the type of representation
# Plot the heatmap for a variable at scaled / normalised values
ivar <- 12
plot(som_model, type = "property", property = getCodes(som_model)[,ivar],
     palette.name=coolBlueHotRed, main = colnames(getCodes(som_model))[ivar])


#plot a variable from the original data set 
#(If there are nodes with missing data might produce errors)
# This function produces a menu for multiple heatmaps if a factor or character is chosen
source('plotHeatMap.R')
# A menu of all variables should be displayed if variable=0 
# (note on Mac this will required working XQuartz installation.)
plotHeatMap(som_model, fdata, variable=0)



## use clustering to cluster the codebook vectors
library(factoextra)
fviz_nbclust(som_model$codes[[1]], kmeans, method = "wss")+
  labs(subtitle = "Elbow method")
km <- kmeans(som_model$codes[[1]], 4)
# plot these results:
plot(som_model, type = "property", 
     property = km$cluster,
     main = "Clusters",
     labels = "",
     shape = "straight") 
add.cluster.boundaries(som_model, km$cluster)
#Plot centers and cluster boundaries
plot(som_model, type="codes",palette.name=coolBlueHotRed, shape = "straight")
add.cluster.boundaries(som_model, km$cluster)

