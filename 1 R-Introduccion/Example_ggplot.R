#############################################
########### ggplot2 Example #################
#############################################

#Load library
library(ggplot2)

# This example will use the mpg dataset in ggplot2 package
data(mpg)
#Descritption of mpg
?mpg
summary(mpg)
head(mpg)

#Example plot using mpg dataset. 
#The car's fuel efficiency on the highway (hwy variable) is plotted as a function of the car's engine displacement (displ variable). 
ggplot(mpg) + geom_point(mapping = aes(x = displ, y = hwy))
#In the following code, the sintax of ggplot is detailed.

## Basic plotting -------------------------------------------------------------
#The function ggplot() creates a coordinate system that you can add layers to.
ggplot() #Evaluate and observe that it only creates an empty figure.
#You need to initialize a ggplot figure and tell what dataset to use.
ggplot(mpg)
#Add graphics layers, called geoms. The visual properties (aesthetics) of the geoms have to be specified.
#When variables in the dataset are mapped to visual properties, the aes() function is used.
#The following line adds a scatterplot graphics where each point has x coordintates given by variable displ and y coordintaes given by vairable hwy.
ggplot(mpg) + geom_point( aes(x = displ, y = hwy))
#Optionally you can add the aesthetics in the ggplot() function.
#However, the aesthetics specified here will be inherited by all the geom layers you will add subsequently.
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + geom_point()
#Additionally, the dataset and aesthetics can be defined for each geom used.
ggplot() + geom_point(data = mpg, aes(x = displ, y = hwy))


## Modify aesthetics ----------------------------------------------------------
#In the case of geom_point(), you can display a point in different ways by changing the values of its aesthetic properties.
#ggplot2 will automatically assign a unique level of the aesthetic (here a unique color) to each unique value of the variable.
#ggplot2 will also add a legend that explains which levels correspond to which values.
#The following lines are examples of definitions for color, shape and size aesthetics depending on class variable (The type of car) and cyl (Number of cylinders of the car)
ggplot(mpg) + geom_point(aes(x = displ, y = hwy, color = class))
ggplot(mpg) + geom_point(aes(x = displ, y = hwy, shape = class))
ggplot(mpg) + geom_point(aes(x = displ, y = hwy, size = cyl))
#You can also combine aesthetics
ggplot(mpg) + geom_point(aes(x = displ, y = hwy, size = cyl, color = class))
#It is possible to use logical conditions
ggplot(mpg) + geom_point(aes(x = displ, y = hwy, color = displ < 5))

#To set an aesthetic manually, set the aesthetic by name as an argument of your geom function; i.e. it goes outside of aes()
#The color does not give any information about a variable, but only changes the appearance of the plot.
ggplot(mpg) + geom_point(aes(x = displ, y = hwy), color = "red")
#Change opacity of points. This is spetially useful for big datasets.
ggplot(mpg) + geom_point(aes(x = displ, y = hwy), alpha = 0.2) 


## Labels -----------------------------------------------------------------
#In order to add the plot's main title and to change the X and Y axis titles, the labs layer is used.
#Changing the size, position and color of the labels can be done with the 'Theme' layer (see ?theme for help).
ggplot(mpg) + 
  geom_point( aes(x = displ, y = hwy)) +
  labs(title="Relation between highway mph and engine displacement", 
       x="Engine displacement [litres]", y="Highway miles per gallon")



## Facets -----------------------------------------------------------------
#When analyzing categorical variables, it is very useful to split your plot into facets.
#facet_grid() splits the figure into subplot where each displays the subset of the data correspondign to a determined value of the selected variables..

#Split horizontaly by class
ggplot(mpg) + geom_point(aes(x = displ, y = hwy, color = class))+facet_grid(.~class)
#Split vertically by driving mechanism and horizontaly by number of cylinders
ggplot(mpg) + geom_point(aes(x = displ, y = hwy, color = class))+facet_grid(drv~cyl)

## Histograms -----------------------------------------------------------------
#Being able to plot histograms of data is very useful in machine learning
#Use bins or binwidth for setting the bars
ggplot(mpg) + geom_histogram(aes(x = displ),bins = 15)
#Some options to divide the histogram according to the levels of a variable
ggplot(mpg) + geom_histogram(aes(x = displ, fill = drv),bins = 15)
ggplot(mpg) + geom_histogram(aes(x = displ, fill = drv),alpha = 0.4,bins = 15, position="identity")
ggplot(mpg) + geom_histogram(aes(x = displ, fill = drv),bins = 15) +facet_grid(.~drv)


## Miscelaneus -----------------------------------------------------------------
#It is possible to store a plot into an object, leaving it accessible for adding layers.
plot1 <- ggplot(mpg) + geom_point(aes(x = displ, y = hwy))
plot1+facet_grid(.~class)  
#You can store the new plot into another object.
plot2 <-plot1+facet_grid(drv~cyl) 
#Use print() to print a ggplot object.
print(plot2)
#Use ggsave() for storing the plot.
ggsave("myggplot.png")  # saves the last plot.
ggsave("myggplot.png", plot=plot2)  # save a stored ggplot


## Interactive plots -------------------------------------------------------------
#load library
library(plotly) 
#plot graph
ggplot(mpg) + geom_point(aes(x = displ, y = hwy))
#Make it interactive
ggplotly()
