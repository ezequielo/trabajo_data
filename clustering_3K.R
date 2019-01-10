##Libreria Rattle
library("rattle")
library("NbClust") #k means optimo
##Probando otras de visualizacion y encontrar k means/ clustering interesantes 

install.packages("cluster")
install.packages("factoextra")
library(factoextra)
library(cluster)
library(ggplot2)


wine <- read.delim("winequality-red.csv", sep = ",", head = TRUE)
data(wine, package='rattle')
head(wine)




##Escalado/estandarizado de los datos 

##opcional quitarla

wine.stand <- scale(wine[-1])  # To standarize the variables
head(wine.stand)
k.means.wine <- kmeans(wine, 3) # k = 3
# K-Means con 3 clusters, tenemos tres etiquetas de salida
k.means.fit <- kmeans(wine.stand, 3) # k = 3
## observamos los atributos tras aplicar el keans a nuestro conjunto de datos escalado
attributes(k.means.fit)
##visualizamos y guardamos
jpeg('kmeans3.jpeg')
plot(wine.stand, col=k.means.fit$cluster, main="Tres clusters")

dev.off()
##TODO


# Visualize

fviz_cluster(k.means.wine, data = wine, ellipse.type =  "convex")+
  theme_minimal()

fviz_cluster(k.means.fit, data = wine.stand, ellipse.type =  "convex",stand = FALSE, frame.type = "norm")+
  theme_minimal()



###METODOS DIFERENTES PARA LA K
##1)Elbow method for k-means clustering 
wss <- 0
for (i in 3:15) {
  km.out <- kmeans(wine.stand, centers = i, nstar=20) 
  wss[i] <- km.out$tot.withinss
}
jpeg('clusters_linea.jpg')

plot(3:15, wss, type = "b",  col="mediumseagreen", xlab = "Number of Clusters",
     ylab = "Within groups sum of squares")
abline(v = 3, lty =2)
dev.off()


fviz_nbclust(wine.stand, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)



## a simple vista podemos observar que el mejor valor para k seria 3 dado que se suele escoger el valor o 
## valores en los que se crea un 'codo' en la gráfica (justificar mejor)
##sin embargo, hay una libreria utilizada para ayudarnos a discenir mejor este caso NbClust
##2.libreria NbClust
# MINIMO DE Ks=3 tenemos 3 etiquetas de salida de clasificacion 

set.seed(123)
nc <- NbClust(wine.stand, min.nc=3, max.nc=15, method="kmeans")

##TODO
fviz_nbclust(wine.stand, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)
jpeg('clusters_optimos_NbClust.jpeg')
plot(nc)
dev.off()

##Salida 
# *** : The Hubert index is a graphical method of determining the number of clusters.
# In the plot of Hubert index, we seek a significant knee that corresponds to a 
# significant increase of the value of the measure i.e the significant peak in Hubert
# index second differences plot. 
# 
# *** : The D index is a graphical method of determining the number of clusters. 
# In the plot of D index, we seek a significant knee (the significant peak in Dindex
#                                                     second differences plot) that corresponds to a significant increase of the value of
# the measure. 
# ******************************************************************* 
#   * Among all indices:                                                
#   * 6 proposed 3 as the best number of clusters 
# * 4 proposed 4 as the best number of clusters 
# * 1 proposed 5 as the best number of clusters 
# * 6 proposed 6 as the best number of clusters 
# * 2 proposed 8 as the best number of clusters 
# * 1 proposed 9 as the best number of clusters 
# * 1 proposed 10 as the best number of clusters 
# * 2 proposed 15 as the best number of clusters 
# 
# ***** Conclusion *****                            
#   
#   * According to the majority rule, the best number of clusters is  3 
# 
# 
# ******************************************************************* 
##

#Conclusion k=3


res.mca <- MCA(wine.stand, graph=FALSE)
fviz_mca_var(res.mca)
fviz_mca_var(res.mca, alpha.var = "contrib") +
  theme_minimal()


