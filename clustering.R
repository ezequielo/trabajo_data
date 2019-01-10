##Libreria Rattle
library("NbClust") #k means optimo
##Probando otras de visualizacion y encontrar k means/ clustering interesantes 
#install.packages("cluster")
library(ggplot2) #First, factoextra necesita ggplot2
library(factoextra)
library(cluster)


###########################################################################
#                                                                         #
#                             CARGA DE DATOS                              #
#                                                                         #
###########################################################################

wine_cl <- read.delim("winequality-red.csv", sep = ",", head = TRUE)
head(wine_cl)


##Eliminamos variable de clasificacion
wine_cl$quality <- NULL
head(wine_cl)


##Escalado/estandarizado de los datos 
wine.stand <- scale(wine_cl)  # To standarize the variables
head(wine.stand)





###########################################################################
#                                                                         #
#                    TÉCNICAS K MÁS ÓPTIMA                                #
#                                                                         #
###########################################################################



####################NbClust kmeans


nc <- NbClust(wine.stand, min.nc=2, max.nc=15, method="kmeans")


####################NbClust ELBOW

#VISUALIZACION ELBOW


jpeg('kmeans_elbow.jpeg')
fviz_nbclust(wine.stand, kmeans,method = c("wss"))+geom_vline(xintercept = 6, linetype = 2)
dev.off()



###################SILHOUETTE

nc_silhouette<- NbClust(wine.stand, min.nc=2, max.nc=15, method="kmeans", index="silhouette")
nc_silhouette


#VISUALIZACION SILHOUETTE

jpeg('kmeans_silhouette.jpeg')
fviz_nbclust(wine.stand, kmeans,method = c("silhouette"))
dev.off()



###########################################################################
#                                                                         #
#        VISUALIZACION DE LOS CLUSTERING SEGUN EL K DE CADA METODO        #
#                                                                         #
###########################################################################

###############################################
# EL RESULTADO TRAS APLICAR SILHOUETE ES K=6  #
###############################################

#Aplicamos Kmeans 6 CLUSTERS

##############################
#       SIN ESCALADO         #
##############################

k.means.wine.6 <- kmeans(wine_cl, 6) 


##############################
#         ESCALADO           #
##############################


k.means.fit.6 <- kmeans(wine.stand, 6) # k = 2


#VISUALIZACIÓN

##############################
#       SIN ESCALADO         #
##############################

jpeg('fviz_cluster_kmeans6_sinescalar.jpeg')
fviz_cluster(k.means.wine.6, data = wine_cl,ellipse.type =  "convex",main="Cluster plot k=6 sin escalar")+
  theme_minimal()
dev.off()

##############################
#         ESCALADO           #
##############################

jpeg('fviz_cluster_kmeans6_escalado.jpeg')
fviz_cluster(k.means.fit.6, data = wine.stand, ellipse.type =  "convex",main="Cluster plot k=6 escalado")+
  theme_minimal()
dev.off()



###########################################
# EL RESULTADO TRAS APLICAR ELBOW ES K=2  #
###########################################

#Aplicamos Kmeans 2 CLUSTERS

##############################
#       SIN ESCALADO         #
##############################

k.means.wine.2 <- kmeans(wine_cl, 2)

##############################
#         ESCALADO           #
##############################


k.means.fit.2 <- kmeans(wine.stand, 2)


#VISUALIZACIÓN

##############################
#       SIN ESCALADO         #
##############################

jpeg('fviz_cluster_kmeans2_sinescalar.jpeg')
fviz_cluster(k.means.wine.2, data = wine_cl,ellipse.type =  "convex",main="Cluster plot k=6 sin escalar")+
  theme_minimal()
dev.off()

##############################
#         ESCALADO           #
##############################

jpeg('fviz_cluster_kmeans2_escalado.jpeg')
fviz_cluster(k.means.fit.2, data = wine.stand, ellipse.type =  "convex",main="Cluster plot k=2 escalado")+
  theme_minimal()
dev.off()







###########################################################################
#                                                                         #
#                               staking                                   #
#                                                                         #
###########################################################################
#https://www.kaggle.com/c/allstate-claims-severity/discussion/25743

