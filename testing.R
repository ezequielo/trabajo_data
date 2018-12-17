

#Para aplicar árboles de decisión
install.packages("rattle")
library(rpart)
library(rattle)
library(RColorBrewer)



#Se guardan los datos y se entrenarán por validación cruzada
data<-read.delim("winequality-red.csv", sep = ",", head = TRUE)

#Muestro los datos
head(data)
#Muestro las columnas
colnames(data)
rownames(data)


# Set random seed.  
set.seed(1)

#Creo el árbol de decisión
tree <- rpart(quality~., data, method = "class")

#Pinto el árbol 
fancyRpartPlot(tree)

# Predice los valores del conjunto data
pred <- predict(tree, data, type = "class")

# Construye la matriz de confusion 
conf <- table(data$quality, pred) 

# Calcula accuracy 
acc <- sum(diag(conf)) / sum(conf) 
print(acc)
