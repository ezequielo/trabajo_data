library(caret)
library(mlbench)
library(doParallel)
library(corrplot)

#---------------------
#   Carga de datos
#---------------------
wine <- read.delim("winequality-red.csv", sep = ",", head = TRUE)

# comprobamos el dataset
dim(wine) 
colnames(wine)
rownames(wine)
wine[2:10, 1:12]
str(wine2)



#---------------------
#   Visualizacion
#---------------------
# TODO visualización de los atributos, valores (medias, modas,...), ditribuciones, diagramas, etc
# ...

# TODO clustering como parte de la visualización
# puede ser que el número de etiquetas (bad, average y good) se vea mas claro en este apartado


# discretizamos la columna quality en tres posibles valores: bad, average y good
# para poder convertirlo en un problema de clasificacion
wine2 <- wine
wine2$quality <- ifelse(wine$quality < 5, 'bad', ifelse(wine$quality < 7, 'average', 'good'))
print(head(wine2, 10))
# podemos ver que el tipo de wine2 ahora es una etiqueta
str(wine2)



#---------------------
#   Preprocesado
#---------------------

#integridad

na_count <-sapply(wine, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count

# correlacion
# hacemos que los resultados sean reproducibles
set.seed(123)


# matriz de correlacion
correlationMatrix <- cor(wine2[,1:11])
print(correlationMatrix)

# pintamos el diagrama de correlacion para verlo graficamente
corrplot(correlationMatrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# cogemos aquellas columnas cuya correlacion sea mayor a 0.75 (muy alta)
# y los pintamos
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
print(highlyCorrelated)
# no hay ningun atributo que tenga una correlacion de mas del 0.75, con lo cual 
# no se elimina ninguna columna, nos quedamos con los 11 atributos


# dividimos en test y training para hacer un 'blind test' una vez entrenemos los modelos
# 70% de training y 30% de test
trainIndex <- createDataPartition(wine2$quality, p = .7, list = FALSE, times = 1)
training <- wine2[ trainIndex,]
test  <- wine2[-trainIndex,]



#---------------------
#   Entrenamiento
#---------------------
# para entrenar, usamos el conjunto de training creado anteriormente
# preparar entorno de ejecucion para prediccion.
registerDoParallel(cores=8)

# k-fold cross-validation con k=10
control <- trainControl(method = "cv", number = 10, allowParallel = TRUE)

# entrenamos modelos
# naive bayes
nb_model <- train(quality ~ ., training, method="naive_bayes", trControl=control)

# neural network
nn_model <- train(quality ~ ., training, method = 'nnet', trControl = control)

# decision tree
dt_model <- train(quality ~ ., training, method = "C5.0", trControl = control)

# nearest neighbour
near_model <- train(quality ~ ., training, method = "knn", trControl = control)

# svm
svm_model <- train(quality ~ ., training, method = "svmLinear2",  trControl = control, probability = TRUE)

# TODO: model tunning, cambiar parametros, etc (esto es mas avanzado asi que se hara lo ultimo si queda tiempo)
# TODO: comparar modelos (curvas ROC, AUC)
# TODO: hacer los blind tests para ver como de buenos son realmente los modelos creados anteriormente

