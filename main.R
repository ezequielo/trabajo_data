library(caret)
library(mlbench)
library(doParallel)
library(corrplot)
library(ROCR)
library(pROC)


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


# discretizamos la columna quality en tres posibles valores: bad y good
# para poder convertirlo en un problema de clasificacion
wine2 <- wine
wine2$quality <- ifelse(wine$quality < 6, 'bad', 'good')

print(head(wine2, 10))
# podemos ver que el tipo de wine2 ahora es una etiqueta
str(wine2)
barplot(table(wine$quality))
barplot(table(wine2$quality))



#---------------------
#   Preprocesado
#---------------------

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
control <- trainControl(method = "cv", number = 10, allowParallel = TRUE, classProbs = TRUE, summaryFunction = twoClassSummary)

# metric
# metric = "ROC"

# naive bayes
nb_model <- train(quality ~ ., training, method="naive_bayes", trControl=control)
print(nb_model)

# neural network
nn_model <- train(quality ~ ., training, method = 'nnet', trControl = control)

# decision tree
dt_model <- train(quality ~ ., training, method = "C5.0", trControl = control)

# nearest neighbour
near_model <- train(quality ~ ., training, method = "knn", trControl = control)

# svm
svm_model <- train(quality ~ ., training, method = "svmLinear2",  trControl = control, probability = TRUE)


# comparamos los modelos obtenidos
models <- list(
  nb = nb_model,
  nn = nn_model,
  dt = dt_model,
  near = near_model,
  svm = svm_model
)
resamples <- resamples(models)
print(resamples)
summary(resamples) 
bwplot(resamples, metric = "ROC")  
dotplot(resamples, metric = "ROC")

# podemos ver como de buenos son cada uno de los modelos entrenados con los diagramas que muestran
# el valor del area bajo la curva ROC


#---------------------
# blind tests
#---------------------

# hacemos prediciones con el conjunto de test para cada modelo
# calculamos una prediccion y medimos el rendimiento:

nbProb <- predict(nb_model, test, type = 'prob')
nbPrediction <- prediction(nbProb[,2], test$quality)
nbPerformance <- performance(nbPrediction, 'tpr', 'fpr')

nnProb <- predict(nn_model, test, type = 'prob')
nnPrediction <- prediction(nnProb[,2], test$quality)
nnPerformance <- performance(nnPrediction, 'tpr', 'fpr')

dtProb <- predict(dt_model, test, type = 'prob')
dtPrediction <- prediction(dtProb[,2], test$quality)
dtPerformance <- performance(dtPrediction, 'tpr', 'fpr')

nearProb <- predict(near_model, test, type = 'prob')
nearPrediction <- prediction(nearProb[,2], test$quality)
nearPerformance <- performance(nearPrediction, 'tpr', 'fpr')

svmProb <- predict(svm_model, test, type = 'prob')
svmPrediction <- prediction(svmProb[,2], test$quality)
svmPerformance <- performance(svmPrediction, 'tpr', 'fpr')


# pintamos las curvas roc
plot(nbPerformance, col = "orange", main = "ROC curves")
plot(dtPerformance, add = TRUE, col = "blue")
plot(nnPerformance, add = TRUE, col = "red")
plot(nearPerformance, add = TRUE, col = "green")
plot(svmPerformance, add = TRUE, col = "pink")
legend("bottomright", title= "Predictors",
       legend = c("NB", "DT", "NN", "kNN", "SVM"), 
       col=c("orange", "blue", "red", "green", "pink"), lty=1, cex=0.8)

# como podemos ver, el rendimiento de los modelos ha sido practicamente igual 
# al de los test, el DT sigue siendo el mejor, a continuacion tenemos nb, nn y svm 
# con valores parecidos y por ultimo el knn con el peor valor


