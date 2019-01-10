library(caret)
library(mlbench)
library(doParallel)
library(corrplot)
library(gridExtra)
library(ggplot2)
library(ROCR)
library(pROC)
library(caretEnsemble)

#---------------------
#   Carga de datos
#---------------------
wine <- read.delim("winequality-red.csv", sep = ",", head = TRUE)

# comprobamos el dataset
dim(wine) 
colnames(wine)
rownames(wine)
wine[2:10, 1:12]
str(wine)


# discretizamos la columna quality en tres posibles valores: bad, average y good
# para poder convertirlo en un problema de clasificacion
wine2 <- wine
wine2$quality <- ifelse(wine$quality < 6, 'bad', 'good')
wine2$quality <- ordered(wine2$quality, levels = c('bad', 'good')) # ordenados para visualizaciones
print(head(wine2, 10))
# podemos ver que el tipo de wine2 ahora es una etiqueta
str(wine2)

barplot(table(wine$quality))
barplot(table(wine2$quality))

#---------------------
#   Visualizacion
#---------------------
# TODO visualización de los atributos, valores (medias, modas,...), ditribuciones, diagramas, etc

# Distribución de la calidad en los vinos del dataset
ggplot(data = wine, aes(x = quality)) + geom_bar(width = 1, color = 'black',fill = I('red'))
grid.arrange(ggplot(data = wine2, aes(x = quality)) + geom_bar(width = 1, color = 'black',fill = I('red')), top="Distribución de ejemplos por clase")

#Distribución de la acidez fija
grid.arrange(ggplot(wine, aes( x = 1, y = fixed.acidity ) ) + geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ),
             ggplot(data = wine, aes(x = fixed.acidity)) + geom_histogram(binwidth = 0.5, color = 'black',fill = I('red')),
             ncol = 2, top="Distribución de los valores de acidez fija")

#Distribución de la acidez volatil
grid.arrange(ggplot(wine, aes( x = 1, y = volatile.acidity ) ) + geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ),
             ggplot(data = wine, aes(x = volatile.acidity)) + geom_histogram(binwidth = 0.1, color = 'black',fill = I('red')),
             ncol = 2, top="Distribución de los valores de acidez volátil")

#Distribución del ácido cítrico
grid.arrange(ggplot(wine, aes( x = 1, y = citric.acid ) ) + geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ),
             ggplot(data = wine, aes(x = citric.acid)) + geom_histogram(binwidth = 0.05, color = 'black',fill = I('red')),
             ncol = 2, top="Distribución de los valores del ácido cítrico")

#Distribución del azucar residual
imagen = grid.arrange(ggplot(wine, aes( x = 1, y = residual.sugar ) ) + geom_jitter(alpha = 0.1 ) +
             geom_boxplot(alpha = 0.2, color = 'red' ) + scale_y_continuous(lim = c(1,8)),
             ggplot(data = wine, aes(x = residual.sugar)) + geom_histogram(binwidth = 0.1, color = 'black',fill = I('red')) +
             scale_x_continuous(lim = c(1,8)),
             ncol = 2, top="Distribución de los valores del azúcar residual")

#Distribución de los cloruros
grid.arrange(ggplot(wine, aes( x = 1, y = chlorides ) ) + geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ) + scale_y_continuous(lim = c(0,0.25)),
             ggplot(data = wine, aes(x = chlorides)) + geom_histogram(binwidth = 0.005, color = 'black',fill = I('red')) +
               scale_x_continuous(lim = c(0,0.25)),
             ncol = 2, top="Distribución de los valores de cloruros")

#Distribución del dióxido de azufre libre
grid.arrange(ggplot(wine, aes( x = 1, y = free.sulfur.dioxide )) + geom_jitter(alpha = 0.1 ) +
             geom_boxplot(alpha = 0.2, color = 'red' ) +  scale_y_continuous(lim = c(0,45)),
             ggplot(data = wine, aes(x = free.sulfur.dioxide)) + 
             geom_histogram(binwidth = 1, color = 'black',fill = I('red')) +
             scale_x_continuous(lim = c(0,45)), ncol = 2,
             top="Distribución de los valores de dióxido de azufre libre")

#Distribución del dióxido de azufre total
grid.arrange(ggplot(wine, aes( x = 1, y = total.sulfur.dioxide )) + 
             geom_jitter(alpha = 0.1 ) + geom_boxplot(alpha = 0.2, color = 'red' ) +
             scale_y_continuous(lim = c(0,170)),
             ggplot(data = wine, aes(x = total.sulfur.dioxide)) +
             geom_histogram(binwidth = 5, color = 'black',fill = I('red')) +
             scale_x_continuous(lim = c(0,170)), ncol = 2,
             top="Distribución de los valores de dióxido de azufre total")

#Distribución de la densidad
grid.arrange(ggplot(wine, aes( x = 1, y = density)) + geom_jitter(alpha = 0.1 ) +
             geom_boxplot(alpha = 0.2, color = 'red' ),
             ggplot(data = wine, aes(x = density)) +
             geom_histogram(binwidth = 0.001, color = 'black',fill = I('red')), ncol = 2,
             top="Distribución de los valores de la densidad")

#Distribución del pH
grid.arrange(ggplot(wine, aes( x = 1, y = pH)) + geom_jitter(alpha = 0.1 ) +
            geom_boxplot(alpha = 0.2, color = 'red' ),
            ggplot(data = wine, aes(x = pH)) +
            geom_histogram(binwidth = 0.1, color = 'black',fill = I('red')), ncol = 2,
            top="Distribución de los valores de pH")

#Distribución de sulfatos
grid.arrange(ggplot(wine, aes( x = 1, y = sulphates)) + geom_jitter(alpha = 0.1 ) +
             geom_boxplot(alpha = 0.2, color = 'red' ) + scale_y_continuous(lim = c(0.3,1.6)),
             ggplot(data = wine, aes(x = sulphates)) + geom_histogram(binwidth = 0.1, color = 'black',fill = I('red')) +
             scale_x_continuous(lim = c(0.3,1.6)), ncol = 2, top="Distribución de los valores de sulfatos")

#Distribución de alcohol
grid.arrange(ggplot(wine, aes( x = 1, y = alcohol)) + geom_jitter(alpha = 0.1 ) + 
             geom_boxplot(alpha = 0.2, color = 'red' ) + scale_y_continuous(lim = c(8,14)),
             ggplot(data = wine, aes(x = alcohol)) +
             geom_histogram(binwidth = 0.1, color = 'black',fill = I('red')) +
             scale_x_continuous(lim = c(8,14)), ncol = 2, top="Distribución de los valores de alcohol")


#Alcohol vs Calidad
ggplot(wine, aes( x = as.factor(quality), y = alcohol)) + geom_jitter(alpha = 0.2 ) + 
       geom_boxplot(alpha = 0.8, color = 'red' ) + scale_y_continuous(lim = c(8,14)) +
       stat_summary(fun.y = "mean", geom = "point", color = "blue", shape = 8, size = 4)
ggplot(wine2, aes( x = as.factor(quality), y = alcohol)) + geom_jitter(alpha = 0.1 ) + 
       geom_boxplot(alpha = 0.8, color = 'red' ) + scale_y_continuous(lim = c(8,14)) +
       stat_summary(fun.y = "mean", geom = "point", color = "blue", shape = 8, size = 4)

#Azúcar residual vs Calidad
ggplot(data=wine, aes(x=as.factor(quality), y=residual.sugar)) + geom_jitter( alpha = 0.2) +
       geom_boxplot(alpha = 0.8,color = 'red') + scale_y_continuous(lim = c(0,5)) +
       stat_summary(fun.y = "mean", geom = "point", color = "blue", shape = 8, size = 4)
ggplot(data=wine2, aes(x=as.factor(quality), y=residual.sugar)) + geom_jitter( alpha = 0.2) +
       geom_boxplot(alpha = 0.8,color = 'red') + scale_y_continuous(lim = c(0,5)) +
       stat_summary(fun.y = "mean", geom = "point", color = "blue", shape = 8, size = 4)

#Enfrentando las dos características con más correlación con la calidad
ggplot(wine2, aes( x = volatile.acidity, y = alcohol, color=quality)) + geom_jitter(alpha = 1 ) +
        scale_color_manual(values=c("red", "green", "blue"))
     
# TODO clustering como parte de la visualización
# puede ser que el número de etiquetas (bad, average y good) se vea mas claro en este apartado


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
control <- trainControl(method = "cv", number = 10, allowParallel = TRUE, classProbs = TRUE, summaryFunction = twoClassSummary)

# metric
# metric = "ROC"

# naive bayes
nb_model <- train(quality ~ ., training, method="naive_bayes", trControl=control)
print(nb_model)

# neural network
nn_model <- train(quality ~ ., training, method = 'nnet', trControl = control)
print(nn_model)

# decision tree
dt_model <- train(quality ~ ., training, method = "C5.0", trControl = control)
print(dt_model)

# nearest neighbour
near_model <- train(quality ~ ., training, method = "knn", trControl = control)
print(near_model)

# svm
svm_model <- train(quality ~ ., training, method = "svmLinear2",  trControl = control, probability = TRUE)
print(svm_model)

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
# stacking
#---------------------

# models - cogemos el cnnet y C5.0 porque tienen baja correlacion (mirar correlacion entre modelos)
algorithmList <- c('nnet', 'C5.0')
models <- caretList(quality~., data=training, trControl=control, methodList=algorithmList)

# correlation entre modelos
results = resamples(models)
modelCor(results)
splom(results)

# stacking
stackControl <- trainControl(method="cv", number=10, allowParallel = TRUE, savePredictions=TRUE, classProbs=TRUE, summaryFunction = twoClassSummary)
stack.glm <- caretStack(models, method="glm", metric="ROC", trControl=stackControl)
print(stack.glm)


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


stackProb <- predict(stack.glm, test, type = 'prob')
stackPrediction <- prediction(stackProb, test$quality)
stackPerformance <- performance(stackPrediction, 'tpr', 'fpr')



# pintamos las curvas roc
plot(nbPerformance, col = "orange", main = "ROC curves")
plot(dtPerformance, add = TRUE, col = "blue")
plot(nnPerformance, add = TRUE, col = "red")
plot(nearPerformance, add = TRUE, col = "green")
plot(svmPerformance, add = TRUE, col = "pink")
plot(stackPerformance, add = TRUE, col = "black")
stackPerformance
legend("bottomright", title= "Predictors",
       legend = c("NB", "DT", "NN", "kNN", "SVM", "Stack"), 
       col=c("orange", "blue", "red", "green", "pink", "black"), lty=1, cex=0.8)

# como podemos ver, el rendimiento de los modelos ha sido practicamente igual 
# al de los test, el DT sigue siendo el mejor, a continuacion tenemos nb, nn y svm 
# con valores parecidos y por ultimo el knn con el peor valor




