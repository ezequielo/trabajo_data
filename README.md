# Análisis de la Calidad del Vino Rojo - FID - Grupo 6
Proyecto desarrollado para la asignatura Fundamentos de Ingeniería de Datos impartida en el Máster de Ingeniería del Software (Universidad de Sevilla).

## Conjunto de Datos
El conjunto de datos elegido para su posterior análisis se encuentra en este [archivo](winequality-red.csv).

## Visualización
Las gráficas de visualización extraida se encuentran en esta [carpeta de Google Drive](https://drive.google.com/drive/folders/1_2z5jXTZoaz1GhUMGMR4sfZFwwfNUDNt?usp=sharing).
En la carpeta se recogen las gráficas de:
- Clustering
- Visualización de las distribuciones
- Comparación entre variables
- Matriz de correlación
- Resultados

## Preprocesado
Para el preprocesado se han llevado a cabo los siguientes estudios:
- Correlación de variables
- Integridad de los datos
- División de los datos (70% Train y 30% Test)

## Propuestas
Los algoritmos elegidos para la posterior extracción de los modelos han sido:
- Naive Bayes
- Redes Neuronales
- k-NN
- Árboles de Decisión (C5.0)
- SVM

Las medidas de bondad con las que hemos elegido medir los resultados de los modelos extraidos son:
- Curva ROC
- AUC

## Entrenamiento
### k-fold Cross Validation
Para llevar a cabo el entrenamiento de los datos sobre los algoritmos elegidos, hemos optado por realizar 
una validación cruzada de los datos de entrenamiento. El valor de k elegido ha sido k=10.

### Rendimiento de los modelos
Tras llevar a cabo la extracción de los modelos resultantes de la aplicación de los algoritmos al conjunto de entrenamiento, 
hemos medido el rendimiento de cada uno con la medida de bondad AUC.
En esta medición hemos determinado que el modelo que mejor rendimiento da es el de Árboles de Decisión.

![img](http://i.imgur.com/yourfilename.png)

### Stacking
Se ha hecho uso del método de stacking para comparar los modelos generados y ver si podemos obtener uno mejor.

## Blind Test
Para hacer el test ciego, hemos usado el 30% del conjunto de datos reservado y le hemos aplicado los modelos extraidos del entrenamiento.
En la siguiente gráfica se puede observar que presentan cierta similaridad a los resultados medidos en el entrenamiento, por tanto podemos
deducir que los modelos extraidos son consistentes.
- Una peculiaridad es que al haber hecho stacking estamos obteniendo un modelo con rendimiento un poco mejor que el de 
árboles de decisión que era el que más AUC presentaba.

## Desarrollado por:
- Ezequiel Rodríguez
- Isa Amaya
- Pedro Cano
- José Gallardo

## Documentación
- Memoria:
- Presentación:

