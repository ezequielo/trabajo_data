
#dependencias
install.packages("ggplot2",dependencies=TRUE)
library(ggplot2)
library(ggcorrplot)
library(corrplot)
install.packages("ggExtra",dependencies=TRUE)
library(ggExtra)

library(ggcorrplot)
install.packages("magrittr") # only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # need to run every time you start R and want to use %>%
library(dplyr)    # alternative, this also loads %

library(pander)
library(caret)
library(mlbench)
library(doParallel)

library(corrplot)


wine <- read.csv2("winequality-red.csv", sep=",")
head(wine)
# Compute a correlation matrix

#corr
#Correlogram
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 2, 
           method="circle", 
           colors = custom_colors, 
           title="Correlogram of wine measurements", 
           ggtheme=theme_bw)


M <- cor(wine)
M <- M - sign(M) * (abs(M) %% 0.01)
corrplot(M, method = "circle", type = "upper", order = "original",
         addCoef.col = "white", number.cex = 0.8,
         sig.level = 0.05, insig = "blank", diag = FALSE)

#Integridad
na_count <-sapply(wine, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count



