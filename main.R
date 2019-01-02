library(ggcorrplot)
install.packages("ggplot2",dependencies=TRUE)
library(ggplot2)
install.packages("ggExtra",dependencies=TRUE)
library(ggExtra)
wine <- read.csv2("winequality-red.csv", sep=",")

data("wine", package = "ggplot2")
head(wine)
# Compute a correlation matrix

corr <- round(cor(wine), 1)
head(corr[, 1:12])


library(RColorBrewer)


# Load the data into variable d
d=read.csv("winequality-red.csv")

#Create custom colors vector
custom_colors=c("#E32800", "#FDB205","#FDF505","#009BDF","#E3FD05","#A7FD05","#7CBE00","#639700","#972000","#871D00","#50FF95","#00DEAF","#00B891","#00B5B8","#0080B8","#0063E8","#0047A7","#9F55FF","#C69BFF","#D69BFF","#B956FE","#DF56FE","#FE5681","#9BDF00")
glimpse(head(d,5))

#Integridad
na_count <-sapply(d, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count
#Visualizaciones por atributos
d$pH[as.character(d$pH)==""] <- NA
#Create histogram plot with variable and label parameters
ggplot(d, aes(x=density, fill=pH))+
  geom_histogram(binwidth=1, alpha=.5, position="dodge")+
  labs(y="pH por denstidad",x="densidad",title="Participant Numbers By Age (Gender)")+
  scale_fill_manual(values=custom_colors,
                    name="pH\n",
                    #breaks=c("Female", "Male", "NA"),
                    labels=c("Female", "Male", "NA"))
