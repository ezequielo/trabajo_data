library(ggcorrplot)
d <- read.csv2("winequality-red.csv", sep=",")

# Correlation matrix
corr <- round(cor(d), 2)
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


M <- cor(d)
M <- M - sign(M) * (abs(M) %% 0.01)
corrplot(M, method = "circle", type = "upper", order = "original",
         addCoef.col = "white", number.cex = 0.8,
         sig.level = 0.05, insig = "blank", diag = FALSE)

