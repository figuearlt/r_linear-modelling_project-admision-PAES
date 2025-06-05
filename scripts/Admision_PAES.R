#############
# LIBRERÍAS #
#############

library(MASS)
#install.packages("ggplot2")
library(ggplot2)
#---
  
################
# CARGAR DATOS #
################


# Cargar datos ya almacenados en el Proyecto Posit Cloud
path <- '/cloud/project/data/raw/datos_admision'
archivo <- 'AdmisionUes.csv'
ruta_completa <- file.path(path,archivo)
paes <- read.csv(ruta_completa,header=TRUE,)

#---

#########################
# EXPLORACION BASE PAES #
#########################

# Min, Max, AVG, Median Variables
summary(paes)

# Valores nulos por variable
any(is.na(paes))
#which(apply(paes, 1, function(x) any(is.na(x))))

# Valores únicos por variable
length(unique(paes$X))
length(unique(paes$admit))
length(unique(paes$paes))
length(unique(paes$nem))
length(unique(paes$rank)) # Variable Ranking hay que revisar la documentación de qué valores representa


#---
  

#######################
# ANALISIS UNIVARIADO #
#######################

# Ranking
ggplot(paes, aes(x = rank)) +
  geom_histogram(binwidth = 1, fill = "darkgreen", color = "black", alpha = 0.8) + # Histograma con binwidth ajustable
  theme_minimal() + labs(title = 'Histograma Ranking', x='Ranking', y='Freq')

ggplot(paes, aes(x = rank)) +
  geom_boxplot()



#---