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
str(paes)

# Cambio la naturaleza de las variables
paes2 <- within(paes,{
  admit <- factor(admit,labels=c("Admitido","No Admitido"))
  rank <- factor(rank,labels=c("D","C","B","A"))
})

summary(paes2)
str(paes2)

# Valores nulos por variable
any(is.na(paes2))
#which(apply(paes, 1, function(x) any(is.na(x))))

# Valores únicos por variable
length(unique(paes2$X))
length(unique(paes2$admit))
length(unique(paes2$paes))
length(unique(paes2$nem))
length(unique(paes2$rank)) 

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