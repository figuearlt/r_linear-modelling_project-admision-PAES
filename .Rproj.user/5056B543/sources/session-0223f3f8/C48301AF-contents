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
  rank <- factor(rank,labels=c("A","B","C","D"))
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
ggplot(paes2, aes(x = rank)) +
  geom_bar(fill = "steelblue") + 
  labs(title = "Distribución de Rank en PAES",
       x = "Rank Obtenido",
       y = "Frecuencia") +
  theme_minimal()

ggplot(paes2, aes(x = rank)) +
  geom_boxplot()



#---