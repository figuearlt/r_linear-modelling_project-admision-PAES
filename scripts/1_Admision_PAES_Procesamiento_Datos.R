#############
# LIBRERÍAS #
#############

library(MASS)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)
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
# Primeras 10 líneas
head(paes)

# Min, Max, AVG, Median Variables
summary(paes)

# Naturaleza de las variables
str(paes)
# Dimensión de la base
dim(paes)
# Cuántos valores duplicados existen?
sum(duplicated(paes))

# Cambio la naturaleza de las variables
paes2 <- within(paes,{
  admit <- factor(admit,labels=c("No Admitido","Admitido"))
  rank <- factor(rank,labels=c("A","B","C","D"))
})

summary(paes2)
str(paes2)
# Transformar la variable X en el índice

rownames(paes)<-paes2$X

# Valores nulos por variable
any(is.na(paes2)) # ¿Cuáles na? 
sum(is.na(paes2)) # ¿Cuántos na?
#which(apply(paes, 1, function(x) any(is.na(x))))

# Valores únicos por variable
length(unique(paes2$X))
length(unique(paes2$admit))
length(unique(paes2$paes))
length(unique(paes2$nem))
length(unique(paes2$rank)) 

head(paes2)


############################################
# Guardamos el dataframe en un archivo csv #
############################################

write.csv(paes2,'/cloud/project/data/processed/datos_admision/AdmisionUes_Ajustado.csv',row.names = FALSE)

#---