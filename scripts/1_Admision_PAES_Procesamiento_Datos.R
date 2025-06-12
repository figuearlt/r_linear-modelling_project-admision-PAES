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
  admit <- as.factor(admit)
  rank <- factor(rank,labels=c("Grupo A","Grupo B","Grupo C","Grupo D"),
                 levels=c(1,2,3,4),ordered = FALSE)
  
})


summary(paes2)
str(paes2)

# Transformar la variable X en el índice

rownames(paes2)<-paes2$X
# Eliminar variable X (Id)
paes3 <-paes2[,-1]
head(paes3)

# Estandarizar la variable paes

paes4<-paes3
head(paes4)


# Valores nulos por variable
any(is.na(paes4)) # ¿Cuáles na? 
sum(is.na(paes4)) # ¿Cuántos na?

# Valores únicos por variable

length(unique(paes4$admit))
length(unique(paes4$paes_std))
length(unique(paes4$nem))
length(unique(paes4$rank)) 

head(paes4)
str(paes4)


############################################
# Guardamos el dataframe en un archivo csv #
############################################

write.csv(paes4,'/cloud/project/data/processed/datos_admision/AdmisionUes_Ajustado.csv',row.names = FALSE)
saveRDS(paes4, "/cloud/project/data/processed/datos_admision/AdmisionUes_Ajustado.rds")
#---


