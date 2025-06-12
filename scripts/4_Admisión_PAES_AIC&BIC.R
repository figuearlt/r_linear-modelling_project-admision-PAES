#########################
# Identificar el modelo #
#########################

library(MASS)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)
#install.packages("carData")
library(carData)
#install.packages("car")
library(car)
#install.packages("ppcor")
library(ppcor)



################################
# Abrir el dataframe procesado #
################################
path <- '/cloud/project/data/processed/datos_admision'
archivo <- 'AdmisionUes_Ajustado_sin_Influyentes.rds'
ruta_completa <- file.path(path,archivo)
paes <- readRDS(ruta_completa)
head(paes)


##########################################################
## Confección de los modelos para trabajar con AIC y BIC##
##########################################################


#################
## AIC stepwise##
#################

modelo_base <- glm(admit ~ paes_std, data = paes, family = binomial)
modelo_completo <- glm(admit ~ paes_std + nem + rank, data = paes, family = binomial)

step(modelo_base, scope = formula(modelo_completo), direction = "forward")
step(modelo_completo, direction = "backward")
step(modelo_completo, direction = "both")


#################
## BIC stepwise##
#################


step(modelo_base, scope = formula(modelo_completo), direction = "forward", k = log(nrow(paes)))
step(modelo_completo, direction = "backward", k = log(nrow(paes)))
step(modelo_completo, direction = "both", k = log(nrow(paes)))
