#############
# LIBRERÍAS #
#############

library(MASS)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)
#install.packages("carg")
library(car)

################################
# Abrir el dataframe procesado #
################################
path <- '/cloud/project/data/processed/datos_admision'
archivo <- 'AdmisionUes_Ajustado.csv'
ruta_completa <- file.path(path,archivo)
paes <- read.csv(ruta_completa,header=TRUE,)
head(paes)

###################################
# Abrir el dataframe sin procesar #
###################################

##Debe hacerse con la matriz original, aunque tengo mis dudas si es de relevancia este análisis,
##dada la naturaleza de 4 de las 5 variables.

path <- '/cloud/project/data/raw/datos_admision'
archivo <- 'AdmisionUes.csv'
ruta_completa <- file.path(path,archivo)
paes.raw <- read.csv(ruta_completa,header=TRUE,)
head(paes.raw)

#################################
# Correlación Parcial y General #
#################################

cor(paes.raw [, -1], y = NULL, use = "everything", method = c("kendall"))
install.packages("ppcor")
library(ppcor)
pcor(paes.raw [, -1], method="kendall")

###################################################
# Regresión Logística e Identificación del Modelo #
###################################################

# Regresión Logística
logit<-glm(admit~rank + paes+ nem,
           data = paes,
           family =binomial(link = "logit"))
summary(logit)

modelo_logit <- glm(
  admit ~ paes + nem + rank,
  data = paes.raw,
  family = binomial()
)
summary(modelo_logit)
exp(modelo_logit$coefficients)

# Revisar e interpretar los resultados de la s tablas de una reg Logit
# Sabeer interpretar las odds
# VIF
# Distancia de Cook
# Estandarizar variable PAES con el valor Z
# categorización correcta ( para trabajar en el modelo) con la variable ranking


#Identificar el modelo por medio de backward, forward, stepwise
# AIC con Forward
fit1<-lm(mpg~1,data=mtcars2)# Punto de partida
forw<-stepAIC(fit1,scope = list(upper=~cyl+disp+hp+drat+wt+qsec+vs+am+gear+carb,lower=~1),direction='forward')