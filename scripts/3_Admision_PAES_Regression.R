#############
# LIBRERÍAS #
#############

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
archivo <- 'AdmisionUes_Ajustado.rds'
ruta_completa <- file.path(path,archivo)
paes <- readRDS(ruta_completa)


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
paes_numeric <- within(paes,{
  admit<-as.numeric(admit)
})
cor(paes_numeric[, -4], method = "kendall")
pcor(paes_numeric [, -4], method="kendall")





###################################################
# Regresión Logística e Identificación del Modelo #
###################################################

# Datos de Entrenamiento y Testeo
set.seed(123)
train.filas <- sample(nrow(paes),.7*nrow(paes),replace=FALSE)
paes.train <- paes[train.filas,]
paes.test <- paes[-train.filas,]

#Estandarización de variable paes
paes.train$paes_std<-scale(paes.train$paes)[,1]
sd(paes.train$paes)

# Regresión Logística
logit<-glm(admit~rank + paes_std+ nem,
           data = paes.train,
           family =binomial())
summary(logit)
exp(logit$coefficients)

# Revisar e interpretar los resultados de la s tablas de una reg Logit
# Sabeer interpretar las odds

#########################
# Identificar el modelo #
#########################



###############################
# Multicolinealidad mediante  #
###############################

# ¿Existe Multicolinealidad de los parámetros? Análisis VIF
car::vif(logit)
cal.vif1<-car::vif(logit)
cal.vif1


##############################################
# Influencia de Outliers en los coeficientes #
##############################################

# Distancia de Cook




#########################
# Identificar el modelo #
#########################

#Identificar el modelo por medio de backward, forward, stepwise
# AIC con Forward
fit1<-lm(mpg~1,data=mtcars2)# Punto de partida
forw<-stepAIC(fit1,scope = list(upper=~cyl+disp+hp+drat+wt+qsec+vs+am+gear+carb,lower=~1),direction='forward')