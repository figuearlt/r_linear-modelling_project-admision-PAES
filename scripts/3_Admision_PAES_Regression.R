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
par(mfrow = c(1, 1))
influencePlot(logit)
# Revisar las observaciones que más influencias los coeficientes
influencia <- influence.measures(logit)
summary(influencia)
# Identificar el valor que afecta
obs_influyentes <- which(apply(influencia$is.inf, 1, any))
obs_influyentes

# Datos Influyentes

influyentes <- paes.train[obs_influyentes, ]
# Aplicar condición dentro de ese subconjunto
influyentes_filtradas <- influyentes[
  influyentes$nem >= 6 &
    (influyentes$rank == "Grupo D" | influyentes$rank == "Grupo C"),
]

influyentes_filtradas

############################################
# Guardamos el dataframe en un archivo csv #
############################################
paes.train_sin_influyentes <-paes.train[-obs_influyentes, ]
write.csv(paes.train_sin_influyentes,'/cloud/project/data/processed/datos_admision/AdmisionUes_Ajustado_sin_Influyentes.csv',row.names = FALSE)
saveRDS(paes.train_sin_influyentes, "/cloud/project/data/processed/datos_admision/AdmisionUes_Ajustado_sin_Influyentes.rds")

######################################
# Nuevo Modelo Logit sin Influyentes #
######################################

# Logit sin influyentes (Logit_2):
logit_2 <- glm(data=paes.train_sin_influyentes,admit~paes_std+nem+rank,family='binomial')
summary(logit_2)

######################################
# Nuevo Modelo Logit con Interacción #
######################################
# Logit sin influyentes y con interacción (Logit_3):
logit_3 <- glm(data=paes.train_sin_influyentes,admit~paes_std*rank+nem,family='binomial')
summary(logit_3)

