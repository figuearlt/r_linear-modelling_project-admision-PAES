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
#install.packages("pROC")
library(pROC)

################################
# Abrir el dataframe procesado #
################################
path <- '/cloud/project/data/processed/datos_admision'
archivo <- 'AdmisionUes_Ajustado.rds'
ruta_completa <- file.path(path,archivo)
paes <- readRDS(ruta_completa)


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
# Resetear valores del índice de la variable
rownames(paes.train) <- NULL
head(rownames(paes.train))

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
# Resetear valores del índice de la variable
rownames(paes.train_sin_influyentes) <- NULL
head(rownames(paes.train_sin_influyentes))
write.csv(paes.train_sin_influyentes,'/cloud/project/data/processed/datos_admision/AdmisionUes_Ajustado_sin_Influyentes.csv',row.names = FALSE)
saveRDS(paes.train_sin_influyentes, "/cloud/project/data/processed/datos_admision/AdmisionUes_Ajustado_sin_Influyentes.rds")


######################################
# Nuevo Modelo Logit sin Influyentes #
######################################

# Logit sin influyentes (Logit_2):
modelo_logit_2 <- glm(data=paes.train_sin_influyentes,admit~paes_std+nem+rank,family='binomial')
summary(modelo_logit_2)

######################################
# Nuevo Modelo Logit con Interacción #
######################################
# Logit sin influyentes y con interacción (Logit_3):
modelo_logit_3 <- glm(data=paes.train_sin_influyentes,admit~paes_std*rank+nem,family='binomial')
summary(modelo_logit_3)
#--


##########################################################
## Confección de los modelos para trabajar con AIC y BIC##
##########################################################


#################
## AIC stepwise##
#################

modelo_base <- glm(admit ~ paes_std, data = paes.train_sin_influyentes, family = binomial)
modelo_completo <- glm(admit ~ paes_std*rank+ nem, data = paes.train_sin_influyentes, family = binomial)

step(modelo_base, scope = formula(modelo_completo), direction = "forward")
step(modelo_completo, direction = "backward")
step(modelo_completo, direction = "both")


#################
## BIC stepwise##
#################

step(modelo_base, scope = formula(modelo_completo), direction = "forward", k = log(nrow(paes)))
step(modelo_completo, direction = "backward", k = log(nrow(paes)))
step(modelo_completo, direction = "both", k = log(nrow(paes)))
# Los 3 procedimientos de selección, para cada uno de ambos criterios de selección (AIC/BIC) entregan que el modelo aditivo completo es el mejor

#####################
#  SELECCION MODELO #
#####################

AIC(modelo_logit_2,modelo_logit_3)
BIC(modelo_logit_2,modelo_logit_3)
modelo_final <- modelo_logit_2
summary(modelo_final)

#######################
# TEST DE LOS MODELOS #
#######################

# Multicolinealidad
car::vif(modelo_final) # No existe multicolinealidad entre los parámetros

# Influencia de los outliers

influencePlot(modelo_final)
cooksd <- cooks.distance(modelo_final)
which(cooksd > 4 / nrow(paes.train_sin_influyentes))
paes.train_sin_influyentes[c(33,413,421, 787, 1002, 1155), ]


str(paes.train_sin_influyentes)

# Curva ROC y AUC

# Estandarización de la variable paes, según la media y sd de paes train
media_paes <- mean(paes.train$paes, na.rm = TRUE)
desv_paes  <- sd(paes.train$paes, na.rm = TRUE)
paes.test$paes_std <- (paes.test$paes - media_paes) / desv_paes
  
prob <- predict(modelo_final, newdata = paes.test, type = "response")
roc_curve <- roc(paes.test$admit, prob)
auc(roc_curve)
plot(roc_curve)
