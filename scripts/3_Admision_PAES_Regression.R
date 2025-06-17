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

# Posit Cloud
#path <- '/cloud/project/data/processed/datos_admision'

# Disco Local
path <- 'C:/Users/diego/OneDrive/Escritorio/Diplomado Data Science/Diplomado PUCV/r_linear_modelling_project_admision_PAES/r_linear-modelling_project-admision-PAES/data/processed/datos_admision'
archivo <- 'AdmisionUes_Ajustado.rds'
ruta_completa <- file.path(path,archivo)
paes <- readRDS(ruta_completa)
head(paes)

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

#Estandarización de variable paes y nem
paes.train$paes_std<-scale(paes.train$paes)[,1]
sd_paes<-sd(paes.train$paes)
print(sd_paes)

paes.train$nem_std<-scale(paes.train$nem)[,1]
sd_nem<-sd(paes.train$nem)
print(sd_nem)

# Regresión Logística
logit<-glm(admit~rank + paes_std+ nem_std,
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


###############################
# Disgnóstico de los Outliers #
###############################

p<-ncol(paes.train)-1 # Por admit
n<-nrow(paes.train)
sqrt_n<-sqrt(n)

# Distancia de Cook
par(mfrow = c(1, 1))
influencePlot(logit,id.method='identify',main="Influence Plot")
summary(influence.measures(logit))
    # Para Hatvalues
    # Qué mide: cuán lejos está una observación del centro del espacio de los predictores.
      high_leverage <- which(hatvalues(logit)>2*p/n)
    # Para Residuos Studentizados
    # Qué mide: cuán inusual es la respuesta observada dado el modelo ajustado, excluyendo la observación en cuestión.
      outliers_rstud <- which(abs(rstudent(logit))>2)
    # Para Distnacia de Cook
    # Qué mide: el efecto total que tendría eliminar una observación sobre todos los coeficientes del modelo.
      dist_cook_measure <- 4/(n-p-1)
      print(dist_cook_measure)
      outliers_dist_cook <- which(cooks.distance(logit)>dist_cook_measure)
    # Para DFBetas 
    # Qué mide: cuánto cambia cada coeficiente si eliminamos una observación específica.
      logit_dfbeta <- which(abs(dfbetas(logit))>2/sqrt_n, arr.ind = FALSE)
    # Para DFFITS
    # Qué mide: cuánto cambia la predicción para una observación si esta es excluida del modelo.
      logit_dffits<-which(abs(dffits(logit))>2*sqrt(p/n)) 

      
      
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
#POSIT CLOUD
#write.csv(paes.train_sin_influyentes,'/cloud/project/data/processed/datos_admision/AdmisionUes_Ajustado_sin_Influyentes.csv',row.names = FALSE)
#saveRDS(paes.train_sin_influyentes, "/cloud/project/data/processed/datos_admision/AdmisionUes_Ajustado_sin_Influyentes.rds")
# DISCO LOCAL
write.csv(paes.train_sin_influyentes,'C:/Users/diego/OneDrive/Escritorio/Diplomado Data Science/Diplomado PUCV/r_linear_modelling_project_admision_PAES/r_linear-modelling_project-admision-PAES/data/processed/datos_admision/AdmisionUes_Ajustado_sin_Influyentes.csv',row.names = FALSE)
saveRDS(paes.train_sin_influyentes, "C:/Users/diego/OneDrive/Escritorio/Diplomado Data Science/Diplomado PUCV/r_linear_modelling_project_admision_PAES/r_linear-modelling_project-admision-PAES/data/processed/datos_admision/AdmisionUes_Ajustado_sin_Influyentes.rds")
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
