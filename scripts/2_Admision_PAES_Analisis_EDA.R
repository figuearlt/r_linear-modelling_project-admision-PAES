#############
# LIBRERÍAS #
#############

library(MASS)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)
#---

################################
# Abrir el dataframe procesado #
################################
path <- '/cloud/project/data/processed/datos_admision'
archivo <- 'AdmisionUes_Ajustado.csv'
ruta_completa <- file.path(path,archivo)
paes <- read.csv(ruta_completa,header=TRUE,)
head(paes)
#######################
# ANALISIS UNIVARIADO #
#######################

# Proporción de admitidos vs no admitidos
tabla_admitidos<-data.frame(table(paes$admit))
colnames(tabla_admitidos)=c("Admitido/No Admitido","N")
tabla_admitidos
tabla_admitidos$proporcion<-round(tabla_admitidos$N / sum(tabla_admitidos$N) *100,3)
colnames(tabla_admitidos)=c("Admitido/No Admitido","N","%")
tabla_admitidos
# Es relevante hacer este cálculo de proporciones, debido a que al momento de hacer el sampling de la data, podemos encontrarnos con una base desproporcionada hacia los Admitidos, cayendo en un caso de Underfitting 
# Esto también impacta el sesgo del modelo, incrementándolo, sin obtener ventajas en cuanto a la varianza.

# Admitido
ggplot(paes, aes(x = admit)) +
  geom_bar(fill = "#58508d") + 
  labs(title = "Distribución de Admisión",
       x = "Admisión",
       y = "Frecuencia") +
  theme_minimal()



# Ranking
p2<-ggplot(paes, aes(x = rank)) +
  geom_bar(fill = "#003f5c") + 
  labs(title = "Distribución de Rank en PAES",
       x = "Rank Obtenido",
       y = "Frecuencia") +
  theme_minimal()

# NEM
p3<-ggplot(paes, aes(x = nem)) +
  geom_bar(fill = "#ff6361") + 
  labs(title = "Distribución de Notas
        de Enseñanza Media",
       x = "NEM Obtenido",
       y = "Frecuencia") +
  theme_minimal()

head(paes)

# PAES
p4<-ggplot(paes, aes(x = paes)) +
  geom_histogram(fill = "#ffa600",bins=50,alpha=0.8) + 
  labs(title = "Distribución de PAES",
       x = "Pje PAES Obtenido",
       y = "Frecuencia") +
  theme_minimal()

# Admitido
p1<-ggplot(paes, aes(x = admit)) +
  geom_bar(fill = "#58508d") + 
  labs(title = "Distribución de Admisión",
       x = "Admisión",
       y = "Frecuencia") +
  theme_minimal()


# Organizarlos en 2x2
grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)


#######################
# ANALISIS BIVARIADO #
#######################


# Ranking y PAES

g1<-ggplot(paes, aes(x = rank, y=paes)) +
  geom_boxplot(fill = "#003f5c") +
  labs(title = "",
       x = "Ranking",
       y = "Puntaje PAES") +
  theme_minimal()

# NEM y PAES
g2<-ggplot(paes, aes(x = nem,y=paes)) +
  geom_point(color='#ff6361') + 
  labs(title = "",
       x = "NEM Obtenido",
       y = "Puntaje PAES") +
  theme_minimal()

# Admitido y PAES
g3<-ggplot(paes, aes(x = admit, y=paes)) +
  geom_boxplot(fill = "#58508d") +
  labs(title = "",
       x = "Admisión",
       y = "Puntaje PAES") +
  theme_minimal()

# Admitido y PAES
g4<-ggplot(paes, aes(x = admit, y=nem)) +
  geom_boxplot(fill = "#ffa600") +
  labs(title = "",
       x = "Admisión",
       y = "Puntaje NEM") +
  theme_minimal()

# Admitido y Ranking

g6<-ggplot(paes,aes(x = rank, fill = admit)) +
  geom_bar(position = "fill",color='#003f5c' ) +
  labs(y = "Proporción", title = "Proporción de Admitidos por Ranking")


# NEM y Ranking

g5<-ggplot(paes, aes(x = rank, y=nem)) +
  geom_boxplot(fill = "#dd5182") +
  labs(title = "",
       x = "Ranking",
       y = "Puntaje NEM") +
  theme_minimal()

# Organizarlos en 3x3
grid.arrange(g1, g2, g3,g4,g5,g6, nrow = 3, ncol = 2)


# Matriz de Correlación entre variables con método kendall
# Evaluar PCA
# Distnacia de Cook para evaluar efecto de outliers en la varianza de la variable admisión
# Tratamiento de variables con outliers
# regresión logística
