library(ggplot2)
library(cluster)
library(factoextra)
library(FactoMineR)
library(mice)
library(dplyr)

# Constantes
url <- "https://raw.githubusercontent.com/Evantastic/analisis12020/master/agaricus-lepiota.data"

# Semilla
set.seed(88)

# Estilo de graficos
personal.theme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15), hjust = 0.5), 
                      legend.title = element_text(colour = "steelblue",  face = "bold.italic", family = "Helvetica"), 
                      legend.text = element_text(face = "italic", colour="steelblue4",family = "Helvetica"), 
                      axis.title = element_text(family = "Helvetica", size = (10), colour = "steelblue4"),
                      axis.text = element_text(family = "Courier", colour = "cornflowerblue", size = (10)))

# Leemos los datos
mushrooms <- read.table(url, header = TRUE, sep = ",")
original <- mushrooms
# Eliminamos atributo innecesario (solo tiene un valor posible)
mushrooms$veil.type <- NULL
# Eliminamos las filas con datos faltantes
#mushrooms <- mushrooms[which(mushrooms$stalk.root != '?'),]
# Eliminamos la clse
mushrooms$class <- NULL

# Imputacion de datos
#init = mice(mushrooms, maxit=0) 
#meth = init$method
#predM = init$predictorMatrix
#meth[c("stalk.root")]="polyreg"
#mushroooms = mice(mushrooms, method=meth, predictorMatrix=predM, m=5)

# Se convierten los datos a factores
# for(i in 1:ncol(mushrooms)) mushrooms[,i] <- as.factor(mushrooms[,i])

#Resultados del análisis de correspondencia múltiple
res.mca <- MCA(mushrooms, graph = FALSE)

head(round(res.mca$var$eta2, 2), 23)

#Para visualizar los porcentajes de inercia explicados por cada dimensión MCA
fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 45)) +
  ggtitle("Analisis MCA") +
  personal.theme

#Para visualizar la correlación entre variables y dimensiones principales de MCA, escriba esto:
fviz_mca_var(res.mca, choice = "mca.cor", repel = TRUE) +
  ggtitle("Variables MCA") +
  personal.theme

# Se agrupan datos con 2 hasta 22 clusters
sil.width <- c(NA)
for (i in 2:22) {
  pam.fit <- pam(mushrooms, k = i)
  sil.width[i] <- pam.fit$silinfo$avg.width
}
# Se grafica el ancho promedio de las siluetas de los clusters
df <- data.frame(silhouette = sil.width,
                 clusters = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22))
plot <- ggplot(data = df, aes(x = clusters, y = silhouette, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  ggtitle("Silueta media vs cantidad de clusters") +
  personal.theme
# Maximizando el ancho de siluetas, el numero de clusters optimo es 2

# Se calcula el cluster en base al numero optimo de clusters segun el ancho de siluetas
pam <- pam(mushrooms, k = 2)

fviz_cluster(pam) + personal.theme

# Se agrega el atributo cluster al dataframe
mushrooms <- cbind(mushrooms, cluster = pam$cluster, class = original$class)

ggplot(mushrooms, aes(x = cluster, fill = class)) +
  geom_bar(position = "fill") +
  labs(y = "Proporcion", x = "Cluster", fill = "Clase") +
  ggtitle("Proporcion de clase dentro de los clusters") +
  personal.theme