library(cluster)
library(factoextra)
library(ggplot2)

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
# Se convierten los datos a factores
for(i in 1:ncol(mushrooms)) mushrooms[,i] <- as.factor(mushrooms[,i])

# Se agrupan datos con 2 hasta 8 clusters
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
# Maximizando el ancho de siluetas, el numero de clusters optimo son 5
pam <- pam(mushrooms, k = 5)
mushrooms <- cbind(mushrooms, cluster_df = pam$cluster)