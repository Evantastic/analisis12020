library(ggplot2)

# Establecemos algunas constantes
filename                       <- "expanded"
edible.class                   <- "EDIBLE"
poisonous.class                <- "POISONOUS"
odor.almond                    <- "ALMOND"
odor.anise                     <- "ANISE"
odor.none                      <- "NONE"

# Leemos los datos
mushrooms <- read.table(filename, header = TRUE, sep = ",")

# Estilo de los graficos
mynamestheme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15), hjust = 0.5), 
                      legend.title = element_text(colour = "steelblue",  face = "bold.italic", family = "Helvetica"), 
                      legend.text = element_text(face = "italic", colour="steelblue4",family = "Helvetica"), 
                      axis.title = element_text(family = "Helvetica", size = (10), colour = "steelblue4"),
                      axis.text = element_text(family = "Courier", colour = "cornflowerblue", size = (10)))

# Grafico de aroma segun comestibilidad
plot <- ggplot(mushrooms, aes(x = class, fill = odor)) +
  geom_bar(position = "fill") +
  labs(y = "Proporcion", x = "Comestibilidad", fill = "Aroma") +
  ggtitle("Proporcion de aroma segun comestibilidad") +
  mynamestheme
print(plot)

# Grafico de color de las esporas segun comestibilidad
plot <- ggplot(mushrooms, aes(x = class, fill = spore.print.color)) +
  geom_bar(position = "fill") +
  labs(y = "Proportion", x = "Comestibilidad", fill = "Color de las esporas") +
  ggtitle("Proporcion de color de las esporas segun comestibilidad") +
  mynamestheme
print(plot)

# Función para aplicar la 1era regla negada
get.n.after.rules <- function(data, n){
  rules.step.1        <- data[which(data$odor == odor.almond |
                                    data$odor == odor.anise |
                                    data$odor == odor.none),]
  n.not.satisfy.rules <- nrow(rules.step.1)
  n.satisfy.rules     <- n - n.not.satisfy.rules
  n.satisfy.rules
}

# Separamos los datos en clases comestibles y venenosos
edible      <- mushrooms[which(mushrooms$class==edible.class),]
poisonous   <- mushrooms[which(mushrooms$class==poisonous.class),]
n.edible    <- nrow(edible)
n.poisonous <- nrow(poisonous)

n.poisonous.satisfy.rules <- get.n.after.rules(poisonous, n.poisonous)
n.edible.satisfy.rules    <- get.n.after.rules(edible, n.edible)

# Tabla de contingencia para resumir los resultados de las pruebas aplicadas
test.results <- matrix(c(n.edible.satisfy.rules, n.poisonous.satisfy.rules,
                         n.edible - n.edible.satisfy.rules, n.poisonous - n.poisonous.satisfy.rules),
                       ncol = 2,
                       byrow = TRUE)
colnames(test.results) <- c("Comestibles", "Venenosos")
rownames(test.results) <- c("Cumplen la regla", "No cumplen la regla")

# Prueba exacta de Fisher
# Hipotesis nula: Los 3 olores en especifo (ningunom, anis, almendra) no determinan la comestibilidad de un hongo. Las variables son indepenedientes
# Hipotesis alternativa: Los 3 olores en especifo(ninguno, anis, almendra) determinan la comestibilidad de un hongo. Las variables no son independientes

test <- fisher.test(test.results)
print(test)
