library(ggplot2)

# Establecemos algunas constantes
url                            <- "https://raw.githubusercontent.com/Evantastic/analisis12020/master/agaricus-lepiota.data"
edible.class                   <- "e"
poisonous.class                <- "p"
odor.almond                    <- "a"
odor.anise                     <- "l"
odor.none                      <- "n"

# Leemos los datos
mushrooms <- read.table(url, header = TRUE, sep = ",")

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


# Data for the confusion matrix
tp <- n.poisonous.satisfy.rules
fp <- n.edible.satisfy.rules
fn <- n.poisonous - n.poisonous.satisfy.rules
tn <- n.edible - n.edible.satisfy.rules
# Matriz de confusion para resumir los resultados de las pruebas aplicadas tiene la siguiente forma
#                          Venenosos Comestibles
# Venenosos(Predichos)        tp        fp
# Comestibles(Predichos)      fn        tn
confusion.matrix <- matrix(c(tp, fp, fn, tn),
                           ncol = 2,
                           byrow = TRUE)
colnames(confusion.matrix) <- c("Venenosos", "Comestibles")
rownames(confusion.matrix) <- c("Venenosos (Predicho)", "Comestibles (Predicho)")
cat("Matriz de confusion", "\n")
print(confusion.matrix)
# Summary of the confusion matrix
confusion.matrix.summary <- matrix(c(tp/(tp + fn), tn/(tn + fp), tp/(tp + fp), tn/(tn + fn),
                                     fn/(fn + tp), fp/(fp + tn), fp/(fp + tp), fn/(fn + tn), tp/(tp + fn + fp)),
                                   ncol = 1)
colnames(confusion.matrix.summary) <- c("Valor")
rownames(confusion.matrix.summary) <- c("Sensibilidad", "Especificidad", "Precision", "Valor predictivo negativo",
                                        "Tasas de fallo", "Tasa de falsa alarma", "Tasa de descubrimiento falso",
                                        "Tasa de omision falsa", "Indice de exito")
cat("\n","Resumen de la matriz de confusion", "\n")
print(confusion.matrix.summary)

# Prueba exacta de Fisher
# Hipotesis nula: Los 3 olores en especifo (ningunom, anis, almendra) no determinan la comestibilidad de un hongo. Las variables son indepenedientes
# Hipotesis alternativa: Los 3 olores en especifo(ninguno, anis, almendra) determinan la comestibilidad de un hongo. Las variables no son independientes

test <- fisher.test(mushrooms$class, mushrooms$odor, simulate.p.value = TRUE)
cat("\n","P-valor de la prueba exacta de Fisher","\n")
cat(test$p.value)
