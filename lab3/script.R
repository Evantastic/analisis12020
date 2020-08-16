library(ggplot2)
library(arulesViz)

# Semilla
set.seed(88)

url <- "https://raw.githubusercontent.com/Evantastic/analisis12020/master/agaricus-lepiota.data"
mushrooms <- read.table(url, header = TRUE, sep = ",")

# Eliminamos atributos que no aportan a la clasificacion
mushrooms$veil.type <- NULL
mushrooms$gill.attachment <- NULL
mushrooms$veil.color <- NULL
mushrooms$ring.number <- NULL

# Convertimos las columnas a factores
for(i in 1:ncol(mushrooms)) mushrooms[,i] <- as.factor(mushrooms[,i])

# Obtenemos las reglas
rules = apriori(
  data = mushrooms,
  parameter = list(support = 0.35, minlen = 1, maxlen = 4),
  appearance = list(rhs = c("class=p", "class=e"))
)

# Se imprime las reglas en base a la confianza
inspect(sort(x = rules, decreasing = TRUE, by = "confidence"))
# Se imprime las reglas en base al soporte
inspect(sort(x = rules, decreasing = TRUE, by = "support"))

# Funcion que aplica la regla 1 a una fila
apply_rule_1 <- function(row) {
  if (row["odor"] == "n") {
    return("e predicho")
  } else {
    return("p predicho")
  }
}
# Funcion que aplica la regla 2 a una fila
apply_rule_2 <- function(row) {
  if (row["odor"] == "n" && row["gill.size"] == "b") {
    return("e predicho")
  } else {
    return("p predicho")
  }
}
# Funcion que aplica la regla 3 a una fila
apply_rule_3 <- function(row) {
  if (row["bruises"] == "f" && row["gill.spacing"] == "c") {
    return("p predicho")
  } else {
    return("e predicho")
  }
}

# Agregamos las clases predichas al data frame
mushrooms$first.rule.class <- apply(mushrooms, 1, apply_rule_1)
mushrooms$second.rule.class <- apply(mushrooms, 1, apply_rule_2)
mushrooms$third.rule.class <- apply(mushrooms, 1, apply_rule_3)

# Se imprime las clases predichas
writeLines("\n\n\n\nMatriz de confusion de la regla 1")
print(table(mushrooms$class, mushrooms$first.rule.class))
writeLines("\n\nMatriz de confusion de la regla 2")
print(table(mushrooms$class, mushrooms$second.rule.class))
writeLines("\n\nMatriz de confusion de la regla 3")
print(table(mushrooms$class, mushrooms$third.rule.class))
writeLines("\n\n\n")
