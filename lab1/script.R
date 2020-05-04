library(ggplot2)

# Leemos los datos
filename <- "~/universidad/analisis/lab1/expanded"
mushrooms <- read.table(filename,header=TRUE,sep=",")
# Separamos los datos en clases comestibles y venenosos
edible <- mushrooms[ which(mushrooms$class=='EDIBLE'),]
poisonous <- mushrooms[ which(mushrooms$class=='POISONOUS'),]
# Eliminamos una variable que resulta que no se usara
poisonous$class <- NULL
edible$class <- NULL
# Obtenemos la cantidad de datos por clase y el total
n_edible <- nrow(edible)
n_poisonous <-nrow(poisonous)
# Obtenemos el nombre de los atributos y le restamos el atributo class
names <- colnames(mushrooms)
names <- names[-1]
# Creamos la tabla de contingencia class vs atributo, para todo atributo que no seas class
for (i in 1:length(names)) {
  # Obtenemos las tablas de frequencias de cada clase
  edible_attribute <- as.data.frame(table(edible[i]))
  poisonous_attribute <- as.data.frame(table(poisonous[i]))
  # Calculamos los porcentaje dadas las frequencias
  edible_attribute$percent = round(100*edible_attribute$Freq/n_edible, digits=2)
  poisonous_attribute$percent = round(100*poisonous_attribute$Freq/n_poisonous, digits=2)
  # Agregamos las clases comestible y venenoso
  edible_attribute$class <- "EDIBLE"
  poisonous_attribute$class <- "POISONOUS"
  # Creamos la tabla de contingencia
  contingency_table <- rbind(edible_attribute,poisonous_attribute)
  # Eliminamos el atributo de frequencia
  contingency_table$Freq <- NULL
  
  # Creamos un doble grafico de torta para cada atributo
  plot <- ggplot(contingency_table, aes(x="", y=percent, group=Var1, color=Var1, fill=Var1)) +
    geom_bar(width = 1, stat = "identity", color="white") +
    coord_polar("y", start=0) +
    facet_wrap(~ class) +
    theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid  = element_blank()) +
    xlab("") +
    ylab("") +
    labs(fill=names[i])
  # Se imprime el grafico
  print(plot)
}
