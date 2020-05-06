library(ggplot2)

# Leemos los datos
filename <- "expanded"
edible_class <- "EDIBLE"
poisonous_class <- "POISONOUS"
mushrooms <- read.table(filename,header=TRUE,sep=",")

# Separamos los datos en clases comestibles y venenosos
edible <- mushrooms[ which(mushrooms$class==edible_class),]
poisonous <- mushrooms[ which(mushrooms$class==poisonous_class),]

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
  edible_attribute$class <- edible_class
  poisonous_attribute$class <- poisonous_class
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

#Función para aplicar las reglas
apply_rules <- function(data){
  rules.step.1 <- data[ which(data$odor == "ALMOND" | 
                              data$odor == "ANISE" | 
                              data$odor == "NONE"),]
  #Rule 2
  rules.step.2 <- rules.step.1[ which(rules.step.1$spore.print.color != "GREEN"),]
  #Rule 3
  rules.step.3 <- rules.step.2[ which(rules.step.2$odor != "NONE" | 
                                      rules.step.2$stalk.surface.below.ring != "SCALY" | 
                                      rules.step.2$stalk.color.above.ring == "BROWN"),]
  rules.step.3
}

poisonous.rules <- apply_rules(poisonous)
n_poisonous.not.satisfy.rules <- nrow(poisonous.rules)
n_poisonous.satisfy.rules <- n_poisonous - n_poisonous.not.satisfy.rules

edible.rules <- apply_rules(edible)
n_edible.not.satisfy.rules <- nrow(edible.rules)
n_edible.satisfy.rules <- n_edible - n_edible.not.satisfy.rules

#Tabla de contingencia para resumir los resultados de las pruebas aplicadas
test.results <- data.frame(Pruebas = factor(c("No cumplen reglas","Cumplen reglas")), 
                      Comestibles = c(n_edible.not.satisfy.rules, n_edible.satisfy.rules), 
                      Venenosos = c(n_poisonous.not.satisfy.rules, n_poisonous.satisfy.rules))




