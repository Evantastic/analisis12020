library("C50")
library("caret")

# Semilla
set.seed(88)

url <- "https://raw.githubusercontent.com/Evantastic/analisis12020/master/agaricus-lepiota.data"
mushrooms <- read.table(url, header = TRUE, sep = ",")

# Eliminamos atributos que no aportan a la clasificacion
mushrooms$veil.type <- NULL
mushrooms$gill.attachment <- NULL
mushrooms$veil.color <- NULL
mushrooms$ring.number <- NULL

#mushrooms$class <- factor(mushrooms$class, levels = c(1,2), labels = c("e", "p"))

# Convertimos las columnas a factores
#for(i in 1:ncol(mushrooms)) mushrooms[,i] <- as.factor(mushrooms[,i])

training.index = createDataPartition(mushrooms$class, p=0.7)$Resample1
training.set = mushrooms[training.index, ]
test.set = mushrooms[-training.index, ]
tree = C5.0(class ~ ., training.set)
tree.rules = C5.0(x = training.set[, -20], y = training.set$class, rules = T)
tree.pred.class = predict(tree, test.set[,-20], type = "class")
tree.pred.prob = predict(tree, test.set[,-20], type = "prob")

#Para la consola
#tree.pred.class
#head(tree.pred.prob)
#plot(tree)
#summary(tree)
#summary(tree.rules)
#conf.matrix.tree = confusionMatrix(table(test.set$class, tree.pred.class))
#print(conf.matrix.tree)


