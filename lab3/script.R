library(ggplot2)
library(arulesViz)

# Semilla
set.seed(88)

url <- "https://raw.githubusercontent.com/Evantastic/analisis12020/master/agaricus-lepiota.data"
mushrooms <- read.table(url, header = TRUE, sep = ",")
for(i in 1:ncol(mushrooms)) mushrooms[,i] <- as.factor(mushrooms[,i])

rules = apriori(
  data = mushrooms,
  parameter = list(support = 0.35, minlen = 1, maxlen = 4),
  appearance = list(rhs = c("class=p", "class=e"))
)

inspect(sort(x = rules, decreasing = TRUE, by = "confidence"))
inspect(sort(x = rules, decreasing = TRUE, by = "support"))