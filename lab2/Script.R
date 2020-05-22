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