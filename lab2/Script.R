library(ggplot2)
library(fastDummies)
# Establecemos algunas constantes
url <- "https://raw.githubusercontent.com/Evantastic/analisis12020/master/agaricus-lepiota.data"

# Leemos los datos
mushrooms <- read.table(url, header = TRUE, sep = ",")

#Primer procesamiento -> Se eliminan datos con valores perdidos
mushrooms <- mushrooms[which(mushrooms$stalk.root != '?'),]

#Segundo procesamiento -> Se pasan los valores categóricos a binarios
mushrooms1 <- dummy_cols(mushrooms,  select_columns = c("class","cap.shape","cap.surface","cap.color",
                                                        "bruises","odor","gill.attachment",
                                                        "gill.spacing","gill.size","gill.color",
                                                        "stalk.shape","stalk.root","stalk.surface.above.ring",
                                                        "stalk.surface.below.ring","stalk.color.above.ring",
                                                        "stalk.color.below.ring","veil.type","veil.color",
                                                        "ring.number","ring.type","spore.print.color",
                                                        "population","habitat")) %>%
  
                                              select(-c("class","cap.shape","cap.surface","cap.color",
                                                        "bruises","odor","gill.attachment",
                                                        "gill.spacing","gill.size","gill.color",
                                                        "stalk.shape","stalk.root","stalk.surface.above.ring",
                                                        "stalk.surface.below.ring","stalk.color.above.ring",
                                                        "stalk.color.below.ring","veil.type","veil.color",
                                                        "ring.number","ring.type","spore.print.color",
                                                        "population","habitat"))
