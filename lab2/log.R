library(ggplot2)
library(dplyr)
library(fastDummies)

url <- "https://raw.githubusercontent.com/Evantastic/analisis12020/master/agaricus-lepiota.data"
set.seed(88)
personal.theme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15), hjust = 0.5), 
                        legend.title = element_text(colour = "steelblue",  face = "bold.italic", family = "Helvetica"), 
                        legend.text = element_text(face = "italic", colour="steelblue4",family = "Helvetica"), 
                        axis.title = element_text(family = "Helvetica", size = (10), colour = "steelblue4"),
                        axis.text = element_text(family = "Courier", colour = "cornflowerblue", size = (10)))
mushrooms <- read.table(url, header = TRUE, sep = ",")
mushrooms$veil.type <- NULL
mushrooms <- mushrooms[which(mushrooms$stalk.root != '?'),]
mushrooms1 <- dummy_cols(mushrooms,  select_columns = c("class"))%>%select(-c("class"))
model1 <- glm(class_p~cap.shape+cap.surface+cap.color+bruises+odor+
                gill.attachment+gill.spacing+gill.size+gill.color+stalk.shape+
                stalk.root+stalk.surface.above.ring+stalk.surface.below.ring+
                stalk.color.above.ring+stalk.color.below.ring+veil.color+
                ring.number+ring.type+spore.print.color+population+habitat,
              data = mushrooms1, family = binomial, control = list(maxit = 50))
