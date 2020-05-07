# Consideraciones
At the start of the script there is a variable called `filename`, this variable must be equal to the path of the `expanded` file contained in the zip file. This file is used instead of the `.data` file to obtain better plots. If the `agaricus-lepiota.data` file needs to be used, edit accordingly:
``` R
filename                       <- "agaricus-lepiota.data" # path to the file
edible.class                   <- "e"
poisonous.class                <- "p"
odor.almond                    <- "a"
odor.anise                     <- "l"
odor.none                      <- "n"
```
