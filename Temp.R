library(readr)
balanceDf = read_csv("~/GapExperiment/Data/balance_scale.csv", 
                     col_types = cols(Class = col_character(), `Left-Distance` = col_double(), 
                                      `Left-Weight` = col_double(), `Right-Distance` = col_double(), 
                                      `Right-Weight` = col_double()))
names(balanceDf)[ncol(balanceDf)]  = "class"
balanceDf$class = factor(balanceDf$class)

banknoteDf = read_csv("~/GapExperiment/Data/banknote_authentication.csv", 
                      col_types = cols(class = col_character()))
names(banknoteDf)[ncol(banknoteDf)]  = "class"
banknoteDf$class = factor(banknoteDf$class)

bloodDf = read_csv("~/GapExperiment/Data/blood.csv", 
                   col_types = cols(
                     `Frequency (times)` = col_double(), 
                     `Monetary (c.c. blood)` = col_double(), 
                     `Recency (months)` = col_double(), 
                     `Time (months)` = col_double(), 
                     `whether he/she donated blood in March 2007`=col_character()))
names(bloodDf)[ncol(bloodDf)]  = "class"
bloodDf$class = factor(bloodDf$class)

cancerDf = read_csv("~/GapExperiment/Data/cancer.csv")
names(cancerDf)[ncol(cancerDf)]  = "class"
cancerDf$class = factor(cancerDf$class)

densityDf = read_csv("~/GapExperiment/Data/density.csv")
names(densityDf)[ncol(densityDf)]  = "class"
densityDf$class = factor(densityDf$class)

glassDf = read_csv("~/GapExperiment/Data/glass.csv")
names(glassDf)[ncol(glassDf)]  = "class"
glassDf$class = factor(glassDf$class)

irisDf <- read_csv("~/GapExperiment/Data/Iris.csv",
                   col_types = cols(
                     sepal.length = col_double(),
                     sepal.width = col_double(),
                     petal.length = col_double(),
                     petal.width = col_double(),
                     class = col_character()
                   ))
names(irisDf)[ncol(irisDf)]  = "class"
irisDf$class = factor(irisDf$class)

liverDf = read_csv("~/GapExperiment/Data/liver-disorders.csv")
names(liverDf)[ncol(liverDf)]  = "class"
liverDf$class = factor(liverDf$class)

mouseDf = read_csv("~/GapExperiment/Data/mouse.csv")
names(mouseDf)[ncol(mouseDf)]  = "class"
mouseDf$class = factor(mouseDf$class)