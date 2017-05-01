RawData <- read.csv("density.csv")
responseY <- RawData[ ,dim(RawData)[2]]
predictorX <- RawData[ , 1:(dim(RawData)[2]-1)]
pca <- princomp(predictorX, cor = T)
pc.comp <- pca$scores
pc.comp1 <- -1*pc.comp[,1]
pc.comp2 <- -1*pc.comp[,2]
TestData <- cbind(pc.comp1, pc.comp2)