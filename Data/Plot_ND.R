RawData <- read.csv("cancer.csv")
TestData <- RawData[ ,1:(dim(RawData)[2]-1)]
clus <- kmeans(TestData, center = 3)
plotcluster(TestData, clus$cluster, main = "Cancer Dataset")