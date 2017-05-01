RawData <- read.csv("glass.csv")
TestData <- RawData[ ,1:(dim(RawData)[2]-1)]
results <- kmeans(TestData,6)
RawData$cluster = results$cluster
TestData <- RawData