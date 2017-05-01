RawData <- read.csv("pen.csv")
TestData <- scale(RawData[, 1:(dim(RawData)[2]-1)])
set.seed(123)
k.max <- 10
wss <- sapply(1:k.max, function(k){kmeans(TestData, k, nstart=10 )$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
