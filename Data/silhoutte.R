k.max <- 10
RawData <- read.csv("pen.csv")
TestData <- scale(RawData[, 1:(dim(RawData)[2]-1)])
sil <- rep(0, k.max)
# Compute the average silhouette width for 
# k = 2 to k = 15
for(i in 2:k.max){
  km.res <- kmeans(TestData, centers = i, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(TestData))
  sil[i] <- mean(ss[, 3])
}
# Plot the  average silhouette width
plot(1:k.max, sil, type = "b", pch = 19, 
     frame = FALSE, xlab = "Number of clusters k")
