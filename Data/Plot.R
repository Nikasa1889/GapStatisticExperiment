TestData <- TestData[,1:2];
results <- kmeans(TestData, InitPoint);
clusterArr = results$cluster;
ListColor <- c("blue", "red", "black" , "brown" , "green", "darkviolet");

plot(TestData,col="white",xlab="X", ylab="Y");

NumRow = dim(TestData)[1];
for (i in 1:NumRow)
{
  
 points(TestData[i,1:2],col=ListColor[clusterArr[i] ],pch=3,cex=.6)
  
}
