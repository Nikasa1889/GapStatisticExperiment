K = 1;
wk = 1;
sk = 1;
B =10;
wp = 0;
GAPk = 1;
Hk = 1;
N = dim(TestData)[1];
numCol = dim(TestData)[2];
if (colnames(TestData)[numCol ] == "cluster" )
{
	numCol = numCol - 1;
}

dim(TestData)[2]
for (K in 1:10)
{
  #with each K, apply K-meansa
  TestData <- as.data.frame(TestData);
  results <- kmeans(TestData, K);
  TestData$cluster = results$cluster;
  numCol = dim(TestData)[2];
  wkVal = 0;
  Kindex = K;
  for (t in 1:Kindex)
  {
    C1 = TestData [TestData$cluster == t ,1:(numCol-1)];

    distTmp = 0.0;
    nk = dim(C1)[1];
    for (i in 1:nk )
    {
      for (j in 1:nk)
        if (i != j)
        {
	    tmp = 0;
	    for (k in 1:(numCol-1))
		{
			tmp = tmp + (C1[i,k] - C1[j,k])^2;
		}
          distTmp = distTmp +  sqrt(tmp);
        }
    }
    wkVal  = distTmp  / (2*nk);
    
  }
  
  wk[K] = wkVal;
  wp = wp + wkVal;
  GAPk [K] = log(wp) - log(wk[K]);
  
}

NewGap = 1
RAk = 1
# calculatting HK
for (K in 1:10)
{
  Hk[K] = ( (wk[K] / wk[K+1]) -1 ) / (N-K-1);
  RAk[K] = GAPk[K] / Hk[K];
  NewGap[K] = ( (wk[K] / GAPk[K]) ) / (N-K-1);
  
  
}

maxT = 0;
ret = 0;
barchart = 0;
for (i in 1:9)
{
	if (NewGap[i] != Inf)
	
	{
		dist = abs(NewGap[i] -  NewGap[i+1]);
		barchart[i] =  dist ;
		if (dist > maxT)
		{
			maxT = dist;
			ret = i;
			
		}
	}
}
barplot(barchart, main="New Gap Bar Chart", 
  	xlab="Values of K")

print(" Number of cluster is ");
print(ret );
