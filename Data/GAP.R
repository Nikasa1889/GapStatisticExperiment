K = 1;
wk = 1;
sk = 1;
B =10;
wp = 0;
GAPk = 1;
Hk = 1;
N = dim(TestData)[1];
for (K in 1:6)
{
  #with each K, apply K-meansa
  TestData <- as.data.frame(TestData);
  results <- kmeans(TestData, K);
  TestData$cluster = results$cluster;
 
  wkVal = 0;
  Kindex = K;
  for (t in 1:Kindex)
  {
    C1 = TestData [TestData$cluster == t ,1:2];

    distTmp = 0.0;
    nk = dim(C1)[1];
    for (i in 1:nk )
    {
      for (j in 1:nk)
        if (i != j)
        {
          tmp = sqrt( (C1[i,1] - C1[j,1])^2 + (C1[i,2] - C1[j,2])^2 );
          distTmp = distTmp +  tmp;
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
for (K in 2:10)
{
  Hk[K] = ( (wk[K] / wk[K+1]) -1 ) / (N-K-1);
  RAk[K] = GAPk[K] / Hk[K];
  NewGap[K] = ( (wk[K] / GAPk[K]) ) / (N-K);
  
  
}

plot(NewGap,type="o", col="blue")