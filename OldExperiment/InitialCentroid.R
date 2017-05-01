K = 3;
N = dim(TestData)[1]; 
M = dim(TestData)[2];
DA <- matrix(, nrow = N, ncol = N); # distance array
DX <- matrix(0,nrow = N, ncol = N); # checked matrix
for (i in 1:N)
{
  for (j in 1:N)
  {
    val1 = (TestData[i,1] - TestData[j,1]) * (TestData[i,1] - TestData[j,1]);
    val2 = (TestData[i,2] - TestData[j,2]) * (TestData[i,2] - TestData[j,2]);
    valTmp = sqrt(val1 + val2);
    DA[i,j] = valTmp;
  }
  DA[i,i] = 1000;
}

m=1;
minT = 10000;
IminT = -1;
JminT = -1;
Group = 1;
for (i in 1:N)
{
  Group[i] = 0;
}
while (m<=K)
{
  minT = 10000;
  for (i in 1:N)
  {
    for (j in i:N)
    {
      if (DA[i,j] < minT && DX[i,j] == 0)
      {
        minT = DA[i,j];
        IminT = i;
        JminT = j;
      }
    }
  }
  DX[IminT ,JminT ] = DX[JminT ,IminT ] = m;
  Group[IminT ] = m;
  Group[JminT ] = m;
  saveI = 0;
  minT  = 1000;
  count = 0;
  while (1)
  {
    for (i in 1:N)
    {
      if (Group[i] == 0)
      {
        if (DA[i,IminT] < minT || DA[i,JminT] < minT)
        {
          minT = DA[i,IminT];
          if (minT  > DA[i,JminT])
            minT  = DA[i,JminT];
          saveI = i;
        }
        
      }
    }
    Group[saveI] = m;
    count =  count + 1;
    if (count >= (N / K))
      break;
  }
  
  m = m + 1;
}

InitPoint <- matrix(0,nrow = K, ncol = 2); # initial point
k = 0;
for (i in 1:K)
{
  xA = 0;
  yA = 0;
  k = 1;
  for (j in 1:N)
  {
    if (Group[j] == i)
    {
      xA[k] =  TestData[j,1];
      yA[k] =  TestData[j,2];
      k = k+1;
      
    }
  }
  InitPoint [i,1] = mean(xA);
  InitPoint [i,2] = mean(yA);
  
}	
