
#using TestData

TP = 0;
TN = 0;
FP = 0;
FN = 0;
n = dim(TestData)[1];
classIndex = 10;
clusterIndex = 11;


for (i in 1:n)
{
	for (j in 1:n)
	{
		if (TestData[i,classIndex ] == TestData[j,classIndex ])
		{
			if (TestData[i,clusterIndex ] == TestData[j,clusterIndex ])
			{
				TP=TP+1;
			}
			else
			{
				FP=FP+1;
			}

		}
		else
		{
			if (TestData[i,clusterIndex ] == TestData[j,clusterIndex ])
			{
				FN = FN + 1;
			}
			else
			{
				TN = TN+1;
			}

		}
	}

}

TP = TP/2;
TN = TN/2;
FP = FP/2;
FN = FN/2;
RI = (TP+TN)/(TP+FP+FN+TN);
print(" Rank Index is : ");
print(RI);
