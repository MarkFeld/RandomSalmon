  ## ���������� ���������� �������� ������ �� ������� Excel
library(readxl)
  ## ������������ �������� ������ � ����-�����
allData <- read_excel("~/R/randomForest/allData.xlsx") 
  ## ���������� ���������� dplyr, ��������� ����������� � ������� 
library(dplyr)
  ## ������� ����-������ ��� ������ �����������
DataVecOOB <- data.frame(iteration = 1:37)
DataPredictors <- data.frame(n = 1:5)
     
    ## ������ ��������� �����
for(i in 1:1000) {
    ## ����������� ��� ���������� ���������� � ������, 
	## � ��������� ����� ����-�����,������ ���������
  newData <- allData %>% mutate_if(is.character, as.factor)## ��� ����� �����
    ## ���������� ������� ������� (low < average < high)
  newData$strata <- ordered(newData$strata,levels = c("low", "average", "high")) 
    ## ������� ������ ������ ��� ������ ������������� ����������� 
  vecOOB <- c()         ## ��� ������ OOB ������� ����
    
	## �� ������ ������ 69 ����������� ����������. 
    ## ����� ��������� �� ���������� ����� ���������� ���,
    ## ��������� ����� ���������� ���������� (5%-�������� �� ��������), 
	## � ������� �����.
    ## ���� ����������, ����� ����������� ���������� ��������� ���

      ## ������ ���������� �����
    while (ncol(newData)-1 > 1) {
  		
		  ## ������� ��� ��������� mtry - ���������� ������ �� ����������
		  ## ����������� ���������� (����������� � ������� �������)
		X <- round(sqrt(ncol(newData)-1)) ## -1 ������� ��������� ����������
		if (X < 2) X = 2 ## mtry �� ����� ���� ���������	  
		  ## ������� ��� ��������� nodesize - ����� �������� �� ���������� 
		  ## ����������� ���������� ����� ������� (����������� � ������� �������)
		Y <- round(log(ncol(newData)-1)-1) ## -1 ������� ��������� ����������
		if (Y < 1) Y = 1	  
		  ## ���������� ���������� ���������� ����
		library(randomForest)
	  	  ## ����� ��� ���������� �� 1 �� 1000
		set.seed(i)	  
		  ## ������� ������ ���������� ����
		newData.rf <- randomForest(strata~ .,  xtest=NULL, ytest=NULL,
								   data=newData, mtry=X, nodesize=Y,
								   ntree=150, 
								   replace=TRUE,
								   importance=TRUE, localImp=TRUE, 
								   proximity=TRUE)
	  	  ## ������� ������ �� �������� ���������,   
		imp <- importance(newData.rf, type = 2) ## type = 2 - ������ �� 'mean decrease accuracy'
		  ## ������������� �������� ��� ����������,
		  ## ������� 5%-��������, �� �������� ����� �������� ���������� ���������� 
		quantileProc <- quantile(imp, 0.05)
		  ## ������������ imp ��� ��������� 
		impDataFrame <- as.data.frame(imp,row.names = NULL, optional = FALSE,
									make.names = TRUE,
									stringsAsFactors = default.stringsAsFactors())
 		  ## ��������� � ���� ������ �������� 
		impDataFrame <- data.frame(MeanDecreaseGini = impDataFrame, 
								   index = rownames(impDataFrame))
		  ## ��������� �� ���������� ��������
		impDataFrame <- impDataFrame[order(impDataFrame$MeanDecreaseGini, decreasing = TRUE),]
		    ## ���� � ����-������ �������� 5 �����������, ���������� �� �����
		  if ((ncol(newData)-1) == 5) {
		    Predictors <- impDataFrame$index
		  }
		  ## ��������� ��������� �������� �� �������� quantileProc
		filterImp <- filter(impDataFrame, impDataFrame$MeanDecreaseGini <= quantileProc)     
		  ## ������������ filterImp ��� ������ 
		filterImpVector <- as.vector(filterImp$index)
		  ## ������� ������� ����������
		I <-match(filterImpVector, names(newData), nomatch = 0, incomparables = NULL)  
		  ## ���������� �������� ������ OOB � ������
		vecOOB <- c(vecOOB, newData.rf$err.rate[newData.rf$ntree])
		  ## ������� �������� � ��������(-���) I
		newData <- newData[ , -c(I)]
	  
	  ## ����� ���������� �����
  }
    ## ��������� ������������� ���������� � ���������� ����������� 
  DataVecOOB <- cbind(DataVecOOB, vecOOB)
  DataPredictors <- cbind(DataPredictors, Predictors)

  ## ����� ��������� �����
}
  ## ������� ���������������� ������� � ����-������� �����������
DataVecOOB <- DataVecOOB[, -1]
DataPredictors<- DataPredictors[, -1]
  ## ������� � �����
library(xlsx)
write.xlsx(DataVecOOB, file="OutputOOB.xlsx", 
           sheetName="Sample_Sheet", row.names=F, showNA=F)
write.xlsx(DataPredictors, file="OutputPredictors.xlsx", 
           sheetName="Sample_Sheet", row.names=F, showNA=F) 