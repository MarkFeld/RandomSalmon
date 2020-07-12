  ## подключаем библиотеку экспорта данных из таблицы Excel
library(readxl)
  ## экспортируем исходные данные в дата-фрейм
allData <- read_excel("~/R/randomForest/allData.xlsx") 
  ## подключаем библиотеку dplyr, облегчает манипуляции с данными 
library(dplyr)
  ## создаем дата-фреймы для записи результатов
DataVecOOB <- data.frame(iteration = 1:37)
DataPredictors <- data.frame(n = 1:5)
     
    ## НАЧАЛО ОСНОВНОГО ЦИКЛА
for(i in 1:1000) {
    ## преобразуем все символьные переменные в фактор, 
	## и объявляем новый дата-фрейм,равный исходному
  newData <- allData %>% mutate_if(is.character, as.factor)## так более лучше
    ## обозначаем порядок фактора (low < average < high)
  newData$strata <- ordered(newData$strata,levels = c("low", "average", "high")) 
    ## создаем пустой вектор для записи промежуточных результатов 
  vecOOB <- c()         ## для ошибки OOB каждого леса
    
	## Во фрейме данных 69 объясняющих переменных. 
    ## Чтобы уменьшить их количество будем выращивать лес,
    ## исключать самые незначимые предикторы (5%-квантиль по важности), 
	## и обучать снова.
    ## Цикл закончится, когда объясняющих переменных останется две

      ## НАЧАЛО ВЛОЖЕННОГО ЦИКЛА
    while (ncol(newData)-1 > 1) {
  		
		  ## формула для параметра mtry - квадратный корень из количества
		  ## объясняющих переменных (округленный в большую сторону)
		X <- round(sqrt(ncol(newData)-1)) ## -1 вычесть зависимую переменную
		if (X < 2) X = 2 ## mtry не менее двух признаков	  
		  ## формула для параметра nodesize - натур логарифм из количества 
		  ## объясняющих переменных минус единица (округленный в большую сторону)
		Y <- round(log(ncol(newData)-1)-1) ## -1 вычесть зависимую переменную
		if (Y < 1) Y = 1	  
		  ## подключаем библиотеку случайного леса
		library(randomForest)
	  	  ## зерно ГСЛ изменяется от 1 до 1000
		set.seed(i)	  
		  ## создаем объект случайного леса
		newData.rf <- randomForest(strata~ .,  xtest=NULL, ytest=NULL,
								   data=newData, mtry=X, nodesize=Y,
								   ntree=150, 
								   replace=TRUE,
								   importance=TRUE, localImp=TRUE, 
								   proximity=TRUE)
	  	  ## именуем данные по важности признаков,   
		imp <- importance(newData.rf, type = 2) ## type = 2 - данные по 'mean decrease accuracy'
		  ## устанавливаем критерий для фильтрации,
		  ## находим 5%-квантиль, по которому будем отсекать незначимые предикторы 
		quantileProc <- quantile(imp, 0.05)
		  ## представляем imp как датафрейм 
		impDataFrame <- as.data.frame(imp,row.names = NULL, optional = FALSE,
									make.names = TRUE,
									stringsAsFactors = default.stringsAsFactors())
 		  ## добавляем в него вектор индексов 
		impDataFrame <- data.frame(MeanDecreaseGini = impDataFrame, 
								   index = rownames(impDataFrame))
		  ## сортируем по уменьшению важности
		impDataFrame <- impDataFrame[order(impDataFrame$MeanDecreaseGini, decreasing = TRUE),]
		    ## если в дата-фрейме остается 5 предикторов, запоминаем их имена
		  if ((ncol(newData)-1) == 5) {
		    Predictors <- impDataFrame$index
		  }
		  ## фильтруем датафрейм важности по критерию quantileProc
		filterImp <- filter(impDataFrame, impDataFrame$MeanDecreaseGini <= quantileProc)     
		  ## представляем filterImp как вектор 
		filterImpVector <- as.vector(filterImp$index)
		  ## находим индексы совпадений
		I <-match(filterImpVector, names(newData), nomatch = 0, incomparables = NULL)  
		  ## записываем значение ошибки OOB в вектор
		vecOOB <- c(vecOOB, newData.rf$err.rate[newData.rf$ntree])
		  ## удаляем признаки с индексом(-ами) I
		newData <- newData[ , -c(I)]
	  
	  ## КОНЕЦ ВЛОЖЕННОГО ЦИКЛА
  }
    ## добавляем промежуточные результаты в датафреймы результатов 
  DataVecOOB <- cbind(DataVecOOB, vecOOB)
  DataPredictors <- cbind(DataPredictors, Predictors)

  ## КОНЕЦ ОСНОВНОГО ЦИКЛА
}
  ## удаляем инициализирующие векторы в дата-фреймах результатов
DataVecOOB <- DataVecOOB[, -1]
DataPredictors<- DataPredictors[, -1]
  ## Экспорт в эксел
library(xlsx)
write.xlsx(DataVecOOB, file="OutputOOB.xlsx", 
           sheetName="Sample_Sheet", row.names=F, showNA=F)
write.xlsx(DataPredictors, file="OutputPredictors.xlsx", 
           sheetName="Sample_Sheet", row.names=F, showNA=F) 