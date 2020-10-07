  ## создаем вектор для записи результатов ошибки ООВ
vecOOB <- c()  
  ## создаем дата-фрейм для записи результатов прогноза 
DataPred <- data.frame(low = 1, average = 2, high = 3)

  ## НАЧАЛО ЦИКЛА
for(i in 1:1000) {
      ## подключаем библиотеку случайного леса
    library(randomForest)
      ## устанавливаем  зерно ГСЛ
    set.seed(i)  
      ## создаем объект случайного леса
    resultData.rf <- randomForest(strata~ .,  xtest=NULL, ytest=NULL,
                               data=resultData, mtry=2, nodesize=1,
                               ntree=150, 
                               replace=TRUE, importance=TRUE, localImp=TRUE, 
                               proximity=TRUE)
      ## записываем значение ошибки OOB в вектор
    vecOOB <- c(vecOOB, resultData.rf$err.rate[resultData.rf$ntree])
      ## вносим прогнозные данные   
    predictData2018 <- data.frame(PDO12_13 = -0.27,
                                  WP7 = -0.81,
                                  WP17_21 = -0.83,
                                  AO5 = 1.18,
                                  ln_S = 3.31)
	  ## вычисляем прогноз уровня на 2020 г.							  
    Pred <- predict(resultData.rf, predictData2018, type = "prob")
      ## записываем реультат прогноза в дата-фрейм
	DataPred <- rbind(DataPred, Pred)
  ## КОНЕЦ ЦИКЛА
}
  ## удаляем инициализирующую строку в дата-фрейме прогноза
DataPred <- DataPred[-1,]
  ## экспорт данных в MS Excel
library(xlsx)
write.xlsx(DataPred, file="DataPred.xlsx", 
           sheetName="Sample_Sheet", row.names=F, showNA=F)
write.xlsx(vecOOB, file="vecOOB.xlsx", 
           sheetName="Sample_Sheet", row.names=F, showNA=F)