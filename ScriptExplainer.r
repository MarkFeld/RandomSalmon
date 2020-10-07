  ## создаём дата-фрейм с выбранными пятью признаками и зависимой переменной
resultData <- data.frame(PDO12_13 = allData$PDO12_13,
                         WP7 = allData$WP7,
                         WP17_21 = allData$WP17_21,
                         AO5 = allData$AO5,
                         ln_S = allData$ln_S, 
                         strata = allData$strata)
  ## объявляем зависимую переменную фактором
resultData$strata <- factor(resultData$strata) 
  ## обозначаем порядок фактора (low < average < high)
resultData$strata <- ordered(resultData$strata,
                             levels = c("low", "average", "high"))
 ## подключаем библиотеку
library(randomForest)
  ## устанавливаем зерно ГСЛ для воспроизводимости результатов
set.seed(853)
  ## создаём объект случайного леса
resultData.rf <- randomForest(strata~ .,  xtest=NULL, ytest=NULL,
                              data=resultData, mtry=2, nodesize=1,
                              ntree=150, 
                              replace=TRUE, importance=TRUE, localImp=TRUE, 
                              proximity=TRUE)
  ## вывод графика ошибки ООВ
plot(resultData.rf)
							  
  ## подключаем библиотеку
library(randomForestExplainer)
  ## проводим основную функцию пакета, результат которой выводится в файл HTML
explain_forest(resultData.rf, interactions = TRUE)
  ## код графиков основных взаимодействий
plot_predict_interaction(resultData.rf, resultData, "WP17_21", "PDO12_13", grid = 100,
                         main = paste0("Взаимодействие предикторов ",
                         paste0("WP17_21", paste0(" и ", "PDO12_13"))), time = NULL)
plot_predict_interaction(resultData.rf, resultData, "WP17_21", "ln_S", grid = 100,
                         main = paste0("Взаимодействие предикторов ",
                         paste0("WP17_21", paste0(" и ", "ln_S"))), time = NULL)
plot_predict_interaction(resultData.rf, resultData, "WP7", "WP17_21", grid = 100,
                         main = paste0("Взаимодействие предикторов ",
                         paste0("WP7", paste0(" и ", "WP17_21"))), time = NULL)
plot_predict_interaction(resultData.rf, resultData, "PDO12_13", "AO5", grid = 100,
                         main = paste0("Взаимодействие предикторов ",
                         paste0("PDO12_13", paste0(" и ", "AO5"))), time = NULL)
  ## создаем дата-фрейм характеристик важности
importance_frame <- measure_importance(resultData.rf)
  ## по выбранным характеристикам важности (с минимальными корреляциями)
  ## выводим многоплановый график по трем характеристикам важности
plot_multi_way_importance(importance_frame, x_measure = "accuracy_decrease", 
                          y_measure = "gini_decrease", 
                          size_measure = "no_of_nodes", no_of_labels = 5)