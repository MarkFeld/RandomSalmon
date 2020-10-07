  ## алгоритм Boruta - проверка признаков на релевантность
  ## создаём фрейм с выбранными пятью признаками и зависимой переменной
resultData <- data.frame(PDO12_13 = allData$PDO12_13,
                         WP7 = allData$WP7,
                         WP17_21 = allData$WP17_21,
                         AO5 = allData$AO5,
                         ln_S = allData$ln_S, 
                         strata = allData$strata)

  ## подключаем библиотеки 
library(Boruta)
library(ranger)
  ## устанавливаем зерно ГСЛ для воспроизводимости результатов
set.seed(1)
  ## создаем дата-фрейм с теневые признаки
resultData.extended <- data.frame(resultData, apply(resultData[,-6],2,sample))
  ## даем имена теневым признакам
names(resultData.extended)[7:11] <- paste("Nonsense",1:5,sep="")

  ## создаем объект Boruta
Boruta.resultData.extended <- Boruta(strata~.,data=resultData.extended,
                                     mcAdj = TRUE,
                                     doTrace=2, 
                                     ntree = 150, 
                                     maxRuns = 300)
  ## результат
Boruta.resultData.extended
Boruta performed 83 iterations in 14.40983 secs.
 5 attributes confirmed important: AO5, ln_S, PDO12_13, WP17_21, WP7;
 5 attributes confirmed unimportant: Nonsense1, Nonsense2, Nonsense3, Nonsense4,
Nonsense5;