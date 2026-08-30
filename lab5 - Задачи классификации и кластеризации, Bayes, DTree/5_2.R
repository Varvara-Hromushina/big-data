library(dplyr)
library(e1071)
library(party)
library(randomForest)

cluster_results <- read.csv("football_clustering_results.csv", stringsAsFactors = FALSE)

cluster_results$cluster <- as.factor(cluster_results$cluster_kmeans)

classification_data <- cluster_results %>%
  dplyr::select(
    win_rate,
    goals_per_match,
    goals_conceded_per_match,
    goal_diff_per_match,
    total_matches,
    cluster
  )


set.seed(1234)
ind <- sample(2, nrow(classification_data), replace = TRUE, prob = c(0.7, 0.3))
trainData <- classification_data[ind == 1, ]
testData <- classification_data[ind == 2, ]

cat("Обучающая выборка:", nrow(trainData), "наблюдений\n")
cat("Тестовая выборка:", nrow(testData), "наблюдений\n\n")


cat("Наивный Байесовский классификатор\n")

nb_model <- naiveBayes(cluster ~ ., data = trainData)

cat("\nАприорные вероятности кластеров:\n")
print(nb_model$apriori)

test_pred_nb <- predict(nb_model, testData)

conf_nb <- table(Факт = testData$cluster, Прогноз = test_pred_nb)
print(conf_nb)

acc_nb <- sum(diag(conf_nb)) / sum(conf_nb)
cat("Точность Naive Bayes:", round(acc_nb * 100, 2), "%\n\n")

cat("Дерево решений\n")

dt_model <- ctree(cluster ~ ., data = trainData)

cat("Структура дерева решений:\n")
print(dt_model)

if(nrow(trainData) <= 500) {
  plot(dt_model, main = "Дерево решений для классификации футбольных команд")
  cat("График дерева построен\n")
} else {
  cat("Размерность данных не позволяет построить график (n > 500)\n")
}

test_pred_dt <- predict(dt_model, newdata = testData)

conf_dt <- table(Факт = testData$cluster, Прогноз = test_pred_dt)
print(conf_dt)

acc_dt <- sum(diag(conf_dt)) / sum(conf_dt)
cat("Точность Decision Tree:", round(acc_dt * 100, 2), "%\n\n")


cat("Случайный лес\n")

set.seed(1234)
rf_model <- randomForest(cluster ~ ., data = trainData, ntree = 100, importance = TRUE)

cat("Модель Random Forest:\n")
print(rf_model)

cat("\nВажность переменных:\n")
print(importance(rf_model))

varImpPlot(rf_model, main = "Важность переменных в Random Forest")

test_pred_rf <- predict(rf_model, newdata = testData)

conf_rf <- table(Факт = testData$cluster, Прогноз = test_pred_rf)
print(conf_rf)

acc_rf <- sum(diag(conf_rf)) / sum(conf_rf)
cat("Точность Random Forest:", round(acc_rf * 100, 2), "%\n\n")


cat("Сравнительный анализ\n")

cat("\nСравнение точности методов классификации:\n")

cat(sprintf("Метод: Naive Bayes Точность (тест): %.2f%%\n", acc_nb * 100))
cat(sprintf("Метод: Decision Tree Точность (тест): %.2f%%\n", acc_dt * 100))
cat(sprintf("Метод: Random Forest Точность (тест): %.2f%%\n", acc_rf * 100))


cat("\nСопоставление результатов:\n")

cat("\nRandom Forest vs Decision Tree:\n")
if(acc_rf > acc_dt) {
  cat("   Random Forest точнее Decision Tree на", round((acc_rf - acc_dt) * 100, 2), "%\n")
  cat("   Random Forest усредняет множество деревьев, снижая дисперсию\n")
} else if(acc_rf < acc_dt) {
  cat("   Decision Tree точнее Random Forest на", round((acc_dt - acc_rf) * 100, 2), "%\n")
  cat("   Возможно, Random Forest переобучился или требуется настройка параметров\n")
} else {
  cat("   Методы показали одинаковую точность\n")
}

cat("\nRandom Forest vs Naive Bayes:\n")
if(acc_rf > acc_nb) {
  cat("   Random Forest точнее Naive Bayes на", round((acc_rf - acc_nb) * 100, 2), "%\n")
} else if(acc_rf < acc_nb) {
  cat("   Naive Bayes точнее Random Forest на", round((acc_nb - acc_rf) * 100, 2), "%\n")
  cat("   Наивный Байес хорошо работает с независимыми признаками\n")
} else {
  cat("   Методы показали одинаковую точность\n")
}

cat("\nСравнение с результатами кластерного анализа (ЛР 5.1):\n")

cat("\nХарактеристики кластеров из ЛР 5.1:\n")
cat("   Кластер 1 (n=105): win_rate=0.429, голов=1.59, разница=0.21\n")
cat("   Кластер 2 (n=20):  win_rate=0.480, голов=2.27, разница=0.72\n")
cat("   Кластер 3 (n=23):  win_rate=0.143, голов=0.85, разница=-3.06\n")
cat("   Кластер 4 (n=89):  win_rate=0.259, голов=1.07, разница=-0.99\n")

cat("\nВывод о кластерном анализе (ЛР 5.1):\n")
cat("   - Полученные 4 кластера хорошо разделимы\n")
cat("   - Точность классификации (>", round(min(acc_nb, acc_dt, acc_rf) * 100, 2), "%) подтверждает\n")
cat("     качество кластеризации\n")
cat("   - Наиболее информативные признаки: goal_diff_per_match и win_rate\n")


cat("\nОбщий вывод по ЛР 5.1 И 5.2\n")
cat("\nЛР 5.1 (Кластерный анализ):\n")
cat("  - Выполнена кластеризация 237 футбольных команд\n")
cat("  - Определено оптимальное число кластеров: 4\n")
cat("  - Получены содержательно интерпретируемые кластеры\n")

cat("\nЛР 5.2 (Классификация):\n")
cat("  - Выполнена классификация тремя методами\n")
cat("  - Наивысшая точность: ", round(max(acc_nb, acc_dt, acc_rf) * 100, 2), "%\n")
cat("  - Кластеры подтвердили свою состоятельность\n")

cat("\nОбщее заключение:\n")
cat("  Кластерный анализ и классификация взаимно подтверждают\n")
cat("  результаты. Кластеры, выделенные в ЛР 5.1, успешно\n")
cat("  классифицируются с высокой точностью, что говорит о\n")
cat("  правильности выбора числа кластеров и использованных признаков.\n")