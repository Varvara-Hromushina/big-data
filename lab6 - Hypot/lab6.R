ath <- read.csv("lab6/athlete_events.csv", 
                stringsAsFactors = FALSE,
                na.strings = c("", "NA", "null"),
                fileEncoding = "UTF-8")

str(ath[, c("Age", "Height", "Weight", "Year")])

ath$Age <- as.numeric(ath$Age)
ath$Height <- as.numeric(ath$Height)
ath$Weight <- as.numeric(ath$Weight)
ath$Year <- as.numeric(ath$Year)

summary(ath$Weight)

swim <- subset(ath, Sport == "Swimming")

cat("1. Дескриптивный анализ данных (EDA)\n")
cat("Количество наблюдений в плавании:", nrow(swim), "\n\n")

swim_weight <- na.omit(swim$Weight)

cat("Статистики веса пловцов (всех):\n")
print(summary(swim_weight))

cat("\nСтатистики веса по полу:\n")
cat("Мужчины:\n")
print(summary(na.omit(swim$Weight[swim$Sex == "M"])))
cat("Женщины:\n")
print(summary(na.omit(swim$Weight[swim$Sex == "F"])))

hist(swim_weight, 
     main = "Распределение веса пловцов", 
     xlab = "Вес (кг)", 
     col = "lightblue",
     border = "darkblue",
     breaks = 30)

abline(v = mean(swim_weight), col = "red", lwd = 2, lty = 2)
abline(v = median(swim_weight), col = "blue", lwd = 2, lty = 2)
legend("topright", legend = c("Среднее", "Медиана"), 
       col = c("red", "blue"), lty = 2, lwd = 2)

boxplot(Weight ~ Sex, data = swim, 
        main = "Вес пловцов и пловчих", 
        xlab = "Пол", 
        ylab = "Вес (кг)",
        col = c("lightpink", "lightblue"))

cat("\n2. Проверка на нормальность и дисперсию\n")

male_weight <- na.omit(swim$Weight[swim$Sex == "M"])
female_weight <- na.omit(swim$Weight[swim$Sex == "F"])

cat("Размер выборки (мужчины):", length(male_weight), "\n")
cat("Размер выборки (женщины):", length(female_weight), "\n\n")

shapiro_male <- shapiro.test(male_weight)
cat("Тест Шапиро-Уилка (мужчины):\n")
print(shapiro_male)

shapiro_female <- shapiro.test(female_weight)
cat("\nТест Шапиро-Уилка (женщины):\n")
print(shapiro_female)

alpha <- 0.05
if(shapiro_male$p.value < alpha) {
  cat("Распределение веса мужчин-пловцов НЕ является нормальным (p < 0.05)\n")
} else {
  cat("Нет оснований отвергнуть нормальность веса мужчин-пловцов (p ≥ 0.05)\n")
}

if(shapiro_female$p.value < alpha) {
  cat("Распределение веса женщин-пловчих НЕ является нормальным (p < 0.05)\n")
} else {
  cat("Нет оснований отвергнуть нормальность веса женщин-пловчих (p ≥ 0.05)\n")
}

par(mfrow = c(1, 2))

qqnorm(male_weight, main = "QQ-plot: Вес мужчин-пловцов")
qqline(male_weight, col = "red", lwd = 2)

qqnorm(female_weight, main = "QQ-plot: Вес женщин-пловчих")
qqline(female_weight, col = "red", lwd = 2)

par(mfrow = c(1, 1))

bartlett_test <- bartlett.test(Weight ~ Sex, data = swim)
cat("\nТест Бартлетта на равенство дисперсий:\n")
print(bartlett_test)

if(bartlett_test$p.value < alpha) {
  cat("Дисперсии различаются (p < 0.05) - используем тест Уэлча\n")
} else {
  cat("Дисперсии равны (p ≥ 0.05) - можно использовать классический t-тест\n")
}


cat("\n3. Одновыборочный тест\n")

mu0 <- 75
cat("H₀: Средний вес пловцов-мужчин =", mu0, "кг\n")
cat("H₁: Средний вес пловцов-мужчин ≠", mu0, "кг\n\n")

t_test_one <- t.test(male_weight, mu = mu0)
cat("Результаты t-теста:\n")
print(t_test_one)

wilcox_test_one <- wilcox.test(male_weight, mu = mu0)
cat("\nРезультаты теста Вилкоксона (ранговый):\n")
print(wilcox_test_one)

if(t_test_one$p.value < alpha) {
  cat("\n Отвергаем H₀: Средний вес значимо отличается от", mu0, "кг\n")
  cat("  Фактический средний вес:", round(t_test_one$estimate, 2), "кг\n")
  cat("  95% доверительный интервал: [", round(t_test_one$conf.int[1], 2), 
      ",", round(t_test_one$conf.int[2], 2), "]\n")
} else {
  cat("\n Нет оснований отвергать H₀: средний вес не отличается от", mu0, "кг\n")
}


cat("\n4. Сравнение двух видов спорта (Плаванье vs Водное поло)\n")

water_polo <- subset(ath, Sport == "Water Polo" & Sex == "M")
water_polo_weight <- na.omit(water_polo$Weight)

cat("Плаванье (мужчины): n =", length(male_weight), 
    ", средний вес =", round(mean(male_weight), 2), "кг\n")
cat("Водное поло (мужчины): n =", length(water_polo_weight), 
    ", средний вес =", round(mean(water_polo_weight), 2), "кг\n\n")

shapiro_polo <- shapiro.test(water_polo_weight)
cat("Тест Шапиро-Уилка (водное поло): p-value =", shapiro_polo$p.value, "\n")

var_test <- var.test(male_weight, water_polo_weight)
cat("\nТест Фишера на равенство дисперсий:\n")
print(var_test)

if(var_test$p.value < alpha) {
  cat("Дисперсии различаются, используем тест Уэлча\n")
  two_sample_test <- t.test(male_weight, water_polo_weight, var.equal = FALSE)
} else {
  cat("Дисперсии равны, используем классический t-тест\n")
  two_sample_test <- t.test(male_weight, water_polo_weight, var.equal = TRUE)
}

cat("\nРезультаты двухвыборочного t-теста:\n")
print(two_sample_test)

wilcox_two <- wilcox.test(male_weight, water_polo_weight)
cat("\nРезультаты теста Вилкоксона:\n")
print(wilcox_two)

if(two_sample_test$p.value < alpha) {
  cat("\nОтвергаем H₀: средний вес пловцов и ватерполистов различается\n")
  cat("  Разница в средних:", round(diff(two_sample_test$estimate), 2), "кг\n")
  cat("  95% ДИ для разницы: [", round(two_sample_test$conf.int[1], 2), 
      ",", round(two_sample_test$conf.int[2], 2), "]\n")
} else {
  cat("\nНет оснований отвергать H₀: средний вес не различается\n")
}

boxplot(list("Плаванье" = male_weight, "Водное поло" = water_polo_weight),
        main = "Сравнение веса спортсменов (мужчины)",
        xlab = "Вид спорта",
        ylab = "Вес (кг)",
        col = c("lightblue", "lightgreen"),
        ylim = range(c(male_weight, water_polo_weight), na.rm = TRUE))

points(1, mean(male_weight), col = "red", pch = 19, cex = 1.5)
points(2, mean(water_polo_weight), col = "red", pch = 19, cex = 1.5)

cat("1. Дескриптивный анализ показал, что...\n")
cat("2. Проверка нормальности:", 
    ifelse(shapiro_male$p.value < 0.05, "вес мужчин не нормален", "вес мужчин нормален"), 
    ";",
    ifelse(shapiro_female$p.value < 0.05, "вес женщин не нормален", "вес женщин нормален"), "\n")
cat("3. Одновыборочный t-тест:", 
    ifelse(t_test_one$p.value < 0.05, 
           "отвергает H₀ о среднем весе = 75 кг", 
           "не отвергает H₀ о среднем весе = 75 кг"), "\n")
cat("4. Двухвыборочный тест (плаванье vs водное поло):", 
    ifelse(two_sample_test$p.value < 0.05,
           "показал значимое различие",
           "не показал значимого различия"), "\n")
