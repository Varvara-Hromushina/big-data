library(datasets)
data("longley")
# 7.2.1
# Количество переменных
ncol(longley)

# Объем выборки
nrow(longley)

# Дескриптивный анализ
summary(longley)

# Корреляционная матрица
cor_matrix <- cor(longley)
print(cor_matrix)

# Диаграммы рассеяния для самых коррелирующих пар
plot(longley$GNP, longley$Year, main = "GNP vs Year", col = "blue")
abline(lm(Year ~ GNP, data = longley), col = "red")

plot(longley$Population, longley$GNP, main = "Population vs GNP", col = "blue")
abline(lm(GNP ~ Population, data = longley), col = "red")

# Матрица диаграмм рассеяния
pairs(longley, main = "Корреляционная матрица longley")

# Нормальность
lapply(longley, shapiro.test)

# Графическая нормальность
par(mfrow = c(2, 4))
for (i in 1:ncol(longley)) {
  qqnorm(longley[, i], main = names(longley)[i])
  qqline(longley[, i], col = "red")
}

# 7.2.2
x <- rexp(50)
cor(x, log(x), method = "spearman")


# Доказательство корреляции

# 1. Диаграмма рассеяния
plot(x, log(x), main = "x vs log(x)", col = "blue", pch = 19)
abline(lm(log(x) ~ x), col = "red")

# 2. График зависимости рангов
plot(rank(x), rank(log(x)), main = "Ранги x и log(x)", col = "darkgreen", pch = 19)
abline(0, 1, col = "red", lwd = 2)

# 3. Монотонность
plot(sort(x), sort(log(x)), type = "l", main = "Монотонная связь", col = "purple", lwd = 2)
