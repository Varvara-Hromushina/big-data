#1

df_csv <- read.csv2("lab2/Книги.csv")
str(df_csv)

rownames(df_csv) <- df_csv[, 1]
df_csv <- df_csv[, -1]
str(df_csv)


library(readxl)

df_xlsx <- read_excel("lab2/Книги.xlsx", 1)

# Преобразуем tibble в обычный data.frame
df_xlsx <- as.data.frame(df_xlsx)

str(df_xlsx)

rownames(df_xlsx) <- df_xlsx[, 1]
df_xlsx <- df_xlsx[, -1]
df_xlsx

#2

calc_stats <- function(x)
{
  c(
    Среднее = mean(x, na.rm = TRUE),
    Медиана = median(x, na.rm = TRUE),
    Мин = min(x, na.rm = TRUE),
    Макс = max(x, na.rm = TRUE),
    Стд_откл = sd(x, na.rm = TRUE),
    Дисперсия = var(x, na.rm = TRUE),
    IQR = IQR(x, na.rm = TRUE)
  )
}

books_stats <- t(apply(df_csv, 2, calc_stats))
books_stats <- round(books_stats, 2)

print("Таблица 1 - Дескриптивная статистика по книгам:")
print(books_stats)


quartiles <- t(apply(df_csv, 2, quantile, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE))
quartiles <- round(quartiles, 2)

print("Таблица 2 - Квартили по книгам:")
print(quartiles)


students_means <- rowMeans(df_csv, na.rm = TRUE)
students_means <- sort(students_means, decreasing = TRUE)

print("Таблица 3 - Рейтинг студентов по средним оценкам:")
print(t(round(students_means,2)))


books_means <- colMeans(df_csv, na.rm = TRUE)
books_means <- sort(books_means, decreasing = TRUE)

print("Таблица 4 - Рейтинг книг по средним оценкам:")
print(round(books_means,2))


cg_col <- which(names(df_csv) == "X.Вишнёвый.сад...А.П..Чехов.")
cg_fans <-  df_csv[df_csv[, cg_col] > 7, ]

print("Студенты, оценившие 'Вишнёвый сад' выше 7:")
print(rownames(cg_fans))

# Удаляем строки с NA в именах
cg_fans_clean <- cg_fans[!is.na(rownames(cg_fans)) & rownames(cg_fans) != "NA", ]

print("Очищенный список фанатов:")
print(rownames(cg_fans_clean))

cg_subset_stats_clean <- data.frame(
  Показатель = c("Среднее", "Медиана", "Мин", "Макс", "Стд_отклонение"),
  Значение = c(
    mean(rowMeans(cg_fans_clean, na.rm = TRUE), na.rm = TRUE),
    median(rowMeans(cg_fans_clean, na.rm = TRUE), na.rm = TRUE),
    min(rowMeans(cg_fans_clean, na.rm = TRUE), na.rm = TRUE),
    max(rowMeans(cg_fans_clean, na.rm = TRUE), na.rm = TRUE),
    sd(rowMeans(cg_fans_clean, na.rm = TRUE), na.rm = TRUE)
  )
)

print("Таблица 5 - Статистика по подмножеству фанатов Вишнёвый сад:")
print(cg_subset_stats_clean)



cg_means <- rowMeans(cg_fans, na.rm = TRUE)
cg_means <- cg_means[!is.na(cg_means)]

hist(cg_means,
     main = "Рис.1. Гистограмма распределения средних оценок\n(фанаты Вишнёвого сада)",
     xlab = "Средняя оценка по всем книгам",
     ylab = "Частота",
     col = "lightblue",
     border = "darkblue"
     )


abline(v = mean(cg_means), col = "red", lwd = 2, lty = 1)
abline(v = median(cg_means), col = "green", lwd = 2, lty = 2)

legend("topright",
       legend = c(paste("Среднее =", round(mean(cg_means), 2)),
                  paste("Медиана =", round(median(cg_means), 2))),
       col = c("red", "green"),
       lty = c(1, 2),
       lwd = 2)



boxplot(cg_fans,
        main = "Рис.2. Боксплот распределения оценок фанатов Вишнёвого сада",
        xlab = "Книги",
        ylab = "Оценка",
        col = "lightblue",
        las = 2,
        cex.axis = 0.7)

abline(h = mean(as.matrix(cg_fans), na.rm = TRUE), 
       col = "red", lwd = 2, lty = 2)

legend("bottomright",
       legend = paste("Общее среднее =", round(mean(as.matrix(cg_fans), na.rm = TRUE), 2)),
       col = "red",
       lty = 2,
       lwd = 2)



cg_median_measures <- data.frame(
  Медиана = apply(cg_fans, 2, median, na.rm = TRUE),
  Q1 = apply(cg_fans, 2, quantile, probs = 0.25, na.rm = TRUE),
  Q3 = apply(cg_fans, 2, quantile, probs = 0.75, na.rm = TRUE),
  IQR = apply(cg_fans, 2, IQR, na.rm = TRUE)
)

print("Таблица 6 - Серединные меры для подмножества:")
print(cg_median_measures)




