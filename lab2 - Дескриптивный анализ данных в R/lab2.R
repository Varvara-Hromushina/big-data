df <- read.csv2("lab2/Книги.csv")
str(df)

rownames(df) <- df[, 1]
df <- df[, -1]
str(df)

columns_max <- apply(df, 2, max, na.rm = TRUE)

columns_min <- apply(df, 2, min, na.rm = TRUE)

columns_mean <- apply(df, 2, mean, na.rm = TRUE)

stats_table <- data.frame (
  Максимум = columns_max,
  Минимум = columns_min,
  Среднее = columns_mean
)

print(stats_table)

book_name <- names(df)[1]
book_rating <- df[, 1]
high_ratings <- sum(book_rating > 7, na.rm = TRUE)
low_ratings <- sum(book_rating < 3, na.rm = TRUE)

pref_vector <- c(high_ratings, low_ratings)
names(pref_vector) <- c(paste(book_name, "> 7"), paste(book_name, "< 3"))
print(pref_vector)

book_rating <- sort(columns_mean, decreasing = TRUE)

rating_table = data.frame (
  Рейтинг = 1:length(book_rating),
  Среднее_значение = book_rating
)

print(rating_table)

cat("Способ 1: Расчет с na.rm = TRUE\nСреднее значение по первой книге с NA:", columns_mean[1])

df_filled <- df

for (i in 1:ncol(df_filled)){
  col_mean <- mean(df_filled[, i], na.rm = TRUE)
  df_filled[is.na(df_filled[, i]), i] <- col_mean
}

cat("\nСпособ 2: Расчёт с заменой NA на среднее по этому столбцу\nСреднее значение по первой книге с NA:\n")
print(df)
print(df_filled)

second_book_fans <- df[which(df[, 2] > 7), ]
print(paste("Студенты, которым нравится", names(df)[2], "больше 7:"))
print(rownames(second_book_fans))

third_book_na <- df[is.na(df[, 3]), ]
print(paste("Студенты, которым не нравится", names(df)[3]))
print(rownames(third_took_na))


students_mean <- rowMeans(df, na.rm = TRUE)
higt_avg_students <- names(students_mean[students_mean > 7])
print("Студенты, которые любят читать (больше 7):")
print(higt_avg_students)



barplot(columns_mean, 
        main = "Средние оценки книг",
        xlab = "Книги", 
        ylab = "Средняя оценка",
        col = "steelblue",
        las = 2,
        cex.names = 0.8,
        ylim = c(0, 10))

abline(h = mean(columns_mean), col = "red", lwd = 2, lty = 2)
legend("topright", "Среднее всех книг", col = "red", lty = 2, lwd = 2)


library(ggplot2)

books_df <- data.frame(
  Книга = names(columns_mean),
  Средняя_оценка = columns_mean
)

ggplot(books_df, aes(x = reorder(Книга, -Средняя_оценка), y = Средняя_оценка)) +
  geom_col(fill = "skyblue", color = "black") +
  labs(title = "Средние оценки книг",
       x = "Книги",
       y = "Средняя оценка") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 75, hjust = 1))
