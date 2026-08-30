# 1

df_csv <- read.csv2("lab2/Книги.csv")
str(df_csv)

rownames(df_csv) <- df_csv[, 1]
df_csv <- df_csv[, -1]
str(df_csv)

# 2

high_rating_students <- df_csv[df_csv[, 1] > 7, ]
print("2.1 Студенты с оценкой >7 по 'Мастер и Маргарита':")
print(rownames(high_rating_students))

selected_books <- df_csv[, c(1, 5, 8, 10)]
print("2.2 Подмножество из 4 книг:")
print(names(selected_books))

tolstoy_lovers <- subset(df_csv, 
                         df_csv$"X.Война.и.мир...Л.Н..Толстой." > 7 & 
                           df_csv$"X.Анна.Каренина...Л.Н..Толстой." > 7)
print("2.3 Студенты, любящие Толстого (оценки >7 по двум книгам):")
print(rownames(tolstoy_lovers))

students_with_na <- df_csv[!complete.cases(df_csv), ]
print("2.4 Студенты с пропущенными данными:")
print(rownames(students_with_na))

# 3

df_temp <- df_csv
groza_col <- which(names(df_temp) == "X.Гроза...А.Н..Островский.")
df_without_groza <- df_temp[, -groza_col]
print("3.1 Удалена колонка 'Гроза'")
print(paste("Было колонок:", ncol(df_temp), "Стало:", ncol(df_without_groza)))

df_few_books <- df_temp[, -c(2, 4, 6, 9)]
print("3.2 Удалены книги с индексами 2,4,6,9")
print(paste("Осталось книг:", ncol(df_few_books)))
print(names(df_few_books))

na_count <- colSums(is.na(df_temp))
cols_to_remove <- names(na_count[na_count > 1])
df_no_na_cols <- df_temp[, !names(df_temp) %in% cols_to_remove]
print("3.3 Удалены колонки с большим количеством NA:")
print(cols_to_remove)
print(paste("Осталось колонок:", ncol(df_no_na_cols)))


# 4

new_student1 <- data.frame(
  X.Мастер.и.Маргарита...М.А..Булгаков. = 9,
  X.Отцы.и.дети...И.С..Тургенев. = 8,
  X.Анна.Каренина...Л.Н..Толстой. = 7,
  X.Гроза...А.Н..Островский. = 6,
  X.Вишнёвый.сад...А.П..Чехов. = 9,
  X.Горе.от.ума...А.С..Грибоедов. = 8,
  X.Евгений.Онегин...А.С..Пушкин. = 7,
  X.Преступление.и.наказание...Ф.М..Достоевский. = 8,
  X.Дубровский...А.С..Пушкин. = 7,
  X.Война.и.мир...Л.Н..Толстой. = 6
)

new_student1 <- new_student1[, names(df_csv)]

df_with_new <- rbind(df_csv, new_student1)
rownames(df_with_new)[nrow(df_with_new)] <- "Петров_А."

print("4.1 Добавлен новый студент (Петров_А.):")
print(df_with_new["Петров_А.", ])

new_student2 <- data.frame(
  X.Мастер.и.Маргарита...М.А..Булгаков. = 7,
  X.Отцы.и.дети...И.С..Тургенев. = NA,
  X.Анна.Каренина...Л.Н..Толстой. = 8,
  X.Гроза...А.Н..Островский. = NA,
  X.Вишнёвый.сад...А.П..Чехов. = 9,
  X.Горе.от.ума...А.С..Грибоедов. = 7,
  X.Евгений.Онегин...А.С..Пушкин. = NA,
  X.Преступление.и.наказание...Ф.М..Достоевский. = 8,
  X.Дубровский...А.С..Пушкин. = 7,
  X.Война.и.мир...Л.Н..Толстой. = NA
)

new_student2 <- new_student2[, names(df_csv)]
df_with_new <- rbind(df_with_new, new_student2)
rownames(df_with_new)[nrow(df_with_new)] <- "Сидорова_М."

print("4.2 Добавлен новый студент с пропусками (Сидорова_М.):")
print(df_with_new["Сидорова_М.", ])

print(paste("Было строк:", nrow(df_csv), "Стало:", nrow(df_with_new)))

# 5

# 5.1 Создаем первую дополнительную таблицу (информация о студентах)
student_info <- data.frame(
  Студент = rownames(df_csv),
  Возраст = c(19, 20, 18, 21, 19, 20, 22, 19, 20, 18, 21, 19, 20, 18, 19, 20),
  Пол = c("Ж", "Ж", "Ж", "Ж", "Ж", "Ж", "Ж", "Ж", "Ж", "Ж", "Ж", "Ж", "Ж", "Ж", "Ж", "Ж"),
  Курс = c(2, 3, 1, 3, 2, 2, 4, 2, 3, 1, 3, 2, 2, 1, 2, 3)
)

print("5.1 Таблица с информацией о студентах:")
print(head(student_info))

# 5.2 Создаем вторую дополнительную таблицу (любимые жанры)
favorite_genres <- data.frame(
  Студент = rownames(df_csv)[c(1, 3, 5, 7, 9, 11, 13, 15)],
  Любимый_жанр = c("Роман", "Драма", "Поэзия", "Комедия", 
                   "Роман", "Драма", "Поэзия", "Комедия"),
  Чтение_в_год = c(12, 5, 8, 3, 15, 6, 10, 4)
)

print("5.2 Таблица с любимыми жанрами (не все студенты):")
print(favorite_genres)

# 5.3 Внутреннее слияние (inner join) - только общие студенты
inner_join_result <- merge(student_info, favorite_genres, by = "Студент")
print("5.3 Внутреннее слияние (только студенты, у которых есть жанры):")
print(inner_join_result)

# 5.4 Левое слияние (left join) - все студенты из первой таблицы
left_join_result <- merge(student_info, favorite_genres, by = "Студент", all.x = TRUE)
print("5.4 Левое слияние (все студенты из info):")
print(head(left_join_result))
print(paste("Пропуски в жанрах:", sum(is.na(left_join_result$Любимый_жанр))))

# 5.5 Правое слияние (right join) - все студенты из второй таблицы
right_join_result <- merge(student_info, favorite_genres, by = "Студент", all.y = TRUE)
print("5.5 Правое слияние (все студенты из genres):")
print(right_join_result)

# 5.6 Полное внешнее слияние (full outer join)
student_extra <- data.frame(
  Студент = c(rownames(df_csv)[c(2, 4, 6)], "Иванов_Петр"),
  Хобби = c("Спорт", "Музыка", "Рисование", "Фотография")
)

full_join_result <- merge(student_info, student_extra, by = "Студент", all = TRUE)
print("5.6 Полное внешнее слияние:")
print(full_join_result)



# Рис.3: Сравнение исходных данных и после добавления строк

boxplot(df_csv,
        main = "Исходные данные (16 студентов)",
        col = "lightblue",
        las = 2,
        cex.axis = 0.6,
        ylim = c(0, 10))

boxplot(df_with_new[1:nrow(df_csv), ],
        main = "После добавления 2 студентов",
        col = "lightgreen",
        las = 2,
        cex.axis = 0.6,
        ylim = c(0, 10))

par(mfrow = c(1, 1))


# Рис.4: Результаты слияния

join_counts <- data.frame(
  Тип_слияния = c("Исходные", "Inner Join", "Left Join", "Right Join", "Full Join"),
  Количество = c(nrow(student_info), 
                 nrow(inner_join_result),
                 nrow(left_join_result),
                 nrow(right_join_result),
                 nrow(full_join_result))
)

barplot(join_counts$Количество,
        names.arg = join_counts$Тип_слияния,
        main = "Рис.4. Количество студентов после разных типов слияния",
        col = c("gray", "lightblue", "lightgreen", "lightcoral", "lightyellow"),
        ylab = "Количество студентов",
        ylim = c(0, 20))

text(1:5, join_counts$Количество + 0.5, 
     labels = join_counts$Количество, 
     cex = 1.2)

