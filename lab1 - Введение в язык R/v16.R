m <- as.integer(readline(prompt = "Введите количество строк m: "))
n <- as.integer(readline(prompt = "Введите количество столбцов n: "))

set.seed(123)
matr <- matrix(sample(1:100, m * n, replace = TRUE), nrow = m, ncol = n)

rownames(matr) <- paste("Row", 1:m, sep = "_")
colnames(matr) <- paste("Col", 1:n, sep = "_")

cat("\nИсходная матрица:\n")
print(matr)

if (m >= 2) {
  even_rows <- seq(2, m, by = 2)
  submatr <- matr[even_rows, , drop = FALSE]
  cat("\nПодматрица из чётных строк:\n")
  print(submatr)
} else {
  cat("\nНедостаточно строк для формирования подматрицы из чётных строк.\n")
}

max_per_col <- apply(matr, 2, max, na.rm = TRUE)
cat("\nМаксимальные элементы столбцов:\n")
print(max_per_col)




