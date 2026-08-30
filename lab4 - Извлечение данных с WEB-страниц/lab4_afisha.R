library(rvest)
library(dplyr)
library(stringr)

folder_path <- "lab4/museum_pages"
html_files <- list.files(folder_path, pattern = "\\.html$", full.names = TRUE)

cat("Найдено HTML-файлов:", length(html_files), "\n\n")

extract_address <- function(html) {
  address <- html %>%
    html_node("span.TSyWq.kiWzk") %>%
    html_text(trim = TRUE)
  
  if (length(address) > 0 && !is.na(address) && address != "") {
    # Удаляем "Музей в Москве" и подобные фразы
    address <- gsub("^Музей в Москве\\s*", "", address)
    address <- gsub("^Выставочный зал в Москве\\s*", "", address)
    address <- gsub("\\d+ отзывов$", "", address)
    return(trimws(address))
  }
  
  return("Не указан")
}

extract_name <- function(html) {
  name <- html %>%
    html_node("h1 div.oOY35, h1 div[data-test='ITEM-NAME']") %>%
    html_text(trim = TRUE)
  
  if (length(name) == 0 || is.na(name) || name == "") {
    name <- html %>%
      html_node("h1") %>%
      html_text(trim = TRUE)
    # Убираем "Музей в Москве" из названия
    name <- gsub("Музей в Москве\\s*$", "", name)
  }
  
  return(trimws(name))
}

all_museums <- data.frame()

for (file in html_files) {
  cat("Обработка файла:", basename(file), "\n")
  
  html_content <- read_html(file)
  
  name <- extract_name(html_content)
  address <- extract_address(html_content)
  
  working_hours <- html_content %>%
    html_node("tr.K1tRv:contains('Часы работы') td.eNJOm") %>%
    html_text(trim = TRUE)
  
  price <- html_content %>%
    html_node("tr.K1tRv:contains('Цены') td.eNJOm, tr.K1tRv:contains('Цена') td.eNJOm") %>%
    html_text(trim = TRUE)
  
  link <- html_content %>%
    html_node("link[rel='canonical']") %>%
    html_attr("href")
  
  if (length(link) == 0 || is.na(link)) {
    link <- basename(file)
  }
  
  if (!is.na(name) && name != "" && name != "Не указано") {
    all_museums <- rbind(all_museums, data.frame(
      Название = name,
      Адрес = address,
      Режим_работы = ifelse(length(working_hours) > 0 && !is.na(working_hours), working_hours, "Не указан"),
      Цена = ifelse(length(price) > 0 && !is.na(price), price, "Не указана"),
      Ссылка = link,
      stringsAsFactors = FALSE
    ))
    cat("  Добавлен:", name, "| Адрес:", address, "\n")
  }
}

all_museums <- all_museums %>% distinct(Название, .keep_all = TRUE)

cat("Всего музеев:", nrow(all_museums), "\n\n")

print(head(all_museums, 10))

write.csv2(all_museums, "museums_msk.csv", row.names = FALSE, fileEncoding = "CP1251")
cat("\nФайл сохранен: museums_msk.csv\n")