# Вариант: Германия, Грузия, Киргизия, Казахстан, Азербайджан, США, Россия

library(learningtower)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(tidyr)


data_file_csv <- "lab3/pisa_data.csv"

student_data <- read.csv(data_file_csv)



target_countries <- c("DEU", "GEO", "KGZ", "KAZ", "AZE", "USA", "RUS")

df <- student_data %>%
  filter(country %in% target_countries)

cat("Фрагмент исходных данных (первые 10 строк)\n")
print(head(df, 10))

# 2

avg_scores <- df %>%
  group_by(country, year) %>%
  summarise(avg_math = mean(math, na.rm = TRUE),
            avg_read = mean(read, na.rm = TRUE),
            avg_science = mean(science, na.rm = TRUE),
            .groups = 'drop')

rus_scores <- avg_scores %>% filter(country == "RUS")
ger_scores <- avg_scores %>% filter(country == "DEU")

cat("\nСредние баллы по России:\n")
print(rus_scores)



ggplot(rus_scores, aes(x = year)) +
  geom_line(aes(y = avg_math, color = "Математика"), size = 1.2) +
  geom_line(aes(y = avg_read, color = "Чтение"), size = 1.2) +
  geom_line(aes(y = avg_science, color = "Естествознание"), size = 1.2) +
  geom_point(aes(y = avg_math), size = 3) +
  geom_point(aes(y = avg_read), size = 3) +
  geom_point(aes(y = avg_science), size = 3) +
  labs(title = "Рис.1. Средние баллы PISA по годам – Россия",
       x = "Год исследования", y = "Средний балл", color = "Предмет:") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14))


ggplot(ger_scores, aes(x = year)) +
  geom_line(aes(y = avg_math, color = "Математика"), size = 1.2) +
  geom_line(aes(y = avg_read, color = "Чтение"), size = 1.2) +
  geom_line(aes(y = avg_science, color = "Естествознание"), size = 1.2) +
  geom_point(aes(y = avg_math), size = 3) +
  geom_point(aes(y = avg_read), size = 3) +
  geom_point(aes(y = avg_science), size = 3) +
  labs(title = "Рис.2. Средние баллы PISA по годам – Германия",
       x = "Год исследования", y = "Средний балл", color = "Предмет:") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14))



last_year <- max(rus_scores$year)
rus_last <- rus_scores %>% 
  filter(year == last_year) %>%
  pivot_longer(cols = starts_with("avg"), names_to = "subject", values_to = "score") %>%
  mutate(subject_rus = case_when(
    subject == "avg_math" ~ "Математика",
    subject == "avg_read" ~ "Чтение",
    subject == "avg_science" ~ "Естествознание"
  ))

ggplot(rus_last, aes(x = subject_rus, y = score, fill = subject_rus)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(score, 1)), vjust = -0.5, size = 5) +
  labs(title = paste("Рис.3. Средние баллы в", last_year, "году (Россия)"),
       x = "Предмет", y = "Средний балл") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

# 4

df_clean <- df %>%
  filter(!is.na(gender))

gender_avg <- df_clean %>%
  group_by(gender) %>%
  summarise(math = mean(math, na.rm = TRUE),
            read = mean(read, na.rm = TRUE),
            science = mean(science, na.rm = TRUE),
            .groups = 'drop') %>%
  pivot_longer(cols = c(math, read, science), 
               names_to = "subject", 
               values_to = "mean_score")

gender_counts <- bind_rows(
  df_clean %>%
    filter(!is.na(math)) %>%
    group_by(gender, subject = "math") %>%
    summarise(count = n(), .groups = 'drop'),

  df_clean %>%
    filter(!is.na(read)) %>%
    group_by(gender, subject = "read") %>%
    summarise(count = n(), .groups = 'drop'),

  df_clean %>%
    filter(!is.na(science)) %>%
    group_by(gender, subject = "science") %>%
    summarise(count = n(), .groups = 'drop')
)

gender_avg <- gender_avg %>%
  left_join(gender_counts, by = c("gender", "subject")) %>%
  group_by(subject) %>%
  mutate(total_subject = sum(count),
         percent = round(count / total_subject * 100, 1)) %>%
  ungroup() %>%
  mutate(percent = ifelse(is.na(percent), 0, percent)) %>%
  mutate(label = paste0(ifelse(gender == "female", "Жен", "Муж"), 
                        " ", case_when(
                          subject == "math" ~ "Матем.",
                          subject == "read" ~ "Чтение",
                          subject == "science" ~ "Наука"
                        ), "\n",
                        round(mean_score, 1), " баллов\n",
                        "(", percent, "%)"))

cat("\nПроверка данных для графиков:\n")
print(gender_avg)

math_data <- gender_avg %>% 
  filter(subject == "math") %>%
  arrange(gender)

read_data <- gender_avg %>% 
  filter(subject == "read") %>%
  arrange(gender)

science_data <- gender_avg %>% 
  filter(subject == "science") %>%
  arrange(gender)

par(mfrow = c(1, 3))
par(mar = c(0, 2, 4, 2))

if(nrow(math_data) == 2 && all(!is.na(math_data$mean_score))) {
  pie(math_data$mean_score,
      labels = math_data$label,
      col = c("#FF9999", "#66B2FF"),
      main = paste("Математика\n"),
      cex = 0.8,
      radius = 1)
} else {
  plot.new()
  text(0.5, 0.5, "Нет данных для математики")
}

# Чтение
if(nrow(read_data) == 2 && all(!is.na(read_data$mean_score))) {
  pie(read_data$mean_score,
      labels = read_data$label,
      col = c("#FF9999", "#66B2FF"),
      main = paste("Чтение\n"),
      cex = 0.8,
      radius = 1)
} else {
  plot.new()
  text(0.5, 0.5, "Нет данных для чтения")
}

if(nrow(science_data) == 2 && all(!is.na(science_data$mean_score))) {
  pie(science_data$mean_score,
      labels = science_data$label,
      col = c("#FF9999", "#66B2FF"),
      main = paste("Естествознание\n"),
      cex = 0.8,
      radius = 1)
} else {
  plot.new()
  text(0.5, 0.5, "Нет данных для естествознания")
}

legend("topright", 
       legend = c("Женщины", "Мужчины"),
       fill = c("#FF9999", "#66B2FF"),
       cex = 0.8,
       bty = "n",
       title = "Пол")

par(mfrow = c(1, 1))


# 5

math_men <- df %>% 
  filter(gender == "male", !is.na(math)) %>% 
  pull(math)

math_women <- df %>% 
  filter(gender == "female", !is.na(math)) %>% 
  pull(math)

cat("Количество мужчин с оценками по математике:", length(math_men), "\n")
cat("Количество женщин с оценками по математике:", length(math_women), "\n")

par(mfrow = c(1, 2))

if(length(math_men) > 0) {
  hist(math_men, breaks = 30, col = rgb(0.2, 0.4, 0.8, 0.7),
       main = "Рис.5(а). Распределение баллов (Математика)\nМужчины",
       xlab = "Баллы PISA", ylab = "Частота", xlim = c(200, 800))
} else {
  plot.new()
  text(0.5, 0.5, "Нет данных для мужчин")
}

if(length(math_women) > 0) {
  hist(math_women, breaks = 30, col = rgb(0.8, 0.3, 0.3, 0.7),
       main = "Рис.5(б). Распределение баллов (Математика)\nЖенщины",
       xlab = "Баллы PISA", ylab = "Частота", xlim = c(200, 800))
} else {
  plot.new()
  text(0.5, 0.5, "Нет данных для женщин")
}

par(mfrow = c(1, 1))

# 6

country_names <- c("DEU" = "Германия", "GEO" = "Грузия", "KGZ" = "Киргизия",
                   "KAZ" = "Казахстан", "AZE" = "Азербайджан", 
                   "USA" = "США", "RUS" = "Россия")

ggplot(avg_scores, aes(x = year, y = avg_math, color = country)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_brewer(palette = "Set1", labels = country_names) +
  labs(title = "Рис.6. Динамика баллов по математике (все страны)",
       x = "Год", y = "Средний балл по математике", color = "Страна:") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14)) +
  guides(color = guide_legend(nrow = 3))


# 7

years_unique <- sort(unique(df$year))
first_year <- min(years_unique)
mid_year <- years_unique[ceiling(length(years_unique)/2)]
last_year <- max(years_unique)

df_computers <- df %>%
  filter(year %in% c(first_year, mid_year, last_year)) %>%
  mutate(computer_value = case_when(
    computer_n == "0" ~ 0,
    computer_n == "1" ~ 1,
    computer_n == "2" ~ 2,
    computer_n == "3+" ~ 3.5,
    TRUE ~ NA_real_
  )) %>%
  filter(!is.na(computer_value))

avg_computers <- df_computers %>%
  group_by(year) %>%
  summarise(avg_comp = mean(computer_value, na.rm = TRUE),
            n_students = n(),
            .groups = 'drop')

cat("Среднее количество компьютеров по годам:\n")
print(avg_computers)

par(mar = c(0, 0, 4, 0))
pie(avg_computers$avg_comp,
    labels = paste0(avg_computers$year, " г.\n", 
                    round(avg_computers$avg_comp, 2), " шт."),
    col = c("#F39C89", "#E74C3C", "#3498DB"),
    main = "Рис.7. Среднее количество компьютеров у учащихся",
    cex = 0.9)
legend("topright", legend = avg_computers$year, 
       fill = c("#F39C89", "#E74C3C", "#3498DB"), title = "Год")


