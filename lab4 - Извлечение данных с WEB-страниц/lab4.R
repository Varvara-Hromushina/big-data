# Вариант: Германия, Грузия, Киргизия, Казахстан, Азербайджан, США, Россия

library(rvest)
library(dplyr)
library(ggplot2)
library(tidyr)

folder_path <- "lab4/numbeo_pages"
html_files <- list.files(folder_path, pattern = "\\.html$", full.names = TRUE)

extract_numbeo_data <- function(file_path) {
  year <- as.numeric(gsub(".*?(\\d{4}).*", "\\1", basename(file_path)))
  page <- read_html(file_path)
  tables <- page %>% html_nodes("table")
  
  if (length(tables) < 2) return(NULL)
  
  suppressWarnings({
    df <- tables[[2]] %>% html_table(fill = TRUE) %>% as.data.frame()
    colnames(df) <- as.character(df[1, ])
    df <- df[-1, ]
    
    names(df) <- c("Rank", "Country", "Quality_of_Life_Index", 
                   "Purchasing_Power_Index", "Safety_Index", 
                   "Health_Care_Index", "Cost_of_Living_Index", 
                   "Property_Price_to_Income_Ratio", 
                   "Traffic_Commute_Time_Index", 
                   "Pollution_Index", "Climate_Index")
    df$Year <- year
    
    numeric_cols <- names(df)[3:11]
    for (col in numeric_cols) {
      df[[col]] <- as.numeric(as.character(df[[col]]))
    }
  })
  return(df)
}

target_countries <- c("Germany", "Georgia", "Kyrgyzstan", "Kazakhstan", 
                      "Azerbaijan", "United States", "Russia")

all_data <- data.frame()
for (file in html_files) {
  df <- extract_numbeo_data(file)
  if (!is.null(df)) {
    df_filtered <- df %>% filter(Country %in% target_countries)
    if (nrow(df_filtered) > 0) {
      all_data <- bind_rows(all_data, df_filtered)
    }
  }
}

all_data <- all_data %>%
  mutate(Country_Ru = case_when(
    Country == "Germany" ~ "Германия",
    Country == "Georgia" ~ "Грузия",
    Country == "Kyrgyzstan" ~ "Киргизия",
    Country == "Kazakhstan" ~ "Казахстан",
    Country == "Azerbaijan" ~ "Азербайджан",
    Country == "United States" ~ "США",
    Country == "Russia" ~ "Россия"
  ))

all_data$Year <- as.numeric(all_data$Year)

write.csv2(all_data, "numbeo_final.csv", row.names = FALSE, fileEncoding = "CP1251")

# Индекс качества жизни (все страны)

p1 <- ggplot(all_data, aes(x = Year, y = Quality_of_Life_Index, 
                           color = Country_Ru, group = Country_Ru)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Динамика индекса качества жизни (2014-2026)",
       x = "Год", y = "Индекс качества жизни", color = "Страна") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("quality_of_life_plot.png", p1, width = 10, height = 6)

# Все показатели (фасетный график)

indicators_long <- all_data %>%
  select(Year, Country_Ru, Quality_of_Life_Index, Purchasing_Power_Index, 
         Safety_Index, Health_Care_Index, Cost_of_Living_Index, 
         Property_Price_to_Income_Ratio, Traffic_Commute_Time_Index,
         Pollution_Index, Climate_Index) %>%
  pivot_longer(cols = -c(Year, Country_Ru), names_to = "Indicator", values_to = "Value") %>%
  filter(!is.na(Value))

indicator_names <- c(
  "Quality_of_Life_Index" = "Качество жизни",
  "Purchasing_Power_Index" = "Покупательная способность",
  "Safety_Index" = "Безопасность",
  "Health_Care_Index" = "Здравоохранение",
  "Cost_of_Living_Index" = "Стоимость жизни",
  "Property_Price_to_Income_Ratio" = "Цена жилья/доход",
  "Traffic_Commute_Time_Index" = "Время в пути",
  "Pollution_Index" = "Загрязнение",
  "Climate_Index" = "Климат"
)

indicators_long$Indicator_Ru <- indicator_names[indicators_long$Indicator]

facet_plot <- ggplot(indicators_long, aes(x = Year, y = Value, 
                                          color = Country_Ru, group = Country_Ru)) +
  geom_line() +
  geom_point(size = 0.5) +
  facet_wrap(~Indicator_Ru, scales = "free_y", ncol = 3) +
  labs(title = "Динамика показателей качества жизни (2014-2026)",
       x = "Год", y = "Значение индекса", color = "Страна") +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("all_indicators_plot.png", facet_plot, width = 14, height = 10)



stats <- all_data %>%
  group_by(Country_Ru) %>%
  summarise(
    Качество_жизни = round(mean(Quality_of_Life_Index, na.rm = TRUE), 1),
    Покупательная_способность = round(mean(Purchasing_Power_Index, na.rm = TRUE), 1),
    Безопасность = round(mean(Safety_Index, na.rm = TRUE), 1),
    Здравоохранение = round(mean(Health_Care_Index, na.rm = TRUE), 1),
    Загрязнение = round(mean(Pollution_Index, na.rm = TRUE), 1)
  ) %>%
  arrange(desc(Качество_жизни))

print("Средние значения по странам:")
print(stats)

min_year <- min(all_data$Year, na.rm = TRUE)
max_year <- max(all_data$Year, na.rm = TRUE)

changes <- all_data %>%
  filter(Year %in% c(min_year, max_year)) %>%
  group_by(Country_Ru) %>%
  filter(n() == 2) %>%
  summarise(
    Качество_жизни_рост = round((Quality_of_Life_Index[Year == max_year] - 
                                   Quality_of_Life_Index[Year == min_year]) / 
                                  Quality_of_Life_Index[Year == min_year] * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(Качество_жизни_рост))

print("Изменение качества жизни за период (%):")
print(changes)
