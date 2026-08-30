library(dplyr)
library(ggplot2)
library(tidyr)
library(cluster)
library(factoextra)
library(NbClust)
library(scatterplot3d)

football_data <- read.csv("lab5/15_International_Football/15_International_Football/results.csv", stringsAsFactors = FALSE)
football_data$date <- as.Date(football_data$date)

football_data <- football_data %>%
  mutate(
    total_goals = home_score + away_score,
    goal_diff = abs(home_score - away_score),
    result = case_when(
      home_score > away_score ~ "Home Win",
      home_score < away_score ~ "Away Win",
      TRUE ~ "Draw"
    )
  )



cat("Фрагмент исходного датасета\n")
print(head(football_data[, c("date", "home_team", "away_team", "home_score", "away_score", "tournament")], 10))

cat("\nДескриптивный анализ данных\n")
cat("Всего матчей:", nrow(football_data), "\n")
cat("Период:", format(min(football_data$date), "%Y-%m-%d"), "-", 
    format(max(football_data$date), "%Y-%m-%d"), "\n")
cat("Уникальных турниров:", length(unique(football_data$tournament)), "\n")
cat("Уникальных команд:", length(unique(c(football_data$home_team, football_data$away_team))), "\n")
cat("Среднее голов за матч:", round(mean(football_data$total_goals), 2), "\n")
cat("Медиана голов:", median(football_data$total_goals), "\n")
cat("Доля домашних побед:", round(mean(football_data$result == "Home Win") * 100, 2), "%\n")
cat("Доля ничьих:", round(mean(football_data$result == "Draw") * 100, 2), "%\n")
cat("Доля выездных побед:", round(mean(football_data$result == "Away Win") * 100, 2), "%\n")
cat("Доля нейтральных полей:", round(mean(football_data$neutral) * 100, 2), "%\n")

cat("\nДополнительное исследование\n")
yearly_goals <- football_data %>%
  mutate(year = as.numeric(format(date, "%Y"))) %>%
  group_by(year) %>%
  summarise(avg_goals = mean(total_goals), matches = n()) %>%
  filter(matches >= 50)

cat("Период с наибольшей результативностью:", 
    yearly_goals$year[which.max(yearly_goals$avg_goals)], 
    "г. (", round(max(yearly_goals$avg_goals), 2), "голов/матч)\n")
cat("Период с наименьшей результативностью:", 
    yearly_goals$year[which.min(yearly_goals$avg_goals)], 
    "г. (", round(min(yearly_goals$avg_goals), 2), "голов/матч)\n")

cat("\nПодготовка данных для кластеризации\n")

home_stats <- football_data %>%
  group_by(home_team) %>%
  summarise(home_matches = n(), home_wins = sum(home_score > away_score),
            home_goals_scored = sum(home_score), home_goals_conceded = sum(away_score),
            neutral_home = sum(neutral)) %>%
  rename(team = home_team)

away_stats <- football_data %>%
  group_by(away_team) %>%
  summarise(away_matches = n(), away_wins = sum(away_score > home_score),
            away_goals_scored = sum(away_score), away_goals_conceded = sum(home_score),
            neutral_away = sum(neutral)) %>%
  rename(team = away_team)

team_stats <- full_join(home_stats, away_stats, by = "team") %>%
  mutate(
    home_matches = coalesce(home_matches, 0), away_matches = coalesce(away_matches, 0),
    total_matches = home_matches + away_matches,
    total_wins = coalesce(home_wins, 0) + coalesce(away_wins, 0),
    total_goals_scored = coalesce(home_goals_scored, 0) + coalesce(away_goals_scored, 0),
    total_goals_conceded = coalesce(home_goals_conceded, 0) + coalesce(away_goals_conceded, 0),
    win_rate = total_wins / total_matches,
    goals_per_match = total_goals_scored / total_matches,
    goals_conceded_per_match = total_goals_conceded / total_matches,
    goal_diff_per_match = (total_goals_scored - total_goals_conceded) / total_matches,
    neutral_ratio = (coalesce(neutral_home, 0) + coalesce(neutral_away, 0)) / total_matches
  ) %>%
  filter(total_matches >= 30)

cat("Команд для кластеризации:", nrow(team_stats), "\n")

cluster_data <- team_stats %>%
  select(win_rate, goals_per_match, goals_conceded_per_match, 
         goal_diff_per_match, neutral_ratio) %>%
  scale()

cat("\nОценка оптимального числа кластеров\n")

set.seed(123)
k_max <- min(10, nrow(cluster_data) - 1)

wss <- sapply(1:k_max, function(k) kmeans(cluster_data, centers = k, nstart = 25)$tot.withinss)
sil_width <- sapply(2:k_max, function(k) {
  km <- kmeans(cluster_data, centers = k, nstart = 25)
  mean(silhouette(km$cluster, dist(cluster_data))[, 3])
})

par(mfrow = c(1, 2))
plot(1:k_max, wss, type = "b", pch = 19, xlab = "Число кластеров", 
     ylab = "Сумма квадратов", main = "Метод локтя")
abline(v = 4, col = "red", lty = 2)

plot(2:k_max, sil_width, type = "b", pch = 19, xlab = "Число кластеров",
     ylab = "Средняя ширина силуэта", main = "Метод силуэта")
abline(v = 4, col = "red", lty = 2)

gap_stat <- clusGap(cluster_data, FUN = kmeans, nstart = 25, K.max = k_max, B = 30)
par(mfrow = c(1, 1))
fviz_gap_stat(gap_stat) + ggtitle("Статистика разрыва")
optimal_gap <- which.max(gap_stat$Tab[, "gap"]) + 1

nbclust_result <- NbClust(cluster_data, distance = "euclidean", 
                          min.nc = 2, max.nc = min(8, k_max), 
                          method = "kmeans", index = "all")
barplot(table(nbclust_result$Best.nc[1,]), main = "Алгоритм консенсуса",
        xlab = "Число кластеров", ylab = "Количество методов", col = "steelblue")

cat("\nИерархическая кластеризация\n")

dist_matrix <- dist(cluster_data, method = "euclidean")
hc <- hclust(dist_matrix, method = "ward.D2")

par(mfrow = c(1, 1))
plot(hc, labels = team_stats$team, cex = 0.5, main = "Дендрограмма кластеризации",
     sub = "", xlab = "Команды", ylab = "Расстояние")
rect.hclust(hc, k = 4, border = 2:5)

team_stats$cluster_hc <- cutree(hc, k = 4)

cat("\n Визуализация групп\n")

cluster_long <- team_stats %>%
  select(cluster_hc, win_rate, goals_per_match, goals_conceded_per_match, 
         goal_diff_per_match, neutral_ratio) %>%
  pivot_longer(-cluster_hc, names_to = "metric", values_to = "value")

p1 <- ggplot(cluster_long, aes(x = factor(cluster_hc), y = value, fill = factor(cluster_hc))) +
  geom_bar(stat = "summary", fun = "mean") +
  facet_wrap(~metric, scales = "free_y", ncol = 3) +
  labs(x = "Кластер", y = "Среднее значение", fill = "Кластер") +
  theme_minimal()
print(p1)

p2 <- ggplot(cluster_long, aes(x = factor(cluster_hc), y = value, fill = factor(cluster_hc))) +
  geom_boxplot() +
  facet_wrap(~metric, scales = "free_y", ncol = 3) +
  labs(x = "Кластер", y = "Значение", fill = "Кластер") +
  theme_minimal()
print(p2)

cluster_summary <- team_stats %>%
  group_by(cluster_hc) %>%
  summarise(n = n(), win_rate = round(mean(win_rate), 3), 
            goals = round(mean(goals_per_match), 2),
            goal_diff = round(mean(goal_diff_per_match), 2))
print(cluster_summary)

cat("\nK-means кластеризация\n")

set.seed(123)
kmeans_result <- kmeans(cluster_data, centers = 4, nstart = 25)
team_stats$cluster_kmeans <- kmeans_result$cluster

pca <- prcomp(cluster_data, scale. = TRUE)
pca_df <- data.frame(PC1 = pca$x[, 1], PC2 = pca$x[, 2], 
                     cluster = factor(team_stats$cluster_kmeans))

p3 <- ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 2, alpha = 0.7) + stat_ellipse(aes(group = cluster), level = 0.68) +
  labs(title = "K-means кластеризация (PCA)") + theme_minimal()
print(p3)

centers_df <- as.data.frame(kmeans_result$centers)
colnames(centers_df) <- c("Win Rate", "Goals/PF", "Goals/PA", "Goal Diff", "Neutral Ratio")
rownames(centers_df) <- paste("Cluster", 1:4)

heatmap(as.matrix(centers_df), main = "Тепловая карта центров кластеров",
        xlab = "Метрики", ylab = "Кластеры",
        col = colorRampPalette(c("red", "white", "green"))(100), margins = c(8, 8))

comparison_table <- table(HC = team_stats$cluster_hc, KMeans = team_stats$cluster_kmeans)
print(comparison_table)

cat("\nScatterplot матрица\n")
pairs(cluster_data[, 1:4], col = team_stats$cluster_kmeans, pch = 19,
      main = "Scatterplot матрица кластеров")

cat("\nТрехмерная кластеризация\n")
colors_3d <- c("red", "blue", "green", "purple")[team_stats$cluster_kmeans]
scatterplot3d(pca$x[, 1:3], color = colors_3d, pch = 19,
              xlab = paste0("PC1 (", round(summary(pca)$importance[2, 1] * 100, 1), "%)"),
              ylab = paste0("PC2 (", round(summary(pca)$importance[2, 2] * 100, 1), "%)"),
              zlab = paste0("PC3 (", round(summary(pca)$importance[2, 3] * 100, 1), "%)"),
              main = "Трехмерная кластеризация")
legend("topright", legend = paste("Кластер", 1:4), 
       col = c("red", "blue", "green", "purple"), pch = 19)

cat("\nПроверка качества кластеризации\n")

anova_win <- aov(win_rate ~ factor(cluster_kmeans), data = team_stats)
print(summary(anova_win))

final_silhouette <- silhouette(team_stats$cluster_kmeans, dist(cluster_data))
avg_sil_width <- mean(final_silhouette[, 3])
cat("Средняя ширина силуэта:", round(avg_sil_width, 4), "\n")


plot(final_silhouette, main = "Диаграмма силуэта", col = team_stats$cluster_kmeans + 1, border = NA)

cat("\nФинальная интерпретация\n")

for(i in 1:4) {
  cluster_char <- team_stats %>%
    filter(cluster_kmeans == i) %>%
    summarise(n = n(), win_rate_mean = mean(win_rate), goals_mean = mean(goals_per_match))
  
  cat(sprintf("\nКластер %d (n=%d): win_rate=%.3f, голов=%.2f\n", 
              i, cluster_char$n, cluster_char$win_rate_mean, cluster_char$goals_mean))
}

write.csv(team_stats[, c("team", "total_matches", "win_rate", "goals_per_match", 
                         "goals_conceded_per_match", "goal_diff_per_match", 
                         "neutral_ratio", "cluster_kmeans")], 
          "football_clustering_results.csv", row.names = FALSE)

cat("\nРезультаты сохранены в 'football_clustering_results.csv'\n")
