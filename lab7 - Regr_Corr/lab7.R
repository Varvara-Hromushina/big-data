df <- read.csv("lab7/Data.csv", header = TRUE, stringsAsFactors = FALSE)

head(df, 10)

usa_raw <- df[df$Country.Code == "USA", ]

get_series <- function(series_code) {
  vals <- usa_raw[usa_raw$Series.Code == series_code, ]
  if(nrow(vals) == 0) return(rep(NA, 14))
  as.numeric(vals[, c("X1989..YR1989.", "X1990..YR1990.", "X1991..YR1991.", 
                      "X2000..YR2000.", "X2001..YR2001.", "X2002..YR2002.",
                      "X2003..YR2003.", "X2004..YR2004.", "X2005..YR2005.",
                      "X2006..YR2006.", "X2007..YR2007.", "X2008..YR2008.",
                      "X2017..YR2017.", "X2018..YR2018.")])
}

years <- c(1989, 1990, 1991, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2017, 2018)

usa_data <- data.frame(
  Year = years,
  GDP_growth = get_series("NY.GDP.MKTP.KD.ZG"),
  Population_growth = get_series("SP.POP.GROW"),
  Unemployment_basic = get_series("SL.UEM.BASC.ZS"),
  Life_exp = get_series("SP.DYN.LE00.IN"),
  Death_rate = get_series("SP.DYN.CDRT.IN"),
  Health_exp = get_series("SH.XPD.GHED.GD.ZS"),
  Bachelors_total = get_series("SE.TER.CUAT.BA.ZS"),
  Bachelors_female = get_series("SE.TER.CUAT.BA.FE.ZS"),
  High_tech_exports = get_series("TX.VAL.TECH.MF.ZS"),
  Journal_articles = get_series("IP.JRN.ARTC.SC")
)

str(usa_data)


# график прироста ВВП
valid_gdp <- !is.na(usa_data$GDP_growth)
plot(usa_data$Year[valid_gdp], usa_data$GDP_growth[valid_gdp], 
     type = "b", col = "blue",
     xlab = "Год", ylab = "Прирост ВВП (%)", 
     main = "Динамика прироста ВВП США")

# Корреляционный анализ
# Пункт a: Рост ВВП и прирост населения
cor_a <- cor(usa_data$GDP_growth, usa_data$Population_growth, use = "complete.obs")
cat("a) Корреляция ВВП и населения:", cor_a, "\n")

plot(usa_data$Population_growth, usa_data$GDP_growth,
     xlab = "Прирост населения (%)", ylab = "Прирост ВВП (%)",
     main = "a) ВВП vs Население", col = "blue", pch = 19)
abline(lm(GDP_growth ~ Population_growth, data = usa_data), col = "red")

# Пункт b: Прирост населения и безработица
cor_b <- cor(usa_data$Population_growth, usa_data$Unemployment_basic, use = "complete.obs")
cat("b) Корреляция населения и безработицы:", cor_b, "\n")

plot(usa_data$Population_growth, usa_data$Unemployment_basic,
     xlab = "Прирост населения (%)", ylab = "Безработица (%)",
     main = "b) Население vs Безработица", col = "darkgreen", pch = 19)

# Пункт c: Расходы на медицину и продолжительность жизни
cor_c1 <- cor(usa_data$Health_exp, usa_data$Life_exp, use = "complete.obs")
cor_c2 <- cor(usa_data$Health_exp, usa_data$Death_rate, use = "complete.obs")
cat("c) Корреляция медицина-жизнь:", cor_c1, "\n")
cat("   Корреляция медицина-смертность:", cor_c2, "\n")

par(mfrow = c(1,2))
plot(usa_data$Health_exp, usa_data$Life_exp,
     xlab = "Расходы на медицину (% ВВП)", ylab = "Продолжительность жизни",
     main = "c1) Медицина - Долголетие", col = "purple", pch = 19)
abline(lm(Life_exp ~ Health_exp, data = usa_data), col = "red")

plot(usa_data$Health_exp, usa_data$Death_rate,
     xlab = "Расходы на медицину (% ВВП)", ylab = "Смертность (на 1000)",
     main = "c2) Медицина - Смертность", col = "orange", pch = 19)
abline(lm(Death_rate ~ Health_exp, data = usa_data), col = "red")
par(mfrow = c(1,1))

# Пункт d: Высшее образование - высокотехнологичный экспорт
cor_d <- cor(usa_data$Bachelors_total, usa_data$High_tech_exports, use = "complete.obs")
cat("d) Корреляция образование-хайтек:", cor_d, "\n")


plot(usa_data$Bachelors_total, usa_data$High_tech_exports,
     xlab = "Население с высшим образованием (%)", 
     ylab = "High-tech экспорт (% от экспорта)",
     main = "d) Образование vs High-tech экспорт", col = "darkred", pch = 19)
abline(lm(High_tech_exports ~ Bachelors_total, data = usa_data), col = "red")

# Пункт e: Расходы на образование - бакалавры среди женщин
usa_data$Education_expenditure <- get_series("SE.XPD.TOTL.GD.ZS")

cat("Расходы на образование (% ВВП) для США:\n")
print(data.frame(Year = usa_data$Year, Education_exp = usa_data$Education_expenditure))

# Пункт f: Высшее образование - статьи в журналах
cor_f <- cor(usa_data$Bachelors_total, usa_data$Journal_articles, use = "complete.obs")
cat("f) Корреляция образование-статьи:", cor_f, "\n")

plot(usa_data$Bachelors_total, usa_data$Journal_articles,
     xlab = "Бакалавры (%)", ylab = "Статьи в журналах",
     main = "f) Образование vs Научные статьи", col = "darkblue", pch = 19)
abline(lm(Journal_articles ~ Bachelors_total, data = usa_data), col = "red")

# Пункт g: Наиболее коррелирующие параметры
cat("\ng) Наиболее сильные корреляции:\n")

cor_matrix <- cor(usa_data[, -1], use = "pairwise.complete.obs")

strong_cors <- which(abs(cor_matrix) > 0.8 & abs(cor_matrix) < 1, arr.ind = TRUE)
for(i in 1:nrow(strong_cors)) {
  r <- strong_cors[i,]
  cat("  ", colnames(cor_matrix)[r[1]], "—", colnames(cor_matrix)[r[2]], 
      ":", round(cor_matrix[r[1], r[2]], 3), "\n")
}



# Регрессионный анализ (пункт h)
# Модель 1: ВВП ~ Образование
model1 <- lm(GDP_growth ~ Bachelors_total, data = usa_data)
summary(model1)

# Модель 2: Статьи ~ Образование
model2 <- lm(Journal_articles ~ Bachelors_total, data = usa_data)
summary(model2)

# Модель 3: Смертность ~ Расходы на медицину
model3 <- lm(Death_rate ~ Health_exp, data = usa_data)
summary(model3)

# Модель 4: Влияние образования на хайтек-экспорт
model4 <- lm(High_tech_exports ~ Bachelors_total, data = usa_data)
summary(model4)


# Прогноз с помощью predict() (пункт i)
# Прогноз количества статей при 40% бакалавров
fit_articles <- lm(Journal_articles ~ Bachelors_total, data = usa_data)
new_data <- data.frame(Bachelors_total = 40)
pred <- predict(fit_articles, new_data, interval = "confidence")
cat("Прогноз статей при 40% бакалавров:", round(pred[1], 0), 
    "\n95% ДИ: от", round(pred[2], 0), "до", round(pred[3], 0), "\n")

plot(usa_data$Bachelors_total, usa_data$Journal_articles,
     xlab = "Бакалавры (%)", ylab = "Статьи", 
     main = "Прогноз научных статей", col = "darkblue", pch = 19, cex = 1.5)

abline(fit_articles, col = "red", lwd = 2)

points(40, pred[1], col = "red", pch = 19, cex = 2)

arrows(40, pred[2], 40, pred[3], code = 3, angle = 90, length = 0.1, col = "red", lwd = 2)

legend("topleft", legend = c("Данные", "Регрессия", "Прогноз 2020"),
       col = c("darkblue", "red", "red"), pch = c(19, NA, 19), lty = c(NA, 1, NA))

