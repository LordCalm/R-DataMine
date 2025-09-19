data <- read.table(
  "c:/Users/ASUS/Documents/НГТУ/КТАиОД/ЛР1/вариант-2(л.р.1).csv",
  header = TRUE, sep = ";", fileEncoding = "windows-1251"
)

View(data)

# Замена имён столбцов
names(data) <- c(
  "№",
  "группа",
  "пол",
  "возраст",
  "стаж",
  "выполнение",
  "ошибки",
  "оценка_баллы",
  "оценка_качество",
  "документация"
)

# Замена типа данных столбцов
cols <- c("пол", "группа", "документация", "оценка_качество")
data[cols] <- lapply(data[cols], as.factor)


# Доступ к столбцам
View(data$возраст) # получить весь столбец "возраст"
View(data[["ошибки"]])
View(data[, "стаж"])


# Доступ к строкам
View(data[1, ]) # первая строка
View(data[10, "пол"]) # значение в 10-й строке в столбце "пол"
# строки с 5 по 7, только выбранные столбцы
View(data[5:7, c("возраст", "стаж")])


# Редактирование данных
View(data$возраст[1])
data$возраст[1] <- 30


# Подвыборки по условиям
# все строки, где пол = 1
subset1 <- data[data$пол == 1, ]
# сотрудники старше 30 лет
subset2 <- data[data$возраст > 30, ]
# стаж > 5 лет и ошибок < 10
subset3 <- data[data$ошибки < 10 & data$стаж > 5, ]
# с помощью функции subset()
subset4 <- subset(data, группа == 1 & выполнение > 90)
View(subset4)


# Расчёт основных статистических характеристик
# Структура данных
str(data)
View(summary(data))

# Функция для вычисления статистических характеристик
mystats <- function(x) {
  if (is.numeric(x)) {
    n <- length(x)
    m <- mean(x, na.rm = TRUE)
    s <- sd(x, na.rm = TRUE)
    med <- median(x, na.rm = TRUE)
    q <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
    minv <- min(x, na.rm = TRUE)
    maxv <- max(x, na.rm = TRUE)

    # мода
    ux <- unique(x)
    modev <- ux[which.max(tabulate(match(x, ux)))]

    # асимметрия и эксцесс
    skew <- sum((x - m)^3 / s^3) / n
    kurt <- sum((x - m)^4 / s^4) / n - 3

    c(
      n = n, min = minv, max = maxv,
      mean = m, median = med,
      q1 = q[1], q3 = q[2],
      stdev = s, mode = modev,
      skewness = skew, kurtosis = kurt
    )
  } else {
    NA
  }
}

# Для всей выборки
all_stats <- lapply(data[sapply(data, is.numeric)], mystats)
all_stats

install.packages("psych")
library(psych)
sm <- describe(data)
View(sm)

# Для группы 1
group1_stats <- lapply(
  data[data$группа == 1, sapply(data, is.numeric)],
  mystats
)
group1_stats

# Для группы 2
group2_stats <- lapply(
  data[data$группа == 2, sapply(data, is.numeric)],
  mystats
)
group2_stats


# Графический анализ данных

# 1. Диаграмма рассеяния
png("plots/01_scatter_age_errors.png", width = 800, height = 600)
plot(data$возраст, data$ошибки,
  type = "p", col = "blue", pch = 16,
  main = "Диаграмма рассеяния",
  xlab = "Возраст", ylab = "Ошибки"
)
dev.off()

# 2. Круговая диаграмма
png("plots/02_pie_gender.png", width = 800, height = 600)
x <- table(data$оценка_качество)
piepercent <- round(100 * x / sum(x), 1)
pie(x,
  labels = paste0(names(x), " (", piepercent, "%)"),
  col = c("red", "purple", "blue"),
  main = "Распределение оценок",
  clockwise = TRUE
)
legend("topright",
  legend = names(x),
  fill = c("red", "purple", "blue"), cex = 0.8
)
dev.off()

# 3. Круговая диаграмма (документация по полу и группе)
# Категориальная радиальная диаграмма (матрица 2х2)
png("plots/03_radial_docs_gender_group.png", width = 1000, height = 1000)
par(mfrow = c(2, 2)) # 2 строки, 2 столбца
for (sex in unique(data$пол)) {
  for (grp in unique(data$группа)) {
    subset_data <- data[data$пол == sex & data$группа == grp, ]
    tbl <- table(subset_data$документация)
    pie(tbl,
      main = paste("Документация\nПол:", sex, "Группа:", grp),
      col = rainbow(length(tbl))
    )
  }
}
dev.off()

# 4. Столбиковые диаграммы (оценка по полу и группе)
png("plots/04_bar_means.png", width = 1000, height = 600)
par(mfrow = c(1, 2))
barplot(tapply(data$оценка_баллы, data$пол, mean),
  col = c("red", "blue"),
  main = "Средняя оценка по полу", ylab = "Средний балл"
)
barplot(tapply(data$оценка_баллы, data$группа, mean),
  col = c("green", "orange"),
  main = "Средняя оценка по группе", ylab = "Средний балл"
)
dev.off()

# 5. Диаграмма размаха
png("plots/05_boxplot_errors_gender.png", width = 800, height = 600)
boxplot(ошибки ~ пол,
  data = data,
  col = c("red", "blue"),
  main = "Диаграмма размаха ошибок по полу",
  xlab = "Пол", ylab = "Ошибки"
)
dev.off()
png("plots/05_boxplot_errors_grp.png", width = 800, height = 600)
boxplot(ошибки ~ группа,
  data = data,
  col = c("red", "blue"),
  main = "Диаграмма размаха ошибок по группе",
  xlab = "Группа", ylab = "Ошибки"
)
dev.off()

# 6. Гистограммы для всех количественных признаков
numeric_cols <- data[, unlist(lapply(data, is.numeric))]
for (colname in names(numeric_cols)) {
  png(paste0("plots/06_hist_", colname, ".png"), width = 800, height = 600)
  hist(numeric_cols[[colname]],
    main = paste("Гистограмма:", colname),
    xlab = colname,
    col = "lightblue", border = "black"
  )
  dev.off()
}

# 7. Матричный график
png("plots/07_pairs_numeric.png", width = 1000, height = 1000)
pairs(numeric_cols,
  main = "Матричный график количественных признаков",
  col = "darkgreen", pch = 16
)
dev.off()

library(GGally)
library(ggplot2)
png("plots/09_ggpairs.png", width = 1000, height = 1000)
ggpairs(data, columns = 1:5, aes(color = группа, alpha = 0.5))
dev.off()


# Для аналитической проверки гипотезы о соответствии нормальному
# закону распределения могут быть применены критерии согласия:
# Шапиро – Уилка, Крамера – Мизеса, Андерсона – Дарлинга
install.packages("nortest")
library(nortest)

# Для всей выборки
# Тест Шапиро–Уилка
shapiro.test(data$ошибки)

# Тест Крамера–Мизеса
cvm.test(data$ошибки)

# Тест Андерсона–Дарлинга
ad.test(data$ошибки)

# Для 1 группы
# Тест Шапиро–Уилка
shapiro.test(data[data$группа == 1, "ошибки"])

# Тест Крамера–Мизеса
cvm.test(data[data$группа == 1, "ошибки"])

# Тест Андерсона–Дарлинга
ad.test(data[data$группа == 1, "ошибки"])

# Для 2 группы
# Тест Шапиро–Уилка
shapiro.test(data[data$группа == 2, "ошибки"])

# Тест Крамера–Мизеса
cvm.test(data[data$группа == 2, "ошибки"])

# Тест Андерсона–Дарлинга
ad.test(data[data$группа == 2, "ошибки"])


# Корреляционный анализ данных
# Для анализа связей между качественными признаками применяются разные тесты.
# Наиболее распространён — χ² (Chi-квадрат) тест Пирсона.
# Он проверяет гипотезу об отсутствии связи между двумя
# категориальными переменными.
factor_cols <- names(data[, unlist(lapply(data, is.factor))])
factor_cols_grp <- factor_cols[factor_cols != "группа"]
pairs <- combn(factor_cols_grp, 2, simplify = FALSE)

# Для 1 группы
for (pair in pairs) {
  tbl <- table(data[data$группа == 1, pair[1]], data[data$группа == 1, pair[2]])

  cat("\nПара:", pair[1], "-", pair[2], "\n")
  # Тест Пирсона
  print(chisq.test(tbl))
  # Критерий Фишера (только если таблица 2х2)
  if (all(dim(tbl) == c(2, 2))) {
    print(fisher.test(tbl))
  }
}

# Для 2 группы
for (pair in pairs) {
  tbl <- table(data[data$группа == 2, pair[1]], data[data$группа == 2, pair[2]])

  cat("\nПара:", pair[1], "-", pair[2], "\n")
  # Тест Пирсона
  print(chisq.test(tbl))
  # Критерий Фишера (только если таблица 2х2)
  if (all(dim(tbl) == c(2, 2))) {
    print(fisher.test(tbl))
  }
}

# Связь между группой и остальными признаками
for (col in factor_cols_grp) {
  tbl <- table(data$группа, data[[col]])

  cat("\nПара: группа -", col, "\n")
  # Тест Пирсона
  print(chisq.test(tbl))
  # Критерий Фишера (только если таблица 2х2)
  if (all(dim(tbl) == c(2, 2))) {
    print(fisher.test(tbl))
  }
}


# Для оценки связи между качественной (факторной) и количественной переменной,
# при условии нормального распределения количественной переменной,
# применяется однофакторный дисперсионный анализ (ANOVA).

# Для оценки связи между качественной (факторной) и количественной переменной,
# если распределение количественной переменной не является нормальным,
# применяется критерий Краскела–Уоллиса (Kruskal-Wallis test).

num_cols <- names(numeric_cols)
for (col in num_cols) {
  cat("Переменная:", col, "\n")

  # ANOVA
  anova_model <- aov(data[[col]] ~ data$оценка_качество)
  cat("ANOVA:\n")
  print(summary(anova_model))

  # Критерий Краскела-Уоллиса
  kruskal_res <- kruskal.test(data[[col]] ~ data$оценка_качество)
  cat("Краскел–Уоллис:\n")
  print(kruskal_res)
}


# Линейный коэффициент корреляции Пирсона (Pearson) показывает степень линейной
# связи между двумя количественными переменными.
# Коэффициент ранговой корреляции Спирмана (Spearman) измеряет связь между
# ранжированными переменными.
# Тау Кендалла (Kendall's Tau) также является непараметрическим показателем
# ранговой корреляции.



# Для 1 группы
M1 <- numeric_cols[data$группа == 1, ]
cor_pearson_1 <- cor(M1, use = "pairwise.complete.obs", method = "pearson")
cor_spearman_1 <- cor(M1, use = "pairwise.complete.obs", method = "spearman")
cor_kendall_1 <- cor(M1, use = "pairwise.complete.obs", method = "kendall")

cat("\nКорреляции (группа 1)\n")
print(cor_pearson_1)
print(cor_spearman_1)
print(cor_kendall_1)

# Для 2 группы
M2 <- numeric_cols[data$группа == 2, ]
cor_pearson_2 <- cor(M2, use = "pairwise.complete.obs", method = "pearson")
cor_spearman_2 <- cor(M2, use = "pairwise.complete.obs", method = "spearman")
cor_kendall_2 <- cor(M2, use = "pairwise.complete.obs", method = "kendall")

cat("\nКорреляции (группа 2)\n")
print(cor_pearson_2)
print(cor_spearman_2)
print(cor_kendall_2)


# Для вычисления коэффициентов частной корреляции можно
# использовать функцию pcor() из пакета ggm
install.packages("ggm")
library(ggm)
if (!require(BiocManager)) install.packages("BiocManager")
BiocManager::install("graph")

# Группа 1
M1 <- numeric_cols[data$группа == 1, ]
S1 <- cov(M1)
# частная корреляция между "ошибки"(5) и "оценка_баллы"(6),
# контролируя остальные переменные: №(1), возраст(2), стаж(3), выполнение(4)
pcor_1 <- ggm::pcor(c(5, 6, 1, 2, 3, 4), S1)
cat("Частная корреляция (группа 1):", pcor_1, "\n")

# Группа 2
M2 <- numeric_cols[data$группа == 2, ]
S2 <- cov(M2)
pcor_2 <- ggm::pcor(c(5, 6, 1, 2, 3, 4), S2)
cat("Частная корреляция (группа 2):", pcor_2, "\n")


# Для графического представления степени взаимосвязей между
# признаками необходимо установить пакет corrplot:
install.packages("corrplot")
library(corrplot)

png("plots/08_corrplot_2grp.png", width = 1000, height = 1000)
col <- colorRampPalette(c(
  "#BB4444", "#EE9988", "#FFFFFF", "#77AADD",
  "#4477AA"
))
corrplot(cor_pearson_2,
  method = "color", col = NULL,
  type = "upper", order = "hclust",
  addCoef.col = "black", tl.col = "black", tl.srt = 45,
  sig.level = 0.01, insig = "blank",
  diag = FALSE
)
dev.off()

cor.test(data$оценка_баллы, data$ошибки)
