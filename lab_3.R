# ==============================================================================
# Лабораторная работа №3. Временные ряды: Часть 2.
# Тема: Модели экспоненциального сглаживания и ARIMA
# ==============================================================================

# 1. Установка и подключение библиотек
if (!require("tseries")) install.packages("tseries")
if (!require("forecast")) install.packages("forecast")
if (!require("lmtest")) install.packages("lmtest") # Для adf.test, Box.test
library(tseries)
library(forecast)
library(lmtest) 
library(graphics) # Для dev.off и png

data <- read.table(
  "л.р.2.csv",
  header = TRUE, sep = ";", fileEncoding = "windows-1251"
)

View(data)

ts_data <- data$вар.2

ts_data <- ts(ts_data, frequency = 12, start = c(2007, 1))

# Разделение на обучающую и тестовую выборки
train_end_idx <- 180
ts_train <- subset(ts_data, end = train_end_idx)
ts_test <- subset(ts_data, start = train_end_idx + 1)

cat("Длина обучающей выборки:", length(ts_train), "\n")
cat("Длина тестовой выборки:", length(ts_test), "\n")

# Метрики
metrics <- function(residuals, y) {
  # Базовые ошибки
  min_err <- min(residuals) # Минимальный остаток
  max_err <- max(residuals) # Максимальный остаток
  mean_err <- mean(residuals) # Средняя ошибка
  sd_err <- sd(residuals) # СКО ошибки
  mae <- mean(abs(residuals)) # Средняя абсолютная ошибка
  mape <- mean(residuals / y) * 100 # средняя ошибка в %
  mape_abs <- mean(abs(residuals / y)) * 100 # средняя абсолютная ошибка в %
  mse <- mean(residuals^2)
  rmse <- sqrt(mse) # Средний квадрат ошибки

  # Коэффициент детерминации R^2
  ss_res <- sum(residuals^2)
  ss_tot <- sum((y - mean(y))^2)
  r2 <- 1 - ss_res / ss_tot
  dc2 <- sum((y - residuals - mean(y))^2) / sum((y - mean(y))^2)

  c(
    MinError = min_err,
    MaxError = max_err,
    MeanError = mean_err,
    StdDevError = sd_err,
    MAE = mae,
    MPE = mape,
    MAPE = mape_abs,
    RMSE = rmse,
    R2 = r2,
    DC2 = dc2
  )
}

# Имена метрик
metric_names <- c(
  "MinError", "MaxError", "MeanError", "StdDevError",
  "MAE", "MPE", "MAPE", "RMSE", "R2", "DC2"
)

# Инициализация пустых таблиц с колонками
results_test_table <- data.frame(matrix(ncol = length(metric_names), nrow = 0))
colnames(results_test_table) <- metric_names

results_train_table <- data.frame(matrix(ncol = length(metric_names), nrow = 0))
colnames(results_train_table) <- metric_names

add_metrics_row <- function(table, metrics_vec, model_name) {
  # Превращаем метрики в строку данных
  df <- as.data.frame(t(metrics_vec)) 
  # Добавляем имя модели как первый столбец
  df_row <- cbind(Model = model_name, df)
  # Используем rbind для добавления строки к таблице
  return(rbind(table, df_row))
}

# ==============================================================================
# Модель Хольта-Винтерса
# ==============================================================================

# R автоматически подберет alpha, beta, gamma
hw_model <- HoltWinters(ts_train)

# Вывод параметров модели (alpha, beta, gamma)
View(hw_model)

res_hw_train <- residuals(hw_model)

metrics_hw_model_train <- metrics(residuals = res_hw_train, y = ts_train)
results_train_table <- add_metrics_row(results_train_table, metrics_hw_model_train, "Holt-Winters (Train)")

View(metrics_hw_model_train)

# График исходного ВР с наложением модельного ВР
png("plots/HW_Residuals.png", width = 1000, height = 600)
plot.ts(ts_train, main = "Модель Хольта-Винтерса")
lines(ts_train - res_hw_train, col = "blue")
legend(
  "topleft",
  legend = c("Факт", "Модель"),
  col = c("black", "blue"),
  lty = c(1, 1)
)
dev.off()

# Прогноз на 12 месяцев
hw_forecast <- forecast(hw_model, h = 12)

metrics_hw_model_test <- metrics(
  residuals = as.numeric(ts_test) - as.numeric(hw_forecast$mean),
  y = as.numeric(ts_test)
)
results_test_table <- add_metrics_row(results_test_table, metrics_hw_model_test, "Holt-Winters (Test)")

View(metrics_hw_model_test)

# График тестового ВР с наложением модельного ВР
png("plots/HW_Test.png", width = 1000, height = 600)
plot.ts(ts_test, main = "Модель Хольта-Винтерса")
lines(hw_forecast$mean, col = "blue")
legend(
  "topleft",
  legend = c("Факт", "Модель"),
  col = c("black", "blue"),
  lty = c(1, 1)
)
dev.off()

# Периодограмма
png("plots/HW_Res_Periodogram.png", width = 800, height = 600)
spec.pgram(res_hw_train, detrend = FALSE, log = "no", main = "Периодограмма остатков (Holt-Winters)")
dev.off()

# ACF и PACF
png("plots/HW_Res_ACF_PACF.png", width = 800, height = 600)
par(mfrow = c(1, 2))
Acf(res_hw_train, main = "ACF остатков (HW)")
Pacf(res_hw_train, main = "PACF остатков (HW)")
par(mfrow = c(1, 1))
dev.off()

# Проверка белого шума (тест Льюнга–Бокса)
Box.test(res_hw_train, lag = 12, type = "Ljung-Box")
Box.test(res_hw_train, lag = 24, type = "Ljung-Box")


# ==============================================================================
# Модель ARIMA
# ==============================================================================

# Методология Бокса-Дженкинса

adf.test(ts_train, k = 5)
ts_diff_1 <- diff(ts_train, lag = 1, differences = 1)

png("plots/ARIMA_diff_1.png", width = 800, height = 600)
plot.ts(ts_diff_1)
dev.off()

adf.test(ts_diff_1)

# Периодограмма
png("plots/ARIMA_diff_1_Periodogram.png", width = 800, height = 600)
spec.pgram(ts_diff_1, detrend = FALSE, log = "no", main = "Периодограмма остатков (ARIMA)")
dev.off()

# ACF и PACF
png("plots/ARIMA_diff_1_ACF_PACF.png", width = 800, height = 600)
par(mfrow = c(1, 2))
Acf(ts_diff_1, main = "ACF остатков (ARIMA)")
Pacf(ts_diff_1, main = "PACF остатков (ARIMA)")
par(mfrow = c(1, 1))
dev.off()

ts_diff_12 <- diff(ts_diff_1, lag = 12, differences = 1)

png("plots/ARIMA_diff_12.png", width = 800, height = 600)
plot.ts(ts_diff_12)
dev.off()

# Периодограмма
png("plots/ARIMA_diff_12_Periodogram.png", width = 800, height = 600)
spec.pgram(ts_diff_12, detrend = FALSE, log = "no", main = "Периодограмма остатков (ARIMA)")
dev.off()

# ACF и PACF
png("plots/ARIMA_diff_12_ACF_PACF.png", width = 800, height = 600)
par(mfrow = c(1, 2))
Acf(ts_diff_12, main = "ACF остатков (ARIMA)")
Pacf(ts_diff_12, main = "PACF остатков (ARIMA)")
par(mfrow = c(1, 1))
dev.off()

arima_bd_model <- Arima(ts_train, order = c(2, 1, 1), seasonal = c(1, 1, 3))
print(arima_bd_model)

arima_bd_fitted <- fitted(arima_bd_model)
res_arima_bd_train <- residuals(arima_bd_model)

metrics_arima_bd_model_train <- metrics(
  residuals = res_arima_bd_train,
  y = ts_train
)
results_train_table <- add_metrics_row(results_train_table, metrics_arima_bd_model_train, "ARIMA (BD) (Train)")

# График исходного ВР с наложением модельного ВР
png("plots/ARIMA_BD_Residuals.png", width = 1000, height = 600)
plot.ts(ts_train, main = "Модель ARIMA")
lines(arima_bd_fitted, col = "blue")
legend(
  "topleft",
  legend = c("Факт", "Модель"),
  col = c("black", "blue"),
  lty = c(1, 1)
)
dev.off()

# Прогноз
arima_bd_forecast <- forecast(arima_bd_model, h = 12)

metrics_arima_bd_model_test <- metrics(
  residuals = as.numeric(ts_test) - as.numeric(arima_bd_forecast$mean),
  y = as.numeric(ts_test)
)
results_test_table <- add_metrics_row(results_test_table, metrics_arima_bd_model_test, "ARIMA (BD) (Test)")

# График тестового ВР с наложением модельного ВР
png("plots/ARIMA_BD_Test.png", width = 1000, height = 600)
plot.ts(ts_test, main = "Модель ARIMA")
lines(arima_bd_forecast$mean, col = "blue")
legend(
  "topleft",
  legend = c("Факт", "Модель"),
  col = c("black", "blue"),
  lty = c(1, 1)
)
dev.off()

# Периодограмма
png("plots/ARIMA_BD_Res_Periodogram.png", width = 800, height = 600)
spec.pgram(res_arima_bd_train, detrend = FALSE, log = "no", main = "Периодограмма остатков (ARIMA)")
dev.off()

# ACF и PACF
png("plots/ARIMA_BD_Res_ACF_PACF.png", width = 800, height = 600)
par(mfrow = c(1, 2))
Acf(res_arima_bd_train, main = "ACF остатков (ARIMA)")
Pacf(res_arima_bd_train, main = "PACF остатков (ARIMA)")
par(mfrow = c(1, 1))
dev.off()

# Проверка белого шума (тест Льюнга–Бокса)
Box.test(res_arima_bd_train, lag = 12, type = "Ljung-Box")
Box.test(res_arima_bd_train, lag = 24, type = "Ljung-Box")

# Используем auto.arima для автоматического подбора параметров (p,d,q)(P,D,Q)
# stepwise=FALSE и approximation=FALSE дают более точный поиск, но работают дольше
arima_model <- auto.arima(ts_train, stepwise = FALSE, approximation = FALSE)

# Вывод параметров модели
print(arima_model)

arima_fitted <- fitted(arima_model)
res_arima_train <- residuals(arima_model)

metrics_arima_model_train <- metrics(
  residuals = res_arima_train,
  y = ts_train
)
results_train_table <- add_metrics_row(results_train_table, metrics_arima_model_train, "ARIMA (Auto) (Train)")

# График исходного ВР с наложением модельного ВР
png("plots/ARIMA_Residuals.png", width = 1000, height = 600)
plot.ts(ts_train, main = "Модель ARIMA")
lines(arima_fitted, col = "blue")
legend(
  "topleft",
  legend = c("Факт", "Модель"),
  col = c("black", "blue"),
  lty = c(1, 1)
)
dev.off()

# Прогноз
arima_forecast <- forecast(arima_model, h = 12)

metrics_arima_model_test <- metrics(
  residuals = as.numeric(ts_test) - as.numeric(arima_forecast$mean),
  y = as.numeric(ts_test)
)
results_test_table <- add_metrics_row(results_test_table, metrics_arima_model_test, "ARIMA (Auto) (Test)")

# График тестового ВР с наложением модельного ВР
png("plots/ARIMA_Test.png", width = 1000, height = 600)
plot.ts(ts_test, main = "Модель ARIMA")
lines(arima_forecast$mean, col = "blue")
legend(
  "topleft",
  legend = c("Факт", "Модель"),
  col = c("black", "blue"),
  lty = c(1, 1)
)
dev.off()

# Периодограмма
png("plots/ARIMA_Res_Periodogram.png", width = 800, height = 600)
spec.pgram(res_arima_train, detrend = FALSE, log = "no", main = "Периодограмма остатков (ARIMA)")
dev.off()

# ACF и PACF
png("plots/ARIMA_Res_ACF_PACF.png", width = 800, height = 600)
par(mfrow = c(1, 2))
Acf(res_arima_train, main = "ACF остатков (ARIMA)")
Pacf(res_arima_train, main = "PACF остатков (ARIMA)")
par(mfrow = c(1, 1))
dev.off()

# Проверка белого шума (тест Льюнга–Бокса)
Box.test(res_arima_train, lag = 12, type = "Ljung-Box")
Box.test(res_arima_train, lag = 24, type = "Ljung-Box")


# ==============================================================================
# Вывод результатов и Сравнение
# ==============================================================================

# Вывод сводных таблиц в консоль
View(results_test_table)
View(results_train_table)

# Сохранение таблиц в CSV
write.csv(results_test_table, "metrics_test_comparison.csv", row.names = FALSE)
write.csv(results_train_table, "metrics_train_comparison.csv", row.names = FALSE)


# ==============================================================================
# Метод исторических прогнозов
library(zoo) # Эта библиотека нужна для корректного форматирования даты
historical_forecasts_arima <- function(
    ts_data_full,
    start_year_test = 2013,
    n_periods = 10,
    metrics_function) {
  
  freq <- frequency(ts_data_full)
  start_ts_year <- start(ts_data_full)[1]
  n_total <- length(ts_data_full)
  
  # Индекс начала теста (Январь 2013)
  start_test_index <- (start_year_test - start_ts_year) * freq + 1
  train_end_initial_idx <- start_test_index - 1
  
  n_test_total <- n_total - train_end_initial_idx
  
  # Горизонт прогноза h для одного периода
  h <- floor(n_test_total / n_periods)
  
  if (h == 0) stop("Недостаточно данных для 10 периодов. Уменьшите n_periods.")
  
  forecasts_list <- list()
  metrics_list <- list()
  models_list <- list()
  
  cat("Начинаем историческое прогнозирование ARIMA (", n_periods, " периодов)...\n")
  cat("Горизонт прогнозирования (h) на период:", h, "месяцев.\n")
  
  for (i in 1:n_periods) {
    
    current_train_end_idx <- train_end_initial_idx + (i - 1) * h
    current_test_start_idx <- current_train_end_idx + 1
    
    current_test_end_idx <- if (i == n_periods) n_total else (current_train_end_idx + h)
    
    if (current_test_start_idx > n_total) break
    
    period_label <- paste0("Период_", i)
    
    # 1. Подготовка TS объектов
    train_ts <- window(ts_data_full, end = time(ts_data_full)[current_train_end_idx])
    test_actual <- window(ts_data_full, 
                          start = time(ts_data_full)[current_test_start_idx], 
                          end = time(ts_data_full)[current_test_end_idx])
    
    h_current <- length(test_actual)
    
    # Получаем конечную дату в формате "Год Месяц" (например, "Дек 2012")
    # и преобразуем ее в простую строку.
    date_label <- as.character(zoo::as.yearmon(time(train_ts))[length(train_ts)])
    
    cat("  ", period_label, ": Обучение до", date_label, 
        "| Прогноз на", h_current, "мес.\n")
    # ----------------------------------------------------------------------
    
    # 2. Построение модели ARIMA
    fit_arima <- auto.arima(train_ts, stepwise = FALSE, approximation = FALSE)
    model_desc <- as.character(fit_arima)
    
    # 3. Прогнозирование
    fc_obj <- forecast(fit_arima, h = h_current)
    pred_values <- fc_obj$mean
    
    # 4. Расчет метрик
    residuals_vec <- as.numeric(test_actual) - as.numeric(pred_values)
    metrics_raw <- metrics_function(residuals_vec, as.numeric(test_actual))
    
    # 5. Сохранение результатов
    metrics_list[[period_label]] <- c(metrics_raw, Model_Desc = model_desc)
  }
  
  metrics_df <- do.call(add_metrics_row, metrics_list)
  metrics_df <- as.data.frame(metrics_df)
  
  cat("...Прогнозирование завершено.\n")
  return(list(forecasts = forecasts_list, metrics = metrics_df, models = models_list))
}

historical_results <- historical_forecasts_arima(
  ts_data_full = ts_data,
  start_year_test = 2013,
  n_periods = 10,
  metrics_function = metrics
)

View(historical_results$metrics)
