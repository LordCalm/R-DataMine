data <- read.table(
  "ЛР2/л.р.2.csv",
  header = TRUE, sep = ";", fileEncoding = "windows-1251"
)

View(data)

ts_data <- data$вар.2

View(ts_data)

ts_data <- ts(ts_data, frequency = 12, start = c(2007, 1))

# Проверка пропусков
sum(is.na(ts_data))

plot.ts(ts_data)
dev.off()

# Аддитивная
png("ЛР2/plots/01_decompose_add.png", width = 800, height = 600)
d_add <- decompose(ts_data, type = "additive")
plot(d_add)
dev.off()

# Мультипликативная
png("ЛР2/plots/02_decompose_mul.png", width = 800, height = 600)
d_mul <- decompose(ts_data, type = "multiplicative")
plot(d_mul)
dev.off()

# Автокорреляционная и частная автокорреляционная функции
png("ЛР2/plots/03_ACF_PACF.png", width = 800, height = 600)
par(mfrow = c(1, 2))
acf(ts_data, lag.max = 15, main = "ACF исходного ряда")
pacf(ts_data, lag.max = 15, main = "PACF исходного ряда")
par(mfrow = c(1, 1))
dev.off()

# Исследование и модельное описание временного ряда на
# основе метода последовательной идентификации

# Индекс времени t (начинаем с 1)
t <- seq_along(ts_data)
data1 <- data.frame(y = as.numeric(ts_data), t = t)

# Линейная и полиномиальная
fit_lin <- lm(y ~ t, data = data1)
fit_quad <- lm(y ~ t + I(t^2), data = data1)
fit_cub <- lm(y ~ t + I(t^2) + I(t^3), data = data1)

# Модифицированная экспонента
# y = k + a * b ^ t
start_exp <- list(
  k = max(data1$y) * 1.1,
  a = diff(range(data1$y)) / -2, # половина размаха
  b = 0.9
)
fit_exp <- nls(y ~ k + a * b^t,
  data = data1,
  start = start_exp
)

# Логистическая кривая
# y = k / (1 + a * exp(b * t))
k0 <- max(data1$y) * 1.1
fit_log <- nls(y ~ k / (1 + a * exp(b * t)),
  data = data1,
  start = list(k = k0, a = 10, b = -0.5)
)

# Кривая Гомперца
# y = k * exp(a * exp(b * t))
k0 <- max(data1$y) * 1.1
fit_gomp <- nls(y ~ k * exp(a * exp(b * t)),
  data = data1,
  start = list(k = k0, a = -5, b = -0.05)
)

# Получение предсказаний и остатков
y <- data1$y
y_pred_lin <- fitted(fit_lin)
r_lin <- residuals(fit_lin)
y_pred_quad <- fitted(fit_quad)
r_quad <- residuals(fit_quad)
y_pred_cub <- fitted(fit_cub)
r_cub <- residuals(fit_cub)

y_pred_exp <- predict(fit_exp)
r_exp <- residuals(fit_exp)
y_pred_log <- predict(fit_log)
r_log <- residuals(fit_log)
y_pred_gomp <- predict(fit_gomp)
r_gomp <- residuals(fit_gomp)

# Модели и предсказания
model_names <- c(
  "Линейная", "Квадратичная", "Кубическая",
  "Экспонента", "Логистика", "Гомперца"
)
pred_list <- list(
  y_pred_lin, y_pred_quad, y_pred_cub,
  y_pred_exp, y_pred_log, y_pred_gomp
)
residuals_list <- list(
  r_lin, r_quad, r_cub,
  r_exp, r_log, r_gomp
)


png("ЛР2/plots/04_Trends.png", width = 800, height = 600)
plot(ts_data, main = "Исходный ряд и наложенные тренды", ylab = "млн руб.")
lines(ts(y_pred_lin, start = start(ts_data), frequency = 12),
  lwd = 2, col = "gray40"
)
lines(ts(y_pred_quad, start = start(ts_data), frequency = 12),
  lwd = 2, col = "royalblue"
)
lines(ts(y_pred_cub, start = start(ts_data), frequency = 12),
  lwd = 2, col = "magenta"
)
lines(ts(y_pred_exp, start = start(ts_data), frequency = 12),
  lwd = 2, col = "darkorange"
)
lines(ts(y_pred_log, start = start(ts_data), frequency = 12),
  lwd = 2, col = "forestgreen"
)
lines(ts(y_pred_gomp, start = start(ts_data), frequency = 12),
  lwd = 2, col = "firebrick"
)
legend("topleft",
  c("Линейная", "Квадрат.", "Куб.", "Экспонента", "Логистика", "Гомперца"),
  col = c("gray40", "royalblue", "magenta", "darkorange", "forestgreen", "firebrick"),
  lwd = 2, bty = "n"
)
dev.off()

png("ЛР2/plots/05_Trends.png", width = 800, height = 600)
plot(ts_data, main = "Исходный ряд и наложенные тренды", ylab = "млн руб.")
# Тренд из декомпозиции
lines(d_add$trend, col = "navy", lwd = 2)
lines(ts(y_pred_exp, start = start(ts_data), frequency = 12),
  lwd = 2, col = "darkorange"
)
lines(ts(y_pred_log, start = start(ts_data), frequency = 12),
  lwd = 2, col = "forestgreen"
)
lines(ts(y_pred_gomp, start = start(ts_data), frequency = 12),
  lwd = 2, col = "firebrick"
)
legend("topleft",
  c("Тренд (decompose)", "Экспонента", "Логистика", "Гомперца"),
  col = c("navy", "darkorange", "forestgreen", "firebrick"),
  lwd = 2, bty = "n"
)
dev.off()

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

# Сбор метрик
# rbind — склеивает все векторы метрик по строкам.
metrics_table <- do.call(
  rbind,
  lapply(residuals_list, function(p) metrics(p, y))
)
rownames(metrics_table) <- model_names
metrics_df <- as.data.frame(round(metrics_table, 4))

# Вывод
View(metrics_df)

install.packages("forecast")
library(forecast)

acc_table <- do.call(rbind, lapply(pred_list, function(p) accuracy(p, y)))
rownames(acc_table) <- model_names
acc_df <- as.data.frame(round(acc_table, 4))

View(acc_df)


# Периодограмма остатков
png("ЛР2/plots/06_Periodogram_Trends.png", width = 1000, height = 600)
par(mfrow = c(2, 3))
spec.pgram(r_lin,
  detrend = FALSE, log = "no",
  main = "Периодограмма остатков: линейная"
)
spec.pgram(r_quad,
  detrend = FALSE, log = "no",
  main = "Периодограмма остатков: квадр."
)
spec.pgram(r_cub,
  detrend = FALSE, log = "no",
  main = "Периодограмма остатков: куб."
)
spec.pgram(r_exp,
  detrend = FALSE, log = "no",
  main = "Периодограмма остатков: экспонента"
)
spec.pgram(r_log,
  detrend = FALSE, log = "no",
  main = "Периодограмма остатков: логистика"
)
spec.pgram(r_gomp,
  detrend = FALSE, log = "no",
  main = "Периодограмма остатков: Гомперца"
)
par(mfrow = c(1, 1))
dev.off()

# Автокорреляционная и частная автокорреляционная функции
png("ЛР2/plots/07_ACF_PACF_Res.png", width = 800, height = 600)
par(mfrow = c(2, 2))
acf(r_log, main = "ACF остатков: логистика")
pacf(r_log, main = "PACF остатков: логистика")
acf(r_gomp, main = "ACF остатков: Гомперца")
pacf(r_gomp, main = "PACF остатков: Гомперца")
par(mfrow = c(1, 1))
dev.off()

# Идентификация сезонной составляющей временного ряда
# В качестве тренда берём логистическую кривую
# Преобразуем остатки во временной ряд
summary(fit_log)
r_ts <- ts(r_log, frequency = 12, start = c(2007, 1))

# Построение графика остатков
png("ЛР2/plots/08_Res.png", width = 800, height = 600)
plot.ts(r_ts,
  col = "blue", ylab = "Остатки", xlab = "Время",
  main = "Остатки после вычитания тренда"
)
dev.off()

# Периодограмма остатков
png("ЛР2/plots/09_Periodogram_Res.png", width = 1000, height = 600)
spec.pgram(r_ts, detrend = FALSE, log = "no", main = "Периодограмма остатков")
dev.off()

# Создаём таблицу для модели
ts_data_seasonal <- data.frame(y = r_ts, t = 1:length(r_ts))
m_season <- nls(
  y ~ a0
    + a1 * cos(2 * pi * t / 12) + b1 * sin(2 * pi * t / 12)
    + a2 * cos(4 * pi * t / 12) + b2 * sin(4 * pi * t / 12),
  data = ts_data_seasonal,
  start = list(a0 = 0, a1 = 1, b1 = 1, a2 = 1, b2 = 1)
)
summary(m_season)

# Обновлённая сезонная компонента
season <- predict(m_season)
res_noise <- residuals(m_season)

# Модель с сезонной компонентой
model_trend_seasonal <- y_pred_log + season

# График исходного ряда и модели
png("ЛР2/plots/10_TS.png", width = 1000, height = 600)
plot(data1$t, data1$y,
  type = "l", col = "black", ylab = "y", xlab = "t",
  main = "Исходный ряд и Модель (тренд + сезонность)"
)
lines(data1$t, model_trend_seasonal, col = "red", lwd = 2)
legend("topleft",
  legend = c("Исходный ряд", "Модель (тренд + сезонность)"),
  col = c("black", "red"), lty = 1, lwd = 2
)
dev.off()

# Периодограмма остатков, ACF и PACF
png("ЛР2/plots/11_Periodogram_SRes.png", width = 1000, height = 600)
spec.pgram(res_noise,
  detrend = FALSE, log = "no",
  main = "Периодограмма остатков"
)
dev.off()

png("ЛР2/plots/12_ACF_PACF_Res.png", width = 1000, height = 600)
par(mfrow = c(1, 2))
acf(res_noise, main = "ACF остатков Модели (тренд + сезонность)")
pacf(res_noise, main = "PACF остатков Модели (тренд + сезонность)")
par(mfrow = c(1, 1))
dev.off()

# Метрики
View(as.data.frame(round(metrics(res_noise, r_ts), 4)))

# Формальный тест (Ljung-Box) для lag = 12 и 24
Box.test(res_noise, lag = 12, type = "Ljung-Box")
Box.test(res_noise, lag = 24, type = "Ljung-Box")


###############################################################
# Одновременная идентификация

# Модель — тренд (логистика) + сезонность одновременно
model_joint <- nls(
  y ~ k / (1 + a * exp(b * t)) +
    a0 +
    a1 * cos(2 * pi * t / 12) + b1 * sin(2 * pi * t / 12) +
    a2 * cos(4 * pi * t / 12) + b2 * sin(4 * pi * t / 12),
  data = data1,
  start = list(
    k = 176, a = 14, b = -0.1,
    a0 = 0, a1 = -2, b1 = 10, a2 = 1, b2 = 3
  ),
  control = nls.control(maxiter = 500, warnOnly = TRUE)
)

summary(model_joint)

# Прогноз
y_pred_joint <- predict(model_joint)

# Остатки
res_joint <- residuals(model_joint)
res_ts <- ts(res_joint, frequency = 12, start = c(2007, 1))

# График исходного ряда + Модель 3
png("ЛР2/plots/13_TS_Model3.png", width = 1000, height = 600)
plot(data1$t, data1$y,
  type = "l", col = "black", lwd = 1,
  main = "Исходный ряд и Модель 3 (тренд + сезонность одновременные)",
  xlab = "t", ylab = "y"
)
lines(data1$t, y_pred_joint, col = "red", lwd = 2)
legend("topleft",
  col = c("black", "red"), lwd = 2,
  legend = c("Исходный ряд", "Модель 3")
)
dev.off()

# Периодограмма остатков
png("ЛР2/plots/14_Model3_Periodogram.png", width = 1000, height = 600)
spec.pgram(res_ts,
  detrend = FALSE, log = "no",
  main = "Периодограмма остатков Модели 3"
)
dev.off()

# ACF и PACF остатков
png("ЛР2/plots/15_Model3_ACF_PACF.png", width = 1000, height = 600)
par(mfrow = c(1, 2))
acf(res_ts, main = "ACF остатков (Модель 3)")
pacf(res_ts, main = "PACF остатков (Модель 3)")
par(mfrow = c(1, 1))
dev.off()

# Проверка белого шума (тест Льюнга–Бокса)
Box.test(res_ts, lag = 12, type = "Ljung-Box")
Box.test(res_ts, lag = 24, type = "Ljung-Box")

# Метрики точности
# Одновременная идентификация
View(as.data.frame(round(metrics(res_ts, y), 4)))
# Последовательная идентификация
View(as.data.frame(round(metrics(res_noise, y), 4)))



#####################################################
# Метод исторических прогнозов
# Подключаем пакет zoo для корректной работы с датами временных рядов
if (!require(zoo)) {
  install.packages("zoo")
  library(zoo)
}

#' Выполняет историческое прогнозирование (временную кросс-валидацию)
#'
#' @param ts_data_full Полный временной ряд (объект ts)
#' @param start_year_test Год, с которого начинается ТЕСТИРОВАНИЕ (например, 2013)
#' @param n_periods Количество тестовых периодов (например, 10)
#' @param metrics_function Функция для расчета метрик (например, ваша функция metrics)
#' @param start_params Список начальных параметров для nls
#'
#' @return Список, содержащий:
#'   - $forecasts: список с прогнозами (объекты ts) для каждого периода
#'   - $metrics: data.frame с метриками для каждого периода
#'
perform_historical_forecasts <- function(
    ts_data_full,
    start_year_test = 2013,
    n_periods = 10,
    metrics_function,
    start_params) {
  # 1. Определение индексов
  freq <- frequency(ts_data_full)
  start_ts_year <- start(ts_data_full)[1]
  n_total <- length(ts_data_full)

  # Индекс первого наблюдения в тестовом периоде (Январь 2013)
  start_test_index <- (start_year_test - start_ts_year) * freq + 1

  # Индекс последнего наблюдения в начальном обучающем наборе (Декабрь 2012)
  train_end_initial_idx <- start_test_index - 1

  # Количество всех точек для тестирования
  n_test_total <- n_total - train_end_initial_idx

  # Размер одного тестового периода (горизонт прогноза h)
  h <- floor(n_test_total / n_periods)

  if (h == 0) {
    stop("Недостаточно данных для разделения на ", n_periods, " периодов. Уменьшите n_periods.")
  }

  # 2. Инициализация списков для результатов
  forecasts_list <- list()
  metrics_list <- list()
  period_names <- c()

  cat("Начинаем историческое прогнозирование...\n")

  # 3. Цикл по 10 периодам
  for (i in 1:n_periods) {
    # 3.1. Определяем индексы для текущего шага
    # Конец обучающего набора (расширяющееся окно)
    current_train_end_idx <- train_end_initial_idx + (i - 1) * h

    # Начало тестового набора
    current_test_start_idx <- current_train_end_idx + 1

    # Конец тестового набора (последний период забирает все оставшиеся данные)
    current_test_end_idx <- if (i == n_periods) {
      n_total
    } else {
      current_train_end_idx + h
    }

    # Безопасная проверка: если данных больше нет, выходим
    if (current_test_start_idx > n_total) {
      break
    }

    # Конвертируем 'ts' время (число) в 'yearmon' (из zoo) и затем в строку
    start_time_numeric <- time(ts_data_full)[current_test_start_idx]
    end_time_numeric <- time(ts_data_full)[current_test_end_idx]

    # as.yearmon() корректно обработает 2013.0, 2013.083 и т.д.
    start_date_obj <- as.yearmon(start_time_numeric)
    end_date_obj <- as.yearmon(end_time_numeric)

    start_date_str <- format(start_date_obj, "%Y-%m")
    end_date_str <- format(end_date_obj, "%Y-%m")

    period_label <- paste0("Period_", i, " (", start_date_str, " to ", end_date_str, ")")
    period_names <- c(period_names, period_label)

    cat("Обработка:", period_label, "\n")
    cat("  Обучение на индексах 1:", current_train_end_idx, "\n")
    cat("  Тестирование на индексах", current_test_start_idx, ":", current_test_end_idx, "\n")

    # 3.2. Подготовка данных
    train_indices <- 1:current_train_end_idx
    test_indices <- current_test_start_idx:current_test_end_idx

    # Обучающий data.frame (t от 1 до N_train)
    train_df <- data.frame(
      y = as.numeric(ts_data_full[train_indices]),
      t = train_indices
    )

    # Фактические данные для теста
    test_actual_y <- as.numeric(ts_data_full[test_indices])

    # Data.frame для прогноза (t от N_train+1 до N_train+h)
    test_df_new <- data.frame(t = test_indices)

    # 3.3. Обучение модели
    fit_joint <- NULL
    try(
      {
        fit_joint <- nls(
          y ~ k / (1 + a * exp(b * t)) +
            a0 +
            a1 * cos(2 * pi * t / 12) + b1 * sin(2 * pi * t / 12) +
            a2 * cos(4 * pi * t / 12) + b2 * sin(4 * pi * t / 12),
          data = train_df,
          start = start_params, # Используем переданные стартовые параметры
          control = nls.control(maxiter = 500, warnOnly = TRUE)
        )
      },
      silent = TRUE
    )

    # 3.4. Расчет прогноза и метрик
    if (!is.null(fit_joint)) {
      # Прогноз
      pred_y <- predict(fit_joint, newdata = test_df_new)

      # Остатки
      residuals <- test_actual_y - pred_y

      # Метрики
      metrics_raw <- metrics_function(residuals, test_actual_y)

      # Сохранение
      forecasts_list[[period_label]] <- ts(
        pred_y,
        start = time(ts_data_full)[current_test_start_idx],
        frequency = freq
      )
      metrics_list[[period_label]] <- metrics_raw
    } else {
      cat("  ПРЕДУПРЕЖДЕНИЕ: Модель nls не сошлась для периода", period_label, ". Результаты будут NA.\n")
      # Если модель не сошлась, заполняем NA
      metric_names <- names(metrics_function(c(1, 1), c(1, 2))) # Получаем имена метрик

      forecasts_list[[period_label]] <- NA
      metrics_list[[period_label]] <- setNames(rep(NA, length(metric_names)), metric_names)
    }
  }

  # 4. Форматирование результатов
  metrics_table <- do.call(rbind, metrics_list)
  metrics_df <- as.data.frame(metrics_table)

  cat("...Прогнозирование завершено.\n")

  return(list(forecasts = forecasts_list, metrics = metrics_df))
}

start_params_joint <- list(
  k = 176, a = 14, b = -0.1,
  a0 = 0, a1 = -2, b1 = 10, a2 = 1, b2 = 3
)

historical_results <- perform_historical_forecasts(
  ts_data_full = ts_data,
  start_year_test = 2013,
  n_periods = 10,
  metrics_function = metrics,
  start_params = start_params_joint
)

View(historical_results$metrics)

# Прогнозы
png("ЛР2/plots/16_Valid.png", width = 1000, height = 600)
# Для n_period периода
n_period <- 9
if (length(historical_results$forecasts) > 0 && !is.na(historical_results$forecasts[1])) {
  # Получаем название первого периода
  first_period_name <- names(historical_results$forecasts)[n_period]

  # Получаем сам прогноз
  forecast_ts <- historical_results$forecasts[[n_period]]

  # Получаем фактические данные для этого периода
  actual_ts <- window(
    ts_data,
    start = start(forecast_ts),
    end = end(forecast_ts)
  )

  # График
  plot(
    actual_ts,
    main = paste("Прогноз vs Факт:", first_period_name),
    ylab = "Значение",
    col = "black",
    lwd = 2
  )
  lines(forecast_ts, col = "red", lwd = 2, lty = 2)
  legend(
    "topleft",
    legend = c("Факт", "Прогноз"),
    col = c("black", "red"),
    lwd = 2,
    lty = c(1, 2)
  )
}
dev.off()
