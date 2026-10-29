library(tidyverse)
library(forecast)
library(gridExtra)
library(fastDummies)
library(Metrics)
library(BETS)
library(lmtest)
library(readxl)


# DATASET
df_raw <- read_excel("D:/smt 5/ADW/FP/datadatabf.xlsx")
head(df_raw)

# define calendar variation variable
black_friday_months <- c(
  "2015-11","2016-11","2017-11","2018-11","2019-11","2020-11",
  "2021-11","2022-11", "2023-11", "2024-11", "2025-11"
)
df_raw$black_friday <- as.integer(df_raw$Month %in% black_friday_months)
df_raw

df_raw %>% filter(black_friday==1)

df <- df_raw %>%
  mutate(ts = ym(Month), search = Search, 
         month = month(ts), year = year(ts),
         t = seq_along(ts)) %>%
  dummy_cols(select_columns = "month") %>%
  select(ts, t, black_friday, year, month, starts_with("month_"), search) 
df

ggplot(data = df) +
geom_line(aes(x = ts, y = search)) +
ggtitle("Search Volume Index for keyword Black Friday") +
xlab("") + ylab("") +
scale_x_date(date_breaks = "2 year", date_labels = "%Y") 

train <- df %>% filter(year != 2025)
test  <- df %>% filter(year == 2025)

y_train <- ts(train$search, start = c(2015, 1), frequency = 12)
y_test  <- ts(test$search, start = c(2025, 1), frequency = 12)


# MODELING
# 1) Deterministic Trend & Seasonality (Fourier)
xreg_train1 <- cbind(
  trend = seq_along(y_train),
  seasonal = fourier(y_train, K = 5),                  
  black_friday  = train$black_friday
) %>% as.matrix()

m1 <- auto.arima(y_train, xreg = xreg_train1, seasonal = FALSE)
summary(m1)

# 2) Stochastic Trend & Deterministic Seasonality (Fourier)
xreg_train2 <- cbind(
  seasonal = fourier(y_train, K = 5),           
  black_friday  = train$black_friday
) %>% as.matrix()

m2 <- auto.arima(y_train, xreg = xreg_train2, seasonal = FALSE)
summary(m2)

# 3) Deterministic Trend & Stochastic Seasonality
xreg_train3 <- cbind(
  trend = seq_along(y_train),           
  black_friday  = train$black_friday
) %>% as.matrix()

m3 <- auto.arima(y_train, xreg = xreg_train3)
summary(m3)

# 4) Stochastic Trend & Seasonality
xreg_train4 <- cbind(
  black_friday  = train$black_friday
) %>% as.matrix()

m4 <- auto.arima(y_train, xreg = xreg_train4)
summary(m4)

# 5) TSR with Deterministic Trend
m5 <- lm(search ~ t, data = train)
summary(m5)

# 6) TSR with Deterministic Trend & Seasonality
m6 <- lm(search ~ -1 + t + month_1 + month_2 + month_3 + month_4 + 
           month_5 + month_6 + month_7 + month_8 + 
           month_9 + month_10 + month_11 + month_12, 
         data = train)
summary(m6)

# 7) TSR with Deterministic Trend & Seasonality, and Calendar Variation
m7 <- lm(search ~ -1 + t + month_1 + month_2 + month_3 + month_4 + 
           month_5 + month_6 + month_7 + month_8 + 
           month_9 + month_10 + month_11 + month_12 + black_friday, 
         data = train)
summary(m7)

# 8) TSR with Deterministic Trend & Seasonality, Calendar Variation, and ARMA Error
res <- ts(m7$residuals)
ggAcf(res) + ggtitle("Residual ACF")

res_arima <- auto.arima(res)
summary(res_arima)



# FORECASTING
# 1) Train Set
predict_train <- train %>% 
  mutate(actual = search, 
         m1_predict = m1$fitted,
         m2_predict = m2$fitted,
         m3_predict = m3$fitted,
         m4_predict = m4$fitted,
         m5_predict = m5$fitted.values,
         m6_predict = m6$fitted.values,
         m7_predict = m7$fitted.values,
         m8_predict = m7$fitted.values + res_arima$fitted) %>%
  select(ts, actual, m1_predict, m2_predict, m3_predict, m4_predict, m5_predict, m6_predict, m7_predict, m8_predict) 
predict_train


# Re-define theme (optional, ensuring it's active)
my_theme <- theme(
  legend.position.inside = c(0.2, 0.75), # Updated for newer ggplot2 versions
  legend.text      = element_text(size = 7),
  legend.title     = element_text(size = 11),
  legend.background= element_rect(fill = "white", colour = "grey80"),
  plot.title       = element_text(size = 10, face = "bold")
)

# P1: Deterministic Trend & Seasonality (Fourier)
p1 <- ggplot(data = predict_train) +
  geom_line(aes(x = ts, y = actual,  color = "Actual")) +
  geom_line(aes(x = ts, y = m1_predict, color = "Predicted")) +
  scale_color_manual(name = NULL, values = c("Actual" = "grey", "Predicted" = "blue")) +
  ggtitle("Deterministic Trend & Seasonality (Fourier)") +
  xlab("") + ylab("") +
  scale_x_date(date_breaks = "3 year", date_labels = "%Y") + # Changed to scale_x_date
  my_theme

# P2: Stochastic Trend & Deterministic Seasonality
p2 <- ggplot(data = predict_train) +
  geom_line(aes(x = ts, y = actual,  color = "Actual")) +
  geom_line(aes(x = ts, y = m2_predict, color = "Predicted")) +
  scale_color_manual(name = NULL, values = c("Actual" = "grey", "Predicted" = "purple")) +
  ggtitle("Stochastic Trend & Det. Seasonality") +
  xlab("") + ylab("") +
  scale_x_date(date_breaks = "3 year", date_labels = "%Y") +
  my_theme

# P3: Deterministic Trend & Stochastic Seasonality
p3 <- ggplot(data = predict_train) +
  geom_line(aes(x = ts, y = actual,  color = "Actual")) +
  geom_line(aes(x = ts, y = m3_predict, color = "Predicted")) +
  scale_color_manual(name = NULL, values = c("Actual" = "grey", "Predicted" = "green4")) +
  ggtitle("Det. Trend & Stochastic Seasonality") +
  xlab("") + ylab("") +
  scale_x_date(date_breaks = "3 year", date_labels = "%Y") +
  my_theme

# P4: Stochastic Trend & Seasonality
p4 <- ggplot(data = predict_train) +
  geom_line(aes(x = ts, y = actual,  color = "Actual")) +
  geom_line(aes(x = ts, y = m4_predict, color = "Predicted")) +
  scale_color_manual(name = NULL, values = c("Actual" = "grey", "Predicted" = "orange")) +
  ggtitle("Stochastic Trend & Seasonality") +
  xlab("") + ylab("") +
  scale_x_date(date_breaks = "3 year", date_labels = "%Y") +
  my_theme

# P5: TSR Trend
p5 <- ggplot(data = predict_train) +
  geom_line(aes(x = ts, y = actual,  color = "Actual")) +
  geom_line(aes(x = ts, y = m5_predict, color = "Predicted")) +
  scale_color_manual(name = NULL, values = c("Actual" = "grey", "Predicted" = "red")) +
  ggtitle("Trend Only") +
  xlab("") + ylab("") +
  scale_x_date(date_breaks = "3 year", date_labels = "%Y") +
  my_theme

# P6: TSR Trend + Seasonal
p6 <- ggplot(data = predict_train) +
  geom_line(aes(x = ts, y = actual,  color = "Actual")) +
  geom_line(aes(x = ts, y = m6_predict, color = "Predicted")) +
  scale_color_manual(name = NULL, values = c("Actual" = "grey", "Predicted" = "blue")) +
  ggtitle("Trend + Seasonal") +
  xlab("") + ylab("") +
  scale_x_date(date_breaks = "3 year", date_labels = "%Y") +
  my_theme

# P7: TSR Trend + Seasonal + Calendar
p7 <- ggplot(data = predict_train) +
  geom_line(aes(x = ts, y = actual,  color = "Actual")) +
  geom_line(aes(x = ts, y = m7_predict, color = "Predicted")) +
  scale_color_manual(name = NULL, values = c("Actual" = "grey", "Predicted" = "brown")) +
  ggtitle("Trend + Seasonal + Calendar") +
  xlab("") + ylab("") +
  scale_x_date(date_breaks = "3 year", date_labels = "%Y") +
  my_theme

# P8: TSR Trend + Seasonal + Calendar + ARMA
p8 <- ggplot(data = predict_train) +
  geom_line(aes(x = ts, y = actual,  color = "Actual")) +
  geom_line(aes(x = ts, y = m8_predict, color = "Predicted")) +
  scale_color_manual(name = NULL, values = c("Actual" = "grey", "Predicted" = "purple")) +
  ggtitle("Trend + Seasonal + Calendar + ARMA") +
  xlab("") + ylab("") +
  scale_x_date(date_breaks = "3 year", date_labels = "%Y") +
  my_theme

# Render the Grid
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol=2)


# TEST SET

# Create Fourier terms for the next 12 periods
future_fourier <- fourier(y_train, K = 5, h = 12)

# 1) Future XREG for M1 (Trend + Fourier + Black Friday)
xreg_future <- cbind(
  trend = seq(length(y_train) + 1, length(y_train) + 12),
  seasonal = future_fourier,
  black_friday = test$black_friday
) %>% as.matrix()

# 2) Future XREG for M2 (Fourier + Black Friday)
xreg_future2 <- cbind(
  seasonal = future_fourier,
  black_friday = test$black_friday
) %>% as.matrix()

# 3) Future XREG for M3 (Trend + Black Friday)
xreg_future3 <- cbind(
  trend = seq(length(y_train) + 1, length(y_train) + 12),
  black_friday = test$black_friday
) %>% as.matrix()

# 4) Future XREG for M4 (Black Friday only)
xreg_future4 <- cbind(
  black_friday = test$black_friday
) %>% as.matrix()

predict_test <- test %>% 
  mutate(
    actual = search,
    
    # ARIMA Forecasts (ensure xreg is passed correctly)
    m1_predict = as.numeric(forecast(m1, xreg = xreg_future)$mean),
    m2_predict = as.numeric(forecast(m2, xreg = xreg_future2)$mean),
    m3_predict = as.numeric(forecast(m3, xreg = xreg_future3)$mean),
    m4_predict = as.numeric(forecast(m4, xreg = xreg_future4)$mean),
    
    # Linear Model Forecasts -> USE stats::predict
    m5_predict = stats::predict(m5, newdata = test),
    m6_predict = stats::predict(m6, newdata = test),
    m7_predict = stats::predict(m7, newdata = test),
    
    # Hybrid Model (M7 + ARIMA Residuals)
    m8_predict = m7_predict + as.numeric(forecast(res_arima, h=12)$mean)
  ) %>%
  select(ts, actual, starts_with("m"))

# View the result
print(predict_test)

# Define the theme
my_theme <- theme(
  legend.position.inside = c(0.2, 0.75), 
  legend.text      = element_text(size = 7),
  legend.title     = element_text(size = 11),
  legend.background= element_rect(fill = "white", colour = "grey80"),
  plot.title       = element_text(size = 10, face = "bold")
)

# --- PLOTS P1 to P4 (ARIMA & Fourier) ---

p1 <- ggplot(data = predict_test) +
  geom_line(aes(x = ts, y = actual,  color = "Actual")) +
  geom_line(aes(x = ts, y = m1_predict, color = "Predicted")) +
  scale_color_manual(name = NULL, values = c("Actual" = "grey", "Predicted" = "blue")) +
  ggtitle("Deterministic Trend & Seasonality (Fourier)") +
  xlab("") + ylab("") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") + # Fixed function & breaks
  my_theme

p2 <- ggplot(data = predict_test) +
  geom_line(aes(x = ts, y = actual,  color = "Actual")) +
  geom_line(aes(x = ts, y = m2_predict, color = "Predicted")) +
  scale_color_manual(name = NULL, values = c("Actual" = "grey", "Predicted" = "purple")) +
  ggtitle("Stochastic Trend & Det. Seasonality") +
  xlab("") + ylab("") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") + # Fixed function & breaks
  my_theme

p3 <- ggplot(data = predict_test) +
  geom_line(aes(x = ts, y = actual,  color = "Actual")) +
  geom_line(aes(x = ts, y = m3_predict, color = "Predicted")) +
  scale_color_manual(name = NULL, values = c("Actual" = "grey", "Predicted" = "green4")) +
  ggtitle("Det. Trend & Stochastic Seasonality") +
  xlab("") + ylab("") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") + # Fixed function & breaks
  my_theme

p4 <- ggplot(data = predict_test) +
  geom_line(aes(x = ts, y = actual,  color = "Actual")) +
  geom_line(aes(x = ts, y = m4_predict, color = "Predicted")) +
  scale_color_manual(name = NULL, values = c("Actual" = "grey", "Predicted" = "orange")) +
  ggtitle("Stochastic Trend & Seasonality") +
  xlab("") + ylab("") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") + # Fixed function & breaks
  my_theme

# --- PLOTS P5 to P8 (Linear Regression & Hybrid) ---

p5 <- ggplot(data = predict_test) +
  geom_line(aes(x = ts, y = actual,  color = "Actual")) +
  geom_line(aes(x = ts, y = m5_predict, color = "Predicted")) +
  scale_color_manual(name = NULL, values = c("Actual" = "grey", "Predicted" = "red")) +
  ggtitle("Trend Only") +
  xlab("") + ylab("") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") + # Fixed function
  my_theme

p6 <- ggplot(data = predict_test) +
  geom_line(aes(x = ts, y = actual,  color = "Actual")) +
  geom_line(aes(x = ts, y = m6_predict, color = "Predicted")) +
  scale_color_manual(name = NULL, values = c("Actual" = "grey", "Predicted" = "blue")) +
  ggtitle("Trend + Seasonal") +
  xlab("") + ylab("") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") + # Fixed function
  my_theme

p7 <- ggplot(data = predict_test) +
  geom_line(aes(x = ts, y = actual,  color = "Actual")) +
  geom_line(aes(x = ts, y = m7_predict, color = "Predicted")) +
  scale_color_manual(name = NULL, values = c("Actual" = "grey", "Predicted" = "brown")) +
  ggtitle("Trend + Seasonal + Calendar Var") +
  xlab("") + ylab("") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") + # Fixed function
  my_theme

p8 <- ggplot(data = predict_test) +
  geom_line(aes(x = ts, y = actual,  color = "Actual")) +
  geom_line(aes(x = ts, y = m8_predict, color = "Predicted")) +
  scale_color_manual(name = NULL, values = c("Actual" = "grey", "Predicted" = "purple")) +
  ggtitle("Trend + Seasonal + Calendar + ARMA") +
  xlab("") + ylab("") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") + # Fixed function
  my_theme

# --- RENDER GRID ---
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol=2)


# MODEL EVALUATION
# TRAIN Set
predict_train %>%
  select(ends_with("_predict")) %>%          
  imap_dfr(~ tibble(
    Model = .y,                               
    RMSE  = rmse (predict_train$actual, .x),
    MAPE  = mape (predict_train$actual, .x),
    sMAPE = smape(predict_train$actual, .x)
  )) %>%
  mutate(Model = recode(Model,
                        m1_predict = "Deterministic Trend & Seasonality (Fourier)",
                        m2_predict = "Stochastic Trend & Deterministic Seasonality (Fourier)",
                        m3_predict = "Deterministic Trend & Stochastic Seasonality",
                        m4_predict = "Stochastic Trend & Seasonality",
                        m5_predict = "Trend",
                        m6_predict = "Trend + Seasonal",
                        m7_predict = "Trend + Seasonal + CV",
                        m8_predict = "Trend + Seasonal + CV + ARMA",
  ))

# TEST Set
predict_test %>%
  select(ends_with("_predict")) %>%          
  imap_dfr(~ tibble(
    Model = .y,                               
    RMSE  = rmse (predict_test$actual, .x),
    MAPE  = mape (predict_test$actual, .x),
    sMAPE = smape(predict_test$actual, .x)
  )) %>%
  mutate(Model = recode(Model,
                        m1_predict = "Deterministic Trend & Seasonality (Fourier)",
                        m2_predict = "Stochastic Trend & Deterministic Seasonality (Fourier)",
                        m3_predict = "Deterministic Trend & Stochastic Seasonality",
                        m4_predict = "Stochastic Trend & Seasonality",
                        m5_predict = "Trend",
                        m6_predict = "Trend + Seasonal",
                        m7_predict = "Trend + Seasonal + CV",
                        m8_predict = "Trend + Seasonal + CV + ARMA",
  ))


# Fungsi uji white noise ARIMA
wn_test_arima <- function(model, lag = 24, name = "") {
  test <- Box.test(residuals(model),
                   lag = lag,
                   type = "Ljung-Box",
                   fitdf = length(model$coef))
  
  tibble(
    Model = name,
    Statistic = test$statistic,
    p_value = test$p.value,
    White_Noise = ifelse(test$p.value > 0.05, "YES", "NO")
  )
}


# Fungsi uji white noise ARIMA
wn_test_arima <- function(model, lag = 24, name = "") {
  test <- Box.test(residuals(model),
                   lag = lag,
                   type = "Ljung-Box",
                   fitdf = length(model$coef))
  
  tibble(
    Model = name,
    Statistic = test$statistic,
    p_value = test$p.value,
    White_Noise = ifelse(test$p.value > 0.05, "YES", "NO")
  )
}

# Jalankan uji
wn_arima <- bind_rows(
  wn_test_arima(m1, name = "M1: Det. Trend & Det. Seasonality"),
  wn_test_arima(m2, name = "M2: Stoch. Trend & Det. Seasonality"),
  wn_test_arima(m3, name = "M3: Det. Trend & Stoch. Seasonality"),
  wn_test_arima(m4, name = "M4: Stoch. Trend & Seasonality")
)

print(wn_arima)


ggAcf(residuals(m1)) + ggtitle("Residual ACF – M1")
ggAcf(residuals(m2)) + ggtitle("Residual ACF – M2")
ggAcf(residuals(m3)) + ggtitle("Residual ACF – M3")
ggAcf(residuals(m4)) + ggtitle("Residual ACF – M4")


# Fungsi uji white noise regresi
wn_test_lm <- function(model, lag = 24, name = "") {
  res <- ts(residuals(model), frequency = 12)
  test <- Box.test(res,
                   lag = lag,
                   type = "Ljung-Box")
  
  tibble(
    Model = name,
    Statistic = test$statistic,
    p_value = test$p.value,
    White_Noise = ifelse(test$p.value > 0.05, "YES", "NO")
  )
}

wn_lm <- bind_rows(
  wn_test_lm(m5, name = "M5: Trend"),
  wn_test_lm(m6, name = "M6: Trend + Seasonal"),
  wn_test_lm(m7, name = "M7: Trend + Seasonal + Calendar")
)

print(wn_lm)


ggAcf(ts(residuals(m7), frequency = 12)) +
  ggtitle("Residual ACF – M7")


Box.test(residuals(res_arima),
         lag = 24,
         type = "Ljung-Box",
         fitdf = length(res_arima$coef))


ggAcf(residuals(res_arima)) +
  ggtitle("Residual ACF – ARMA Error (M8)")


bind_rows(wn_arima, wn_lm)


h <- 12

last_date <- max(df$ts)

future_ts <- seq(
  from = last_date %m+% months(1),
  by   = "month",
  length.out = h
)


future_df <- tibble(
  ts = future_ts,
  t  = max(train$t) + seq_len(h),
  month = month(future_ts),
  year  = year(future_ts),
  black_friday = as.integer(format(future_ts, "%m") == "11")
)

future_df <- future_df %>%
  dummy_cols(select_columns = "month")


forecast_m7_future <- stats::predict(m7, newdata = future_df)

res_fc_future <- forecast(res_arima, h = h)

forecast_m8_future <- future_df %>%
  mutate(
    Point_Forecast = forecast_m7_future + as.numeric(res_fc_future$mean),
    Lower_95 = forecast_m7_future + res_fc_future$lower[,2],
    Upper_95 = forecast_m7_future + res_fc_future$upper[,2]
  ) %>%
  select(ts, Point_Forecast, Lower_95, Upper_95)

print(forecast_m8_future)

ggplot() +
  # Data historis
  geom_line(
    data = df,
    aes(x = ts, y = search),
    color = "grey"
  ) +
  
  # Garis forecast (M8)
  geom_line(
    data = forecast_m8_future,
    aes(x = ts, y = Point_Forecast),
    color = "purple",
    linewidth = 1
  ) +
  
  # Interval kepercayaan 95%
  geom_ribbon(
    data = forecast_m8_future,
    aes(x = ts, ymin = Lower_95, ymax = Upper_95),
    fill = "purple",
    alpha = 0.2
  ) +
  
  # Garis batas akhir data aktual
  geom_vline(
    xintercept = max(df$ts),
    linetype = "dashed",
    color = "black"
  ) +
  
  ggtitle("Future Forecast Search Volume – Model 8 (Hybrid TSR + ARMA)") +
  xlab("") + ylab("") +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b %Y"
  ) +
  theme_minimal()
