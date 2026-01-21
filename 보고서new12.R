library(dplyr)
library(readr)
library(tidyr)
library(car)

data <- read_csv("data.csv")

data <- data %>%
  mutate(
    Export85_d = as.numeric(Export85_d),
    Export84_d = as.numeric(Export84_d),
    Export87_d = as.numeric(Export87_d),
    Export85 = as.numeric(Export85),
    Export84 = as.numeric(Export84),
    Export87 = as.numeric(Export87),
    exchange_rate = as.numeric(exchange_rate),
    IPI = as.numeric(IPI)
  )

data$log_Export85 <- log(data$Export85)
data$log_Export84 <- log(data$Export84)
data$log_Export87 <- log(data$Export87)

data <- data %>%
  arrange(time) %>%
  mutate(
    EPU_lag0 = lag(EPU, 0),
    EPU_lag1 = lag(EPU, 1),
    EPU_lag2 = lag(EPU, 2),
    EPU_lag3 = lag(EPU, 3),
    EPU_lag4 = lag(EPU, 4),
    EPU_lag5 = lag(EPU, 5),
    EPU_lag6 = lag(EPU, 6),
    EPU_lag7 = lag(EPU, 7),
    EPU_lag8 = lag(EPU, 8),
    EPU_lag9 = lag(EPU, 9),
    EPU_lag10 = lag(EPU, 10),
    EPU_lag11 = lag(EPU, 11),
    EPU_lag12 = lag(EPU, 12)
  )

data_filtered <- data %>%
  slice(13:108) %>%
  drop_na(log_Export85, log_Export84, log_Export87,
          EPU_lag0, EPU_lag1, EPU_lag2, EPU_lag3, EPU_lag4, EPU_lag5,
          EPU_lag6, EPU_lag7, EPU_lag8, EPU_lag9, EPU_lag10, EPU_lag11,
          EPU_lag12, exchange_rate, IPI)

data_filtered <- data_filtered %>%
  mutate(
    log_EPU_lag0 = log(EPU_lag0),
    log_EPU_lag1 = log(EPU_lag1),
    log_EPU_lag2 = log(EPU_lag2),
    log_EPU_lag3 = log(EPU_lag3),
    log_EPU_lag4 = log(EPU_lag4),
    log_EPU_lag5 = log(EPU_lag5),
    log_EPU_lag6 = log(EPU_lag6),
    log_EPU_lag7 = log(EPU_lag7),
    log_EPU_lag8 = log(EPU_lag8),
    log_EPU_lag9 = log(EPU_lag9),
    log_EPU_lag10 = log(EPU_lag10),
    log_EPU_lag11 = log(EPU_lag11),
    log_EPU_lag12 = log(EPU_lag12)
  )

data_filtered <- data_filtered %>%
  mutate(
    EPU_mean1 = rowMeans(select(., log_EPU_lag1:log_EPU_lag3)),
    EPU_mean2 = rowMeans(select(., log_EPU_lag4:log_EPU_lag6)),
    EPU_mean3 = rowMeans(select(., log_EPU_lag7:log_EPU_lag9)),
    EPU_mean4 = rowMeans(select(., log_EPU_lag10:log_EPU_lag12))
  )

model85_1 <- lm(log_Export87 ~ EPU_mean1 + exchange_rate + IPI, data = data_filtered)
model85_2 <- lm(log_Export87 ~ EPU_mean1 + EPU_mean2 + exchange_rate + IPI, data = data_filtered)
model85_3 <- lm(log_Export87 ~ EPU_mean1 + EPU_mean2 + EPU_mean1*EPU_mean2 + exchange_rate + IPI, data = data_filtered)

summary(model85_1)
summary(model85_2)
summary(model85_3)