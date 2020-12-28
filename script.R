install.packages("readxl")
library(readxl)
install.packages("writexl")
library(writexl)
install.packages("ggplot2")
library(ggplot2)
install.packages("lmtest")
library(lmtest)
install.packages("tidyr")
library(tidyr)
install.packages("olsrr")
library(olsrr)

data <- read_xlsx("Traffic Shop.xlsx", skip = 3)
data <- replace_na(data, list(Holidays = 0))
attach(data)

print(cor(data[, c(5, 25, 59, 62:64)]))

print(plot(data[data$Brand != 0,]$Traffic, data[data$Brand != 0,]$Brand))
traffic_radio_trp <- lm(data[data$Brand != 0,]$Traffic ~
                    data[data$Brand != 0,]$Brand)
print(summary(traffic_radio_trp))
print(ols_test_normality(traffic_radio_trp))
print(dwtest(traffic_radio_trp))
print(bptest(traffic_radio_trp))
print(resettest(traffic_radio_trp))

print(plot(data$Traffic,
          data$`Brand Promo`))
traffic_tb_trp <- lm(data$Traffic ~
                    data$`Brand Promo`)
print(summary(traffic_tb_trp))
print(ols_test_normality(traffic_tb_trp))
print(dwtest(traffic_tb_trp))
print(bptest(traffic_tb_trp))
print(resettest(traffic_tb_trp))

print(plot(Traffic, Holidays))
traffic_holidays <- lm(Traffic ~ Holidays)
print(summary(traffic_holidays))
print(ols_test_normality(traffic_holidays))
print(dwtest(traffic_holidays))
print(bptest(traffic_holidays))
print(resettest(traffic_holidays))

print(plot(Traffic, data$`Exchange rate`))
traffic_ex_rate <- lm(Traffic ~ data$`Exchange rate`)
print(summary(traffic_ex_rate))
print(ols_test_normality(traffic_ex_rate))
print(dwtest(traffic_ex_rate))
print(bptest(traffic_ex_rate))
print(resettest(traffic_ex_rate))

print(plot(Traffic, data$`Ave salary`))
traffic_ave_sal <- lm(Traffic ~ data$`Ave salary`)
print(summary(traffic_ave_sal))
print(ols_test_normality(traffic_ave_sal))
print(dwtest(traffic_ave_sal))
print(bptest(traffic_ave_sal))
print(resettest(traffic_ave_sal))

print(cor(data[c(5, 58, 57, 56)]))
traffic_radio_trp <- lm(Traffic ~ data$`Competitors 1...58` + 
                              data$`Competitor 2` +
                              data$`Competitor 3`)
print(summary(traffic_radio_trp))

model <- lm(Traffic ~ Brand + `Brand Promo` +
                  `Ukraine Ave OOH` + Holidays +
                  `Exchange rate` + `Ave salary`)
print(summary(model))