library(ggplot2)
library(dplyr)
library(moments)

dataset2018 <- read.csv("D:/JPAL/Reporte R/dataset2018.csv")
WS <- dataset2018$Speed_100m_m.s

min(WS)
max(WS)
mean(WS)
sd(WS)
median(WS)
skewness(WS)
kurtosis(WS)

dataset2018 %>% ggplot(aes(x=Speed_100m_m.s)) +
  geom_histogram(aes(y=..density..), binwidth = 1,
  fill='gray', color = 'white', alpha=1, boundary = 0) +
  xlab("Velocidad de viento [m/s]") +
  ylab("Densidad de probabilidad") +
  theme_bw()

acf.WS <- acf(WS, lag.max = 120, plot = FALSE)

FAC <- data.frame(acf.WS$acf,acf.WS$lag)

FAC %>% ggplot(aes(acf.WS.lag, acf.WS.acf)) +
  geom_line(size = 1) +
  theme_bw() +
  xlab("Retardo [h]") +
  ylab("Autocorrelación") +
  xlim(c(0,120)) +
  ylim(c(0,1))

