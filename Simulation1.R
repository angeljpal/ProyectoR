set.seed(5)
t = 8760
tray = 1
delta_t = 1
theta = sqrt(2*alpha)
n = t/delta_t

X=matrix(ncol = tray , nrow = n)
Y=matrix(ncol = tray , nrow = n)

for(i in 1:tray) {
X_0 <- rnorm(1)
dW <- sqrt(delta_t)*rnorm(n)
for(j in 1:n) { 
X[j,i] <- X_0 - alpha * X_0 * delta_t + theta * dW[j]
X_0 <- X[j,i]
Y[j,i] <- lambda_2 * ( - log(1 - pnorm( X[j,i] )))^(1/lambda_1)
}
}

graphic.weibull.final <- data.frame(graphic.weibull, seq(1,8760), Y)

graphic.weibull.final %>% ggplot() +
  geom_histogram(aes(x=dataset2018.Speed_100m_m.s, y=..density..), binwidth = 1, fill='gray', color = 'white', alpha=1, boundary = 0) +
  geom_histogram(aes(x=Y, y=..density..), binwidth = 1, color = 'blue', size = 1, alpha=0, boundary = 0) +
  geom_line(aes(xweibull,yweibull), color = 'red', size = 1) +
  xlab("Velocidad de viento [m/s]") + 
  ylab("Densidad de probabilidad") +
  theme_bw()

acf.Y <- acf(Y, lag.max = 120, plot = FALSE)

graphic.fac.final <- data.frame(graphic.FIT.FAC, acf.Y$acf)

graphic.fac.final %>% ggplot() +
  geom_line(aes(acf.WS.lag, acf.WS.acf), size = 1) +
  geom_line(aes(acf.WS.lag, FIT.FAC), color = 'red', size = 1) +
  geom_line(aes(acf.WS.lag, acf.Y.acf), color = 'blue', size = 1) +
  theme_bw() +
  xlab("Retardo [h]") +
  ylab("Autocorrelación") +
  xlim(c(0,120)) +
  ylim(c(0,1))

graphic.weibull.final %>% ggplot() +
  geom_line(aes(seq.1..8760., Y), size = 1) +
  theme_bw() +
  xlab("Tiempo [h]") +
  ylab("Velocidad de viento [m/s]") +
  xlim(c(0,120)) +
  ylim(c(0,20))
