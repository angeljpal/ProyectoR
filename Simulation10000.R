t = 8760
tray = 10000
delta_t = 1
theta = sqrt(2*alpha)
n = t/delta_t

X=matrix(ncol = tray , nrow = n)
Y=matrix(ncol = tray , nrow = n)

system.time(
for(i in 1:tray) {
X_0 <- rnorm(1)
dW <- sqrt(delta_t)*rnorm(n)
for(j in 1:n) { 
X[j,i] <- X_0 - alpha * X_0 * delta_t + theta * dW[j]
X_0 <- X[j,i]
Y[j,i] <- lambda_2 * ( - log(1 - pnorm( X[j,i] )))^(1/lambda_1)
}
}
)

hist(Y, freq=FALSE, breaks = 40, main = " ", xlab="Velocidad de viento [m/s]", ylab="Densidad de probabilidad" )
lines(WEIBULL$xweibull, WEIBULL$yweibull, col = "red", lwd = 2)

for(i in 1:tray) {
  Aux_acf <- acf(Y[ ,i], lag.max = 120, plot = FALSE)
  if(i==1){
  m_fac <- data.frame(Aux_acf$acf)
  } else {
  m_fac[,i] <- data.frame(Aux_acf$acf)
  }
}

acf_prom <- rowMeans(m_fac[,1:121])

graphic.fac.prom <- data.frame(acf.WS$lag, FIT.FAC, acf_prom)

graphic.fac.prom %>% ggplot() +
  geom_line(aes(acf.WS.lag, FIT.FAC), color = 'red', size = 1) +
  geom_line(aes(acf.WS.lag, acf_prom), color = 'gray', size = 1) +
  theme_bw() +
  xlab("Retardo [h]") +
  ylab("Autocorrelación") +
  xlim(c(0,120)) +
  ylim(c(0,1))
