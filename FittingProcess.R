library(rriskDistributions)
fit.dist <- fit.cont(WS)

lambda_1 <- fit.dist$fittedParams[1]
lambda_2 <- fit.dist$fittedParams[2]

xweibull = seq(25/8760, 25, 25/8760)

yweibull <- dweibull(xweibull, shape = lambda_1, scale = lambda_2)

WEIBULL <- data.frame(xweibull, yweibull)

graphic.weibull <- data.frame(dataset2018$Speed_100m_m.s, WEIBULL)

graphic.weibull %>% ggplot(aes(x=dataset2018.Speed_100m_m.s)) +
        geom_histogram(aes(y=..density..), binwidth = 1, fill='gray', color = 'white', alpha=1, boundary = 0) +
        geom_line(aes(xweibull,yweibull), color = 'red', size = 1) +
        xlab("Velocidad de viento [m/s]") + 
        ylab("Densidad de probabilidad") +
        theme_bw()

z <- acf.WS$acf[1:68,1,1]
x <- acf.WS$lag[1:68,1,1]
y <- log(z)
fit.exponetial <- lm(y~x-1)

alpha <- -fit.exponetial$coefficients

tau <- seq(0, 120)

FIT.FAC <- exp(-alpha*tau)

graphic.FIT.FAC <- data.frame(FAC, FIT.FAC)

graphic.FIT.FAC %>% ggplot() +
        geom_line(aes(acf.WS.lag, acf.WS.acf), size = 1) +
        geom_line(aes(acf.WS.lag, FIT.FAC), color = 'red', size = 1) +
        theme_bw() +
        xlab("Retardo [h]") +
        ylab("Autocorrelación") +
        xlim(c(0,120)) +
        ylim(c(0,1))
