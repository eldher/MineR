# zelmac exercise units
library(dplyr)
library(data.table)
library(forecast)

# Serie zelmac 30 ------------
serie <- c(4612,4328,4294,4173,4245,4011,4213,4029,3832,4519,3676,3939,3200,3229,3742,3173,3338,3066,3106,2590,2996,2778,2829,2951,2722,2649,2863,2805,2521,2530,2319,2141,2495,2413,2428,2239,2022,2198,2311,2250,2461,2397,2196,2274,2184)

length(serie)

serie.ts <- ts(serie, frequency = 12, start = c(2013,01) )

plot(serie.ts)


fit <- tbats(serie.ts)
plot(fit)
fc <- forecast(fit, h=15)

plot(fit)
plot(fc)

# lets try stl

fit_stl <- ets(serie.ts)
plot(fit_stl)

stl_f <- forecast(fit_stl, h = 15)
plot(stl_f)

View(as.data.table(stl_f))
View(as.data.table(fc))
plot(Hw)

Hw <- HoltWinters(serie.ts, seasonal = "multiplicative")
hwf <- forecast(Hw,h=15)
View(as.data.table(hwf))
plot(Hw)
Hw$SSE


Hw1 <- HoltWinters(serie.ts, seasonal = "additive") 
Hw1$SSE

(Hw1$SSE - Hw$SSE)/Hw1$SSE

# Serie zelmac 60 -------------
serie <- c(1409,1287,1580,1292,1349,1294,1321,1174,1035,1176,1095,1110,1180,955,1250,1085,1070,1075,1176,971,1061,862,807,748,875,786,859,833,808,815,725,888,790,808,814,857,783,753,639,766,761,768,747,751,655)

length(serie)

serie.ts <- ts(serie, frequency = 12, start = c(2013,01) )
plot(serie.ts)


# 
# hw <- HoltWinters(serie.ts)
# 
# fhw <-predict(hw, n.ahead = 15)
# 
# plot(serie.ts)
# lines(fhw)
# 
# final <- ts.union(serie.ts,fhw)
# 
# plot(final)

# too many parameters, bad fit



fit <- tbats(serie.ts)
fc <- forecast(fit, h=15)

plot(fit)
plot(fc)

# lets try stl

fit_ets <- ets(serie.ts)
plot(fit_ets)

stl_f <- forecast(fit_ets, h = 15)
plot(stl_f)

View(as.data.table(stl_f))
View(as.data.table(fc))

model = stl(serie.ts, s.window=6)
summary(model)

plot(model)

ff <- auto.arima(serie.ts)
forecast(ff,h=15)

Hw <- HoltWinters(serie.ts, seasonal = "multiplicative")
summary(Hw)
hwf <- forecast(Hw,h=15)
View(as.data.table(hwf))

plot(Hw)
Hw$SSE

Hw <- HoltWinters(serie.ts, seasonal = "additive")
summary(Hw)
plot(Hw)


Hw$SSE
