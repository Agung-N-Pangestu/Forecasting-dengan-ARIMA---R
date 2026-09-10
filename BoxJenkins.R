# 1. Import data kamu
data3 <-RSXFS
data3

# 2. Ambil kolom kedua sebagai data penjualan (misalnya "RSXFS")
sales <- data3[, 2]
sales

# 3. Ubah jadi time series
sales.ts <- ts(sales, start = c(2000, 1), frequency = 12)
sales.ts
sales.ts.dec<-decompose(sales.ts, type="additive")
plot(sales.ts.dec)

library(fNonlinear)
bdsTest(sales.ts)

library(tseries)
adf.test(sales.ts)

### Plot data dan ACF & PACF
par(mfrow = c(3, 1), mar = c(2, 2, 2, 2))
plot(sales.ts, type = "l", xlab = "Month", ylab = "Rates", xaxt = "n")
x.pos <- c(1, 49, 97, 145, 193, 241, 289, 300)
x.label <- c("2000/1" , "2004/1", "2008/1", "2012/1", "2016/1", "2020/1", "2024/1", "2024/12")
axis(1, x.pos, x.label)
acf(sales, 100, xlab = "Lag", ylab = "ACF", main = "")
acf(sales, 100, type = "partial", xlab = "Lag", ylab = "Partial ACF", main = "")

# 4. Partition the series into training and test data (90% train, 10% test misalnya)
sales.train <- ts(sales.ts[1:290], start = c(2000, 1), frequency = 12)
sales.train
sales.test <- ts(sales.ts[291:300], start = c(2024, 3), frequency = 12)
sales.test
plot(sales.train)
points(sales.train)
plot(sales.test)
points(sales.test)


# 5. STL decomposition (seasonal)
sales.stl <- stl(sales.train, "periodic")
sales.stl
sales.sea <- sales.stl$time.series[, 1]
sales.sea
sales.desea <- sales.train - sales.sea
sales.desea
sales.desea.series <- as.numeric(sales.desea)
sales.desea.series

### Plot deseasonalized
par(mfrow = c(3, 1), mar = c(2, 2, 2, 2))
plot(sales.desea.series, type = "l", xlab = "Month", ylab = "Deseasonalized Rates", xaxt = "n")
x.pos <- c(1, 49, 97, 145, 193, 241, 289, 300)
x.label <- c("2000/1" , "2004/1", "2008/1", "2012/1", "2016/1", "2020/1", "2024/1", "2024/12")
axis(1, x.pos, x.label)
acf(sales.desea.series, 100, xlab = "Lag", ylab = "ACF", main = "")
acf(sales.desea.series, 100, type = "partial", xlab = "Lag", ylab = "Partial ACF", main = "")

library(tseries)
# 6. ADF Test
adf.test(sales.desea)

# 7. Pemilihan model ARIMA dengan AIC
sales.desea.aic <- matrix(0, 5,5)
for (i in 0:4) for (j in 0:4) {
  fit.arima <- arima(sales.desea, order = c(i, 1, j))
  sales.desea.aic[i + 1, j + 1] <- fit.arima$aic
}
sales.desea.aic

# 8. Fit ARIMA terbaik 
fit.arima414 <- arima(sales.desea.series, order = c(4, 1, 4))
fit.arima414
fit.arima414$residuals
adf.test(fit.arima414$residuals)

par(mfrow = c(1, 1), mar = c(2, 2, 2, 2))
acf(fit.arima414$residuals, 100, xlab = "Lag", ylab = "ACF", main = "")

# Cek kecocokan model
par(mfrow = c(1, 1), mar = c(2, 2, 2, 2))
plot(sales.desea.series, type='l')
fit.arima213$residuals
datamodel<-sales.desea.series-fit.arima213$residuals
datamodel
lines(datamodel,col="red")

### Uji hipotesis kecocokan model dengan Ljung-Box
tsdiag(fit.arima213)
Box.test(fit.arima213$residuals)

# 9. Prediksi dengan data test
fit.arima213 <- arima(sales.desea, order = c(2, 1, 3))
sales.pred <- predict(fit.arima213, n.ahead = 10)

# 10. Gabungkan prediksi dengan data sesungguhnya
sales.pred.summary <- data.frame(
  true = as.numeric(sales.test),
  predict = sales.pred$pred + sales.sea[1:10],
  ci.lower = sales.pred$pred - 1.96 * sales.pred$se + sales.sea[1:10],
  ci.upper = sales.pred$pred + 1.96 * sales.pred$se + sales.sea[1:10]
)
colnames(sales.pred.summary) <- c("true", "predict", "ci.lower", "ci.upper")
sales.pred.summary

### plot kesamaan model data dan data asli
par(mfrow = c(1, 1), mar = c(4, 4, 4, 4))
plot(c(1, 10), range(sales.pred.summary), type = "n", xlab = "Month", ylab = "RSXFS", xaxt = "n")
lines(1:10, sales.pred.summary$true, lty = 1)
points(1:10, sales.pred.summary$true, pch = 16)
lines(1:10, sales.pred.summary$predict, lty = 2)
points(1:10, sales.pred.summary$predict, pch = 1)
lines(1:10, sales.pred.summary$ci.lower, lty = 3)
points(1:10, sales.pred.summary$ci.lower, pch = 0)
lines(1:10, sales.pred.summary$ci.upper, lty = 3)
points(1:10, sales.pred.summary$ci.upper, pch = 0)
legend(1, max(sales.pred.summary$ci.upper),
       c("true", "predicted", "pred. int.", "pred. int."),
       lty = c(1, 2, 3, 3), pch = c(16, 1, 0, 0))
axis(1, at = c(2, 4, 6, 8, 10), labels = c("Apr","Jun","Agust","Oct","Des"))

# Forecast
sales.pred2 <- predict(fit.arima213, n.ahead = 38)
sales.pred2
true.values <- c(as.numeric(sales.test), rep(NA, 28))  # Total = 38
sales.pred2.summary <- data.frame(
  true = true.values,  # 10 nilai aktual, sisanya NA
  predict = sales.pred2$pred + sales.sea[1:38],
  ci.lower = sales.pred2$pred - 1.96 * sales.pred2$se + sales.sea[1:38],
  ci.upper = sales.pred2$pred + 1.96 * sales.pred2$se + sales.sea[1:38]
)
colnames(sales.pred2.summary) <- c("true", "predict", "ci.lower", "ci.upper")
sales.pred2.summary

### plot prediksi
par(mfrow = c(1, 1), mar = c(4, 4, 4, 4))
plot(
  x = c(1, 38),
  y = range(sales.pred2.summary[, c("true", "predict", "ci.lower", "ci.upper")], na.rm = TRUE),
  type = "n", xlab = "Month", ylab = "RSXFS", xaxt = "n"
)
lines(1:38, sales.pred2.summary$true, lty = 1)
points(1:38, sales.pred2.summary$true, pch = 16)
lines(1:38, sales.pred2.summary$predict, lty = 2)
points(1:38, sales.pred2.summary$predict, pch = 1)
lines(1:38, sales.pred2.summary$ci.lower, lty = 3)
points(1:38, sales.pred2.summary$ci.lower, pch = 0)
lines(1:38, sales.pred2.summary$ci.upper, lty = 3)
points(1:38, sales.pred2.summary$ci.upper, pch = 0)
legend(1, max(sales.pred2.summary$ci.upper, na.rm = TRUE),
       legend = c("true", "predicted", "pred. int.", "pred. int."),
       lty = c(1, 2, 3, 3), pch = c(16, 1, 0, 0))
axis(1, at = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38),
     labels = c("Apr", "Jun", "Agust", "Oct", "Des", "Feb", "Apr", "Jun", "Agust", "Oct",
                "Des", "Feb", "Apr", "Jun", "Agust", "Oct", "Des", "Feb", "Apr"))

# Hitung selisih (error)
error <- sales.pred.summary$predict - sales.pred.summary$true
error

# MAE
mae <- mean(abs(error))

# RMSE
rmse <- sqrt(mean(error^2))

# MAPE (jika tidak ada nilai 0)
mape <- mean(abs(error / sales.pred.summary$true)) * 100

# Tampilkan hasil
cat("MAE: ", round(mae, 2), "\n")
cat("RMSE:", round(rmse, 2), "\n")
cat("MAPE:", round(mape, 2), "%\n")

