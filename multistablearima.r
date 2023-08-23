library(ggplot2)
library(fpp2)

df <- read.csv("data/energybalance_0135.csv")

time = as.numeric(df[1,])
series = as.numeric(df[2,])
# TODO hardcoded dt
timeseries = ts(series, start=0, deltat=0.01)
pdf("diff")
timeseries %>% diff() %>% ggtsdisplay(main="")

(fit <- Arima(timeseries, order=c(3,1,1)))

pdf("residuals.pdf")
checkresiduals(fit)

pdf("forecast.pdf")
autoplot(forecast(fit))

