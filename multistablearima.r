library(ggplot2)
library(fpp3)

df <- read.csv("data/energybalance.csv")

timeseries <- tibble(df) |>
    as_tsibble(index = Time)

train <- timeseries %>% filter(Time <= 1600)

pdf("ts_train.pdf")
autoplot(train)

pdf("nodiff.pdf")
train |>
    gg_tsdisplay(Temperature, plot_type='partial')

pdf("diff.pdf")
train |>
    gg_tsdisplay(difference(Temperature), plot_type='partial')

pdf("diff2.pdf")
train |>
    gg_tsdisplay(difference(difference(Temperature)), plot_type='partial')

fit <- train |>
  model(search = ARIMA(Temperature, stepwise = FALSE, greedy = FALSE))
  
print(fit)

glance(fit) |> arrange(AICc) |> select(.model:BIC)


pdf("residuals.pdf")
fit |>
  select(search) |>
  gg_tsresiduals()
  

pdf("forecast.pdf")
fit |>
  forecast(h=1000) |>
  autoplot(train)

pdf("simulation.pdf")
simulations <- fit |>
  forecast(h = 1000, simulate = TRUE, times = 1000)
fit |>
  forecast(h = 1000, simulate = TRUE, times = 1) |>
  autoplot(train)

hist <- tibble(x = simulations |>
  pull(Temperature) |>
  unlist())

pdf("histogram.pdf")
ggplot() +
  geom_histogram(aes(x = x, y = after_stat(ndensity), fill = "Temperature"),
    data = hist,
    bins = 50
  )


# pdf("diff2.pdf")
# train %>%
#     difference() %>%
#     difference() %>%
#     autoplot()

# (fit <- auto.arima(train))

# pdf("residuals.pdf")
# checkresiduals(fit)

# pdf("forecast.pdf")
# autoplot(forecast(fit))
