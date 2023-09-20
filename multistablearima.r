library(ggplot2)
library(fpp3)

df <- read.csv("data/energybalance.csv")

timeseries <- tibble(df) |>
    as_tsibble(index = Time)

train <- timeseries %>% filter(Time <= 1600)
print(train)

pdf("ts_train.pdf")
autoplot(train)

pdf("nodiff.pdf")
train |>
    gg_tsdisplay(Temperature, plot_type = "partial")

pdf("diff.pdf")
train |>
    gg_tsdisplay(difference(Temperature), plot_type = "partial")

pdf("diff2.pdf")
train |>
    gg_tsdisplay(difference(difference(Temperature)), plot_type = "partial")

fit <- train |>
  model(
    nodiff = ARIMA(Temperature ~ pdq(0:6, 0, 0:6), stepwise = FALSE, greedy = FALSE, approximation = FALSE),
    diff1 = ARIMA(Temperature ~ pdq(0:6, 1, 0:6), stepwise = FALSE, greedy = FALSE, approximation = FALSE)
    #,
    # diff2 = ARIMA(Temperature ~ pdq(0:6, 2, 0:6), stepwise = FALSE, greedy = FALSE, approximation = FALSE)
  )

print(fit)

glance(fit) |> arrange(AICc) |> select(.model:BIC)

pdf("residuals_nodiff.pdf")
fit |>
  select(nodiff) |>
  gg_tsresiduals()
  
pdf("residuals_diff1.pdf")
fit |>
  select(diff1) |>
  gg_tsresiduals()

# pdf("residuals_diff2.pdf")
# fit |>
#   select(diff2) |>
#   gg_tsresiduals()

pdf("forecast.pdf", width = 5, height = 2.5)
fit |>
  forecast(h=1000) |>
  autoplot(train)

pdf("simulation.pdf", width = 5, height = 2.5)
simulations <- fit |>
  forecast(h = 1000, simulate = TRUE, times = 1000)
fit |>
  forecast(h = 1000, simulate = TRUE, times = 1) |>
  autoplot(train)

simulationshist <- fit |>
  select(nodiff) |>
  forecast(h = 1000, simulate = TRUE, times = 1000)

hist <- tibble(x = simulationshist |>
  pull(Temperature) |>
  unlist())

pdf("histogram.pdf", width = 5, height = 2.5)
ggplot() +
  geom_histogram(aes(x = x, y = after_stat(ndensity), fill = "Temperature"),
    data = hist,
    bins = 50,
    show.legend = FALSE
  ) +
  xlab("Temperature") +
  ylab("Density")


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
