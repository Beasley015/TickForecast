library(dplyr)
library(lubridate)
library(ggplot2)

# Run once if needed:
# install.packages("quantreg")

#------------------ Ixodes counts ------------------

ixodes_sites <- c(
  "GREN", "HNRY", "TEA", "BLAN", "SCBI",
  "SERC", "ORNL", "MLBS", "TREE", "HARV"
)

ticks <- read.csv(
  "/usr4/ugrad/neochatt/TickForecast/Data/tickLong.csv"
) |>
  mutate(
    collectDate = as.Date(collectDate),
    year = year(collectDate)
  ) |>
  filter(
    siteID %in% ixodes_sites,
    scientificName == "Ixodes scapularis",
    lifeStage == "Nymph"
  )

stat <- ticks |>
  group_by(siteID, plotID) |>
  summarise(
    n_records = n(),
    n_missing = sum(is.na(processedCount)),
    total_ticks = sum(processedCount),
    max_count = max(processedCount),
    density = mean(processedCount),
    .groups = "drop"
  )

#------------------ EVI2 amplitude ------------------

evi <- read.csv(
  "/usr4/ugrad/neochatt/TickForecast/Data/full_EVI2.csv"
) |>
  mutate(
    date = as.Date(date),
    year = year(date)
  ) |>
  filter(
    siteID %in% ixodes_sites,
    !is.na(date),
    is.finite(evi2_median)
  )

leaf <- evi |>
  group_by(siteID, plotID, year) |>
  summarise(
    amp = max(evi2_median) - min(evi2_median),
    .groups = "drop"
  ) |>
  group_by(siteID, plotID) |>
  summarise(
    litter = mean(amp),
    .groups = "drop"
  )

#------------------ Join and filter ------------------

dat <- stat |>
  inner_join(leaf, by = c("siteID", "plotID")) |>
  filter(
    n_records > 30,
    is.finite(density),
    is.finite(litter),
    density > 0
  )

# Add spatial coordinates without duplicating plot rows
spatial_data <- spat |>
  dplyr::select(
    siteID, plotID,
    latitude_top_left, longitude_top_left,
    latitude_bottom_right, longitude_bottom_right
  ) |>
  distinct()

dat <- dat |>
  left_join(spatial_data, by = c("siteID", "plotID"))

# Use one observation per plot for the regression
qdat <- dat |>
  dplyr::select(siteID, plotID, litter, density) |>
  distinct()

#------------------ Upper quantile regression ------------------

qfit <- quantreg::rq(
  density ~ litter,
  tau = 0.90,
  data = qdat
)

set.seed(123)
print(summary(qfit, se = "boot", R = 2000))

# Predictions for the fitted line
prediction_grid <- data.frame(
  litter = seq(
    min(qdat$litter),
    max(qdat$litter),
    length.out = 200
  )
)

prediction_grid$predicted <- predict(
  qfit,
  newdata = prediction_grid
)

#------------------ Plot ------------------

p <- ggplot(qdat, aes(x = litter, y = density)) +
  geom_point(size = 2, alpha = 0.75) +
  geom_line(
    data = prediction_grid,
    aes(x = litter, y = predicted),
    inherit.aes = FALSE,
    colour = "red",
    linewidth = 1
  ) +
  labs(
    title = "Ixodes scapularis nymphs",
    subtitle = "Red line: fitted 90th percentile",
    x = "Mean annual EVI2 amplitude",
    y = "Mean processed count per record"
  ) +
  theme_bw()

print(p)


#---------------------------------------------------------------------------
















