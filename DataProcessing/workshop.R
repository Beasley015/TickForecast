doy <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/DOYs.csv")

retreat <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/RETREAT.csv")

lat <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/UPDATED_FINAL_bounders.csv")

lat <- lat |>
  group_by(siteID) |>
  summarise(latitude = mean(c(latitude_top_left, latitude_bottom_right)))


dat <- left_join(doy, retreat)

dat <- inner_join(dat, lat)

dat <- dat |>
  group_by(siteID) |>
  mutate(
    mean_15 = sapply(seq_along(tick_15), \(i) mean(tick_15[-i])),
    sd_15  = sapply(seq_along(tick_15), \(i) sd(tick_15[-i]))
  ) |>
  ungroup()


dat$pred_15 <- dat$mean_15 + dat$sd_15 * rt(nrow(dat), df = 5) / sqrt(5 / 3)











