library(dplyr)
library(lubridate)

files <- list.files("/usr4/ugrad/neochatt/TickForecast/Data", pattern = "Ixodes", full.names = TRUE)
lai <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/all_LAIs.csv")
ndvi <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/all_VIs.csv")

lai$date <- as.Date(lai$date, format = "%m/%d/%Y")
ndvi$date <- as.Date(ndvi$date)

deer_files <- list()

par <- "beta[8]"

for (f in files) {
  dat <- read.csv(f)
  dat <- dat |> filter(node == par)
  dat$start.date <- as.Date(dat$start.date)
  deer_files[[length(deer_files) + 1]] <- dat
}

deer_files <- bind_rows(deer_files)

deer_files <- deer_files |>
  mutate(week = floor_date(start.date, "week")) |>
  group_by(week, siteID) |>
  mutate(par = mean(mean, na.rm = TRUE))

params <- deer_files |>
  group_by(siteID) |>
  summarize(par = mean[which.max(as.Date(start.date))], 
            sd = variance[which.max(as.Date(start.date))]^0.5) 


lai <- lai |>
  group_by(siteID) |>
  summarize(lai = mean(lai_mean, na.rm = TRUE))

ndvi <- ndvi |>
  group_by(siteID) |>
  summarize(ndvi = mean(ndvi_mean, na.rm = TRUE))

kat <- params |>
  inner_join(lai, by = "siteID") |>
  inner_join(ndvi, by = "siteID")

plot(kat$ndvi, kat$par, pch = 16, cex = 0.7)
abline(lm(kat$par ~ kat$ndvi), col = "red")

plot(kat$lai, kat$par, pch = 16, cex = 0.7)
abline(lm(kat$par ~ kat$lai), col = "red")


summary(lm(kat$par ~ kat$ndvi))
summary(lm(kat$par ~ kat$lai))


n <- 1000
sites <- nrow(kat)

vals <- matrix(NA, nrow = n, ncol = sites)
colnames(vals) <- kat$siteID

for (i in 1:sites) {
  vals[, i] <- rnorm(n, mean = kat$par[i], sd = kat$sd[i])
}


