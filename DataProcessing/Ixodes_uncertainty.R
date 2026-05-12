library(dplyr)
library(lubridate)
library(broom)
options(scipen = 999)

#obtain parameter .csv's
files <- list.files("/usr4/ugrad/neochatt/TickForecast/Data", pattern = "Ixodes", full.names = TRUE)

#parameter names
pars <- c("phi.l.mu", "phi.n.mu", "phi.a.mu", "theta.ln", "theta.na", "beta[1]", "beta[2]", "beta[3]", "beta[4]", "beta[5]", "beta[6]", "beta[7]", "beta[8]", "beta[9]", "beta[10]", "beta[11]", "beta[12]", "beta[13]", "beta[14]")

ndvi_prop <- data.frame(
  parameter = character(length(pars)),
  b1 = numeric(length(pars)),
  var_NDVI = numeric(length(pars)),
  propagated_var = numeric(length(pars)),
  ndvi_ = numeric(length(pars))
)

lai_prop <- data.frame(
  parameter = character(length(pars)),
  b1 = numeric(length(pars)),
  var_LAI = numeric(length(pars)),
  propagated_var = logical(length(pars)),
  prop_total_param_var = numeric(length(pars))
)

r = 1

for(par in pars){
  
  deer_files <- list()
  
  #load vegetation data
  lai <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/all_LAIs.csv")
  ndvi <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/all_VIs.csv")
  
  #render it plottable
  lai$date <- as.Date(lai$date, format = "%m/%d/%Y")
  ndvi$date <- as.Date(ndvi$date)
  
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
    summarize(par = mean[which.max(as.Date(start.date))])
  
  lai <- lai |>
    group_by(siteID) |>
    summarize(lai = mean(lai_mean, na.rm = TRUE))
  
  ndvi <- ndvi |>
    group_by(siteID) |>
    summarize(ndvi = mean(ndvi_mean, na.rm = TRUE))
  
  kat <- params |>
    inner_join(lai, by = "siteID") |>
    inner_join(ndvi, by = "siteID")
  
  
  
  b1 <- summary(lm(kat$par ~ kat$ndvi))$adj.r.squared
  s <- coef(lm(kat$par ~ kat$ndvi))[2]
  
  ndvi_prop$parameter[r] <- par
  ndvi_prop$b1[r] <- s
  ndvi_prop$var_NDVI <- var(kat$ndvi)
  ndvi_prop$propagated_var[r] <- s^2 * var(kat$ndvi)
  ndvi_prop$prop_total_param_var[r] <- (s^2 * var(kat$ndvi))/var(kat$par)
  
  
  s <- coef(lm(kat$par ~ kat$lai))[2]
  
  lai_prop$parameter[r] <- par
  lai_prop$b1[r] <- s
  lai_prop$var_LAI <- var(kat$lai)
  lai_prop$propagated_var[r] <- s^2 * var(kat$lai)
  lai_prop$prop_total_param_var[r] <- (s^2 * var(kat$lai))/var(kat$par)
 
  
  r <- r+1
}
