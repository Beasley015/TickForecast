library(dplyr)
library(terra)

dat <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/UPDATED_bounding_boxes.csv")
dat_2 <- data.frame(site = character(nrow(dat)))



rows <- 1:nrow(dat)

for(r in rows){
  
  ID <- dat[r,6]
  dat_2[r, 1] <- gsub("[^A-Z]", "", ID)
  
}

dat$site <- dat_2$site

dat <- dat[, c("site", setdiff(names(dat), "site"))]

n <- n_distinct(dat_2$site)
sites <- unique(dat_2$site)

dat_3 <- data.frame(
  site = character(n),
  latitude_top_left = numeric(n),
  longitude_top_left = numeric(n),
  latitude_bottom_right = numeric(n),
  longitude_bottom_right = numeric(n)
)

x <- 1

for(s in sites){
  
  cat <- dat |> filter(site == s)
  rs <- 2:nrow(cat)
  
  
  
  lat_tl <- cat[1,2]
  lon_tl <- cat[1,3]
  lat_br <- cat[1,4]
  lon_br <- cat[1,5]
  
  
  
  for(r in rs){
    
    if(lat_tl < cat[r,2]){
      lat_tl <- cat[r,2]
    }
    
    if(lon_tl > cat[r,3]){
      lon_tl <- cat[r,3]
    }
    
    if(lat_br > cat[r,4]){
      lat_br <- cat[r,4]
    }
    
    if(lon_br < cat[r,5]){
      lon_br <- cat[r,5]
    }
    
  }
  
  
  
  crs_projected <- "EPSG:3857"
  crs_geo <- "EPSG:4326"
  
  
  pts <- vect(matrix(c(lon_tl, lat_tl), ncol = 2), crs = crs_geo)
  
  pts_projected <- project(pts, crs_projected)
  
  
  ab <- crds(pts_projected)
  a <- ab[1,1]
  b <- ab[1,2]
  
  lat_top_left <- b + 45
  lon_top_left <- a - 45
  
  
  pts_final <- vect(matrix(c(lon_top_left, lat_top_left), ncol = 2), crs = crs_projected)
  
  pts_final_geo <- project(pts_final, crs_geo)
  
  ab_geo <- crds(pts_final_geo)
  
  
  dat_3[x, 2] <- ab_geo[1,2]
  dat_3[x, 3] <- ab_geo[1,1]
  
  
  
  
  pts2 <- vect(matrix(c(lon_br, lat_br), ncol = 2), crs = crs_geo)
  
  pts2_projected <- project(pts2, crs_projected)
  
  
  cd <- crds(pts2_projected)
  c <- cd[1,1]
  d <- cd[1,2]
  
  lat_bottom_right <- d - 45
  lon_bottom_right <- c + 45
  
  
  pts2_final <- vect(matrix(c(lon_bottom_right, lat_bottom_right), ncol = 2), crs = crs_projected)
  
  pts2_final_geo <- project(pts2_final, crs_geo)
  
  cd_geo <- crds(pts2_final_geo)
  
  
  dat_3[x, 4] <- cd_geo[1,2]
  dat_3[x, 5] <- cd_geo[1,1]
  
  
  
  
  
  dat_3[x,1] <- s
  
  
  x <- x+1
}




