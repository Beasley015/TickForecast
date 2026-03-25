library(terra)

dat <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/plotLatLon.csv")


dat2 <- data.frame(
  site = character(nrow(dat)),
  latitude_top_left = numeric(nrow(dat)),
  longitude_top_left = numeric(nrow(dat)),
  latitude_bottom_right = numeric(nrow(dat)),
  longitude_bottom_right = numeric(nrow(dat))
)

rows <- 1:nrow(dat)



for(r in rows){
  
  crs_projected <- "EPSG:3857"
  crs_geo <- "EPSG:4326"
  
  lat <- dat[r, 2]
  lon <- dat[r, 3]
  
  
  pts <- vect(matrix(c(lon, lat), ncol = 2), crs = crs_geo)
  
  pts_projected <- project(pts, crs_projected)
  
  
  xy <- crds(pts_projected)
  x <- xy[1,1]
  y <- xy[1,2]
  
  lat_top_left <- y + 45
  lon_top_left <- x - 45
  lat_bottom_right <- y - 45
  lon_bottom_right <- x + 45
  
  pts_final_mat <- matrix(
    c(lon_top_left, lat_top_left,
      lon_bottom_right, lat_bottom_right),
    nrow = 2, ncol = 2, byrow = TRUE
  )
  
  pts_final <- vect(pts_final_mat, type = "points", crs = crs_projected)
  
  
  pts_final_geo <- project(pts_final, crs_geo)
  
  xy_geo <- crds(pts_final_geo)
  
  dat2[r, 1] <- dat[r, 1]
  dat2[r, 2] <- xy_geo[1,2]
  dat2[r, 3] <- xy_geo[1,1]
  dat2[r, 4] <- xy_geo[2,2]
  dat2[r, 5] <- xy_geo[2,1]
  
}


write.csv(dat2, "bounding_boxes.csv", row.names = FALSE)
