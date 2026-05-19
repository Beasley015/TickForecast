#This script generates plot and site-level bounding boxes in the following ways, 
#respectively:

# (1): Take each plot's centroid and draw a 45-m rectangular buffer around it
# (2): For each *site*, compute the minimum rectangle required to encompass all the
#      plot-level boxes; then, add another 45-m rectangular buffer. 

#==================PLOT LEVEL======================

library(terra)

#Load plot coordinates
dat <- read.csv("/projectnb/dietzelab/ebeasley/TickForecast/Data/plotLatLon.csv")

#Initialize data frame
dat2 <- data.frame(
  siteID = character(nrow(dat)),
  plotID = character(nrow(dat)),
  latitude_top_left = numeric(nrow(dat)),
  longitude_top_left = numeric(nrow(dat)),
  latitude_bottom_right = numeric(nrow(dat)),
  longitude_bottom_right = numeric(nrow(dat))
)

rows <- 1:nrow(dat)



for(r in rows){
  
  #define coordinate reference systems 
  crs_projected <- "EPSG:3857"
  crs_geo <- "EPSG:4326"
  
  lat <- dat$decimalLatitude[r]
  lon <- dat$decimalLongitude[r]
  
  #switch to projected for linear operations 
  pts <- vect(matrix(c(lon, lat), ncol = 2), crs = crs_geo)
  
  pts_projected <- project(pts, crs_projected)
  
  #disaggregate projected centroid into latititude and longitude 
  xy <- crds(pts_projected)
  x <- xy[1,1]
  y <- xy[1,2]
  
  #draw rectangular buffer
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
  
  #switch back to geometric 
  pts_final_geo <- project(pts_final, crs_geo)
  
  xy_geo <- crds(pts_final_geo)
  
  #insert into dataframe by site 
  dat2$siteID[r] <- gsub("[^A-Z]", "", dat$plotID[r])
  dat2$plotID[r] <- dat$plotID[r]
  dat2$latitude_top_left[r] <- xy_geo[1,2]
  dat2$longitude_top_left[r] <- xy_geo[1,1]
  dat2$latitude_bottom_right[r] <- xy_geo[2,2]
  dat2$longitude_bottom_right[r] <- xy_geo[2,1]
}




#==================SITE LEVEL======================

#obtain number and names of unique sites from plot box dataframe
n <- n_distinct(dat$siteID)
sites <- unique(dat$siteID)

#initialize new dataframe
dat3 <- data.frame(
  siteID = character(n),
  latitude_top_left = numeric(n),
  longitude_top_left = numeric(n),
  latitude_bottom_right = numeric(n),
  longitude_bottom_right = numeric(n)
)

x <- 1

for(s in sites){
  
  #extract only the plot-level boxes for site s
  cat <- dat |> filter(siteID == s)
  rs <- 2:nrow(cat)
  
  lat_tl <- cat$latitude_top_left[1]
  lon_tl <- cat$longitude_top_left[1]
  lat_br <- cat$latitude_bottom_right[1]
  lon_br <- cat$longitude_bottom_right[1]
  
  
  #find maximum top left latitude/bottom right longitude and minimum bottom right
  #latitude/top left longitude of all the plot-level boxes 
  
  for(r in rs){
    
    if(lat_tl < cat$latitude_top_left[r]){
      lat_tl <- cat$latitude_top_left[r]
    }
    
    if(lon_tl > cat$longitude_top_left[r]){
      lon_tl <- cat$longitude_top_left[r]
    }
    
    if(lat_br > cat$latitude_bottom_right[r]){
      lat_br <- cat$latitude_bottom_right[r]
    }
    
    if(lon_br < cat$longitude_bottom_right[r]){
      lon_br <- cat$longitude_bottom_right[r]
    }
    
  }
  
  
  
  crs_projected <- "EPSG:3857"
  crs_geo <- "EPSG:4326"
  
  pts <- vect(matrix(c(lon_tl, lat_tl), ncol = 2), crs = crs_geo)
  
  pts_projected <- project(pts, crs_projected)
  
  #Compute projected top left coordinates 
  ab <- crds(pts_projected)
  a <- ab[1,1]
  b <- ab[1,2]
  
  #Add final 45-m buffer 
  lat_top_left <- b + 45
  lon_top_left <- a - 45
  
  
  pts_final <- vect(matrix(c(lon_top_left, lat_top_left), ncol = 2), crs = crs_projected)
  
  pts_final_geo <- project(pts_final, crs_geo)
  
  ab_geo <- crds(pts_final_geo)
  
  #insert top left latitude and longitude into data frame
  dat3[x, 2] <- ab_geo[1,2]
  dat3[x, 3] <- ab_geo[1,1]
  
  
  
  
  pts2 <- vect(matrix(c(lon_br, lat_br), ncol = 2), crs = crs_geo)
  
  pts2_projected <- project(pts2, crs_projected)
  
  #Compute projected bottom right coordinates 
  cd <- crds(pts2_projected)
  c <- cd[1,1]
  d <- cd[1,2]
  
  #Add final 45-m buffer
  lat_bottom_right <- d - 45
  lon_bottom_right <- c + 45
  
  
  pts2_final <- vect(matrix(c(lon_bottom_right, lat_bottom_right), ncol = 2), crs = crs_projected)
  
  pts2_final_geo <- project(pts2_final, crs_geo)
  
  cd_geo <- crds(pts2_final_geo)
  
  #insert bottom right latitude and longitude into dataframe
  dat3[x, 4] <- cd_geo[1,2]
  dat3[x, 5] <- cd_geo[1,1]
  
  #define site 
  dat3[x,1] <- s
  
  
  x <- x+1
}

