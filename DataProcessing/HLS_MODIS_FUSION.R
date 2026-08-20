evi <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/MODIS_site_VIs.csv")
evi$date <- as.Date(evi$date)

gren <- evi |> filter(siteID == "GREN")
tree <- evi |> filter(siteID == "TREE")

gren_a <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/GREN_A.csv")
gren_a <- na.omit(gren_a)
gren_a$date <- as.Date(gren_a$date)

gren_b <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/GREN_B.csv")
gren_ba <- na.omit(gren_b)
gren_b$date <- as.Date(gren_b$date)

tree_a <- read.csv("/usr4/ugrad/neochatt/TickForecast/Data/TREE.csv")
tree_a <- na.omit(tree_a)
tree_a$date <- as.Date(tree_a$date)




legend("topright", 
       legend = c("T18TWM (HLS)", "T18TXM (HLS)", "MODIS"), 
       col = c("red", "blue", "black"), 
       lty = c(1, 1, 2),
       cex = 0.7)


plot(gren_a$date, gren_a$evi2, type = "l", col = "red", 
     xlab = "Date", ylab = "EVI",
     main = "GREN")
points(gren_a$date, gren_a$evi2, pch = 16, cex = 0.7, col = "red")

lines(gren_b$date, gren_b$evi2, col = scales::alpha("blue", 0.7))
points(gren_b$date, gren_b$evi2, pch = 16, cex = 0.7, col = "blue")

lines(gren$date, gren$evi_median, lty = 2)
points(gren$date, gren$evi_median, pch = 16, cex = 0.7)

plot(gren$date, gren$evi_median, type = "l", lty = 2)



plot(tree_a$date, tree_a$evi2, type = "l", col = "orange", main = "TREE")
points(tree_a$date, tree_a$evi2, pch = 16, cex = 0.7, col = "orange")

lines(tree$date, tree$evi_median, lty = 2)
points(tree$date, tree$evi_median, pch = 16, cex = 0.7)


legend("topright", 
       legend = c("T15TYL (HLS)", "MODIS"), 
       col = c("orange", "black"), 
       lty = c(1, 2),
       cex = 0.7)