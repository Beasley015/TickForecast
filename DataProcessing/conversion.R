library(fs)
library(tidyverse)

# -------------- For satellite scene organization -------------------- 

# Example file path = HLS.L30.T10SDG.2020005T185227.v2.0.B02.tif

# Create Function 
parse_HLS <- function(input){
  fname <- basename(input)
  part <- unlist(strsplit(fname,".", fixed = TRUE))
  list(
    sensor = part[2], 
    tileID = sub("^T", "", part[3]), 
    sceneID = paste(part[1:6], collapse = '.'),
    band = part[7],
    file = input
  )
}
# Directories
in_dir <- "/projectnb/dietzelab/neochatt/MSLP/HLS/TREE"                    # Satellite data 
out_dir <- "/projectnb/dietzelab/neochatt/MSLP/input"

#Get files
imgList <- list.files(path = in_dir, full.names= TRUE)

# For Satellite data - need to MOVE files
for(i in seq_along(imgList)) {
  input <- imgList[i]
  parsed <- parse_HLS(input)
  dest_path <- file.path(out_dir, "HLS30", parsed$tileID, "images", parsed$sceneID)
  
  if (!dir.exists(dest_path)) dir.create(dest_path, recursive=TRUE)
  fs::file_move(parsed$file, file.path(dest_path, basename(parsed$file)))
  
  if(i %% 10000 == 0 || i == length(imgList)) {
    message(sprintf("Files processed: %d / %d", i, length(imgList)))
  } }


# -------------- For Fmask organization -------------------- 

# Example file path = HLS.L30.T10SDG.2016003T184610.v2.0.Fmask.tif

# Create Function 
parse_HLS <- function(input){
  fname <- basename(input)
  part <- unlist(strsplit(fname,".", fixed = TRUE))
  list(
    sensor = part[2], 
    tileID = sub("^T", "", part[3]), 
    sceneID = paste(part[1:6], collapse = '.'),
    band = part[7],
    file = input
  )
}

# Directories
in_dir <- "/projectnb/dietzelab/neochatt/MSLP/HLS/GREN"   # Fmask 
out_dir <- "/projectnb/dietzelab/ccmmf/data_phen/HLS_data_sort"

#Get files
imgList <- list.files(path = in_dir, full.names= TRUE)

# For Fmask - need to COPY files
for(i in seq_along(imgList)) {
  input <- imgList[i]
  parsed <- parse_HLS(input)
  dest_path <- file.path(out_dir, "HLS30", parsed$tileID, "images", parsed$sceneID)
  
  if (!dir.exists(dest_path)) dir.create(dest_path, recursive=TRUE)
  fs::file_copy(parsed$file, file.path(dest_path, basename(parsed$file)), overwrite = TRUE)
  
  if(i %% 1000 == 0 || i == length(imgList)) {
    message(sprintf("Files processed: %d / %d", i, length(imgList)))
  } }

# ----------- For ancillary organization ---------------- 

library(tools)

# Set the function to parse and copy file Eg: water_10SDH.tif
move_file <- function(in_dir, out_dir){
  file <- list.files(in_dir, pattern = "\\.tif$", full.names = TRUE)
  for (f in file){
    fname <- basename(f) 
    parts <- strsplit(fname, "_")[[1]]
    tileid <- file_path_sans_ext(parts[2]) #drop .tif
    
    tile_folder <- file.path(out_dir, tileid, 'images')
    
    dir.create(tile_folder, recursive = TRUE, showWarnings = FALSE)
    file.copy(f, file.path(tile_folder, fname), overwrite = TRUE)
    
    message("Copying", " ", fname)
  }
}

# Call function
in_dir <- "/projectnb/dietzelab/skanee/MSLP/ancillary_pipeline/dem/usgs_ned/hls_tiles/slope"               # Satellite data 
out_dir <- "/projectnb/dietzelab/neochatt/MSLP/input/HLS30"

move_file(in_dir, out_dir)
