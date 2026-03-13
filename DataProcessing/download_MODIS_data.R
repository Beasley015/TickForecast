# SUMMARY: This script utilizes a somewhat modified version of Dongchen's function
# to (1): query NASA's Earthdata catalog for MODIS products corresponding
# to a pre-defined area (bounding box) and date range, (2): retrieve the 
# download links which correspond to those files, and (3): download and save
# them to an output directory set by the user. 


# --- FIX: override memDecompress for gzip ---
# This replaces R's default gzip decompression with a gzcon-based workaround.
# Added because the original workflow was failing locally with a
# libdeflate-related decompression issue when reading NASA's responses.
memDecompress <- function(from, type = "gzip", ...) {
  if (identical(type, "gzip")) {
    con <- gzcon(rawConnection(from), open = "rb")
    on.exit(close(con), add = TRUE)
    # Read decompressed bytes from the gzip-compressed raw input
    readBin(con, what = "raw", n = 1e9)
  } else {
    base::memDecompress(from, type = type, ...)
  }
}

# Load helper functions from Dongchen's script.
# Use these helpers to:
# 1) resolve the DOI to a NASA collection,
# 2) build CMR query URLs,
# 3) interpret Earthdata credentials.
source("https://raw.githubusercontent.com/PecanProject/pecan/develop/modules/data.remote/R/NASA_DAAC_download.R")



NASA_DAAC_download <- function(ul_lat,
                               ul_lon,
                               lr_lat,
                               lr_lon,
                               ncore = 1,
                               from,
                               to,
                               outdir = getwd(),
                               band = NULL,
                               data_version = NULL,
                               credential_path = NULL,
                               doi,
                               just_path = FALSE) {
  
  # Ensure each download request is accompanied by an output directory and...
  if (is.null(outdir) & !just_path) {
    message("Please provide outdir if you want to download the file.")
    return(NA)
  }
  
  #...Earthdata credentials.
  if (!just_path & is.null(credential_path)) {
    PEcAn.logger::logger.info("Please provide the physical path to the credential file!")
    return(NA)
  }
  
  # Convert the credential file path into something httr can use for authentication
  if (!just_path) {
    netrc <- getnetrc(credential_path)
  }
  
  # Define the requested date range
  daterange <- c(from, to)
  
  # Resolve the DOI to NASA Common Metadata Repository (CMR) provider/concept IDs, thereby telling the API which data collection to search.
  provider_conceptID <- NASA_CMR_finder(doi = doi)
  
  # Build bounding box in the format expected by the NASA CMR API:
  # xmin, ymin, xmax, ymax = ul_lon, lr_lat, lr_lon, ul_lat
  page <- 1
  bbox <- paste(ul_lon, lr_lat, lr_lon, ul_lat, sep = ",")
  
  # This vector will collect the download URLs returned by the API.
  granules_href <- c()
  
  # Some DOIs can resolve to more than one provider/concept ID,
  # so loop across all matching concepts.
  for (i in seq_along(provider_conceptID[[2]])) {
    repeat {
      
      # Build one CMR query URL for the current page of results.
      request_url <- NASA_DAAC_URL(
        provider = provider_conceptID$provider[i],
        concept_id = provider_conceptID$concept_id[i],
        page = page,
        bbox = bbox,
        daterange = daterange
      )
      
      # Request the page of JSON metadata from NASA CMR.
      response <- curl::curl_fetch_memory(request_url)
      content <- rawToChar(response$content)
      result <- jsonlite::parse_json(content)
      
      # Stop if the API call failed.
      if (response$status_code != 200) {
        stop(paste("\n", result$errors, collapse = "\n"))
      }
      
      # Each "entry" is a granule.
      granules <- result$feed$entry
      
      # If no more granules are returned, stop paging.
      if (length(granules) == 0) break
      
      # Pull out all href links associated with those granules.
      granules_href <- c(granules_href, sapply(granules, function(x) {
        sapply(x$links, function(y) y$href)
      }))
      
      # Filename filter: keep only links whose basename matches "band".
      if (!is.null(band)) {
        granules_href <- granules_href[grepl(band, basename(granules_href), fixed = TRUE)]
      }
      
      # Version filter.
      if (!is.null(data_version)) {
        granules_href <- granules_href[grepl(data_version, granules_href, fixed = TRUE)]
      }
      
      # Move to the next page of CMR results.
      page <- page + 1
    }
  }
  
  # If nothing matched the requested spatiotemporal window, return NA.
  if (length(granules_href) == 0) {
    PEcAn.logger::logger.info("No files found. Please check the spatial and temporal search window.")
    return(NA)
  }
  
  # Keep only direct HTTPS links.
  # since NASA sometimes also returns s3 links or other non-download links.
  granules_href <- granules_href[grepl("^https://", granules_href)]
  
  # Remove duplicate URLs that point to files with the same basename.
  inds <- which(duplicated(basename(granules_href)))
  if (length(inds) > 0) granules_href <- granules_href[-inds]
  
  # Keep only actual data files rather than metadata or ancillary links.
  inds <- which(
    stringr::str_ends(basename(granules_href), ".h5") |
      stringr::str_ends(basename(granules_href), ".tif") |
      stringr::str_ends(basename(granules_href), ".hdf") |
      stringr::str_ends(basename(granules_href), ".nc")
  )
  granules_href <- granules_href[inds]
  
  # NOTE:
  # I removed an old "more than one dot" (for lack of a better phrase) filename filter from the original
  # script. This filter caused the function to break down constantly because MODIS filenames tend to come packed with dots
  # (e.g. MOD13A1.A2016001.h11v05.061.2021234.hdf).
  
  # If downloading, skip files already present in the output directory.
  if (!just_path) {
    same.file.inds <- which(basename(granules_href) %in% list.files(outdir))
    if (length(same.file.inds) > 0) granules_href <- granules_href[-same.file.inds]
  }
  
  # If everything was filtered out (or already exists), return NA.
  if (length(granules_href) == 0) return(NA)
  
  # If just_path = FALSE, download the files directly using httr + netrc auth.
  if (!just_path) {
    for (i in seq_along(granules_href)) {
      
      httr::GET(
        granules_href[i],
        httr::write_disk(file.path(outdir, basename(granules_href)[i]), overwrite = TRUE),
        httr::config(netrc = TRUE, netrc_file = netrc, followlocation = TRUE)
      )
    }
    
    # Return local file paths after download
    return(file.path(outdir, basename(granules_href)))
  } else {
    
    # If just_path = TRUE, only return the matching URLs without downloading.
    return(granules_href)
  }
}


# Load the table of site-specific bounding boxes.
dat <- read.csv("FINAL_bounders.csv")

# Select one site's bounding box from FINAL_bounders.csv.
#Ex. Row 42 corresponds to UKFS 
ul_lat <- dat$latitude_top_left[42]
ul_lon <- dat$longitude_top_left[42]
lr_lat <- dat$latitude_bottom_right[42]
lr_lon <- dat$longitude_bottom_right[42]

# Define the date range and MODIS product DOI.
# MOD13A1.061 = NDVI / EVI product
# MOD15A2H.061 = LAI product
from <- "2016-01-01"
to   <- "2022-12-31"
doi <- "10.5067/MODIS/MOD13A1.061"

# Create a local output folder for downloaded files.
# I ran downloads locally rather than on the SCC to avoid deluging the latter
# with dozens of GBs worth of raw .hdf data. 
outdir <- "C:/Users/neoch/OneDrive/Desktop/ticks" 
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

# Query NASA CMR for all matching file URLs, but don't download yet.
# This returns a vector of direct links to the granules matching the
# site bounding box, date range, and DOI.
links <- NASA_DAAC_download(ul_lat = ul_lat, ul_lon = ul_lon,
                            lr_lat = lr_lat, lr_lon = lr_lon,
                            from = from,
                            to   = to,
                            doi  = doi,
                            outdir = outdir,
                            #Set actual path to personal netrc in user environment 
                            credential_path = "C:/Users/neoch/_netrc",
                            just_path = TRUE)

library(curl)

# Personalized Earthdata bearer token used for authenticated downloads.
# Generate from website
token <- "[INSERT HERE]"

#Stop early if token is missing
if (token == "") stop("EARTHDATA_TOKEN is not set.")

# Example output directory for downloaded files (in this case, Vegetation
#Indices for the UKFS site).
outdir <- "C:/UKFS_VI"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

# Build a reusable curl handle that:
# 1. follows redirects
# 2. attaches the Earthdata bearer token to every request
h <- new_handle()
handle_setopt(
  h,
  followlocation = TRUE,
  httpheader = c(paste0("Authorization: Bearer ", token))
)

# Extract a clean filename from a URL by removing any extraneous query string.
safe_basename <- function(u) basename(sub("[?#].*$", "", u))

# Download one URL to disk unless a nonempty copy already exists.
download_one <- function(u) {
  dest <- file.path(outdir, safe_basename(u))
  
  # Skip download if the file is already present and nonempty.
  if (file.exists(dest) && file.info(dest)$size > 0) return(dest)
  
  # Otherwise, download it with the authenticated curl handle.
  curl_download(u, destfile = dest, handle = h, quiet = FALSE)
  dest
}

# Remove very small files left over from earlier failed download attempts.
existing <- list.files(outdir, full.names = TRUE)
tiny <- existing[file.info(existing)$size < 1000]
if (length(tiny)) file.remove(tiny)

# Attempt to download every matching granule URL.
# If a single file fails, report the error and continue with the others.
paths <- vapply(links, function(u) {
  tryCatch(download_one(u), error = function(e) {
    message("FAILED: ", u)
    message("  -> ", conditionMessage(e))
    NA_character_
  })
}, FUN.VALUE = character(1))

# Retain only successful downloads.
paths <- paths[!is.na(paths)]

# Summarize file sizes as a first sanity check. 
summary(file.info(paths)$size)