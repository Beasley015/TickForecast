# =========================================================================== #
# functions for extracting and working with daymet
# daymet data has already been downloaded with R/0_intakeDayMet.R
# =========================================================================== #

library(tidyverse)

#' function that calculates cumulative growing degree days for each plot
#' @param site the site being modeled
#' @param org either "tick" or "smam"
daymet_cumGDD <- function(sites) {
  if(all(c("GREN", "HNRY", "TEA") %in% sites)){
    df.cary <- read_csv("./Data/Cary_maxTemperature.csv",
                        show_col_types = F)
    cary.sites <- rep(c("GREN", "HNRY", "TEA"), each = nrow(df.cary))
  
    df.cary <- bind_rows(df.cary, df.cary, df.cary) %>%
      mutate(siteID = cary.sites)
  }

  df.neon <- read.csv("./Data/daymetSite_maxTemperature.csv") %>%
    select(year, yday, Date, maxTemperature, siteID) %>%
    filter(siteID %in% sites) %>%
    mutate(Date = as.Date(Date, format = "%Y-%m-%d"))
  
  if(exists("df.cary")==T){
    df.all <- bind_rows(df.cary, df.neon) 
  } else{
    df.all <- df.neon
  }
	
	df <- df.all %>%
		group_by(year) %>%
		mutate(
			growingDegree = if_else(maxTemperature > 10, maxTemperature - 10, 0),
			cumGDD = cumsum(growingDegree)
		) %>%
		select(Date, siteID, cumGDD, year)

	return(df)
}

## max temperature ==================================================================
daymet_temp <- function(sites, minimum) {
	if (minimum) {
	  if(all(c("GREN", "HNRY", "TEA") %in% sites)){
	    df.cary <- read_csv("./Data/Cary_minTemperature.csv",
	                        show_col_types = F)
	    cary.sites <- rep(c("GREN", "HNRY", "TEA"), each = nrow(df.cary))
	    
	    df.cary <- bind_rows(df.cary, df.cary, df.cary) %>%
	      mutate(siteID = cary.sites)
	  }
	  
	  df.all <- read.csv("./Data/daymetSite_minTemperature.csv") %>%
	    select(year, yday, Date, minTemperature, siteID) %>%
	    filter(siteID %in% sites) %>%
	    mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
	    bind_rows(df.cary)
		
		neon.col <- "tempTripleMinimum"
		daymet.col <- "minTemperature"
		
	} else {
	  if(all(c("GREN", "HNRY", "TEA") %in% sites)){
	    df.cary <- read_csv("./Data/Cary_maxTemperature.csv",
	                        show_col_types = F)
	    cary.sites <- rep(c("GREN", "HNRY", "TEA"), each = nrow(df.cary))
	    
	    df.cary <- bind_rows(df.cary, df.cary, df.cary) %>%
	      mutate(siteID = cary.sites)
	  }
	  
	  df.all <- read.csv("./Data/daymetSite_maxTemperature.csv") %>%
	    select(year, yday, Date, maxTemperature, siteID) %>%
	    filter(siteID %in% sites) %>%
	    mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
	    bind_rows(df.cary)
		
		neon.col <- "tempTripleMaximum"
		daymet.col <- "maxTemperature"
	}

	 df.temp <- df.all %>%
	   group_by(yday)
	 
	 neon.temp <- read_csv("./Data/airTempDaily.csv",
	                       show_col_types = F)
	
	 neon.sub <- neon.temp %>%
	   filter(siteID %in% sites) %>%
	   mutate(yday = yday(Date))
	 
	 neon.doy <- neon.sub %>%
	   group_by(siteID, yday) %>%
	   summarise(muNeon = mean(.data[[neon.col]])) %>%
	   ungroup()

	 daymet.doy <- df.temp %>%
	   group_by(siteID, yday) %>%
	   summarise(muDaymet = mean(.data[[daymet.col]])) %>%
	   ungroup()

	 tempbias <- right_join(neon.doy, daymet.doy, by = c("yday", "siteID")) %>%
	   mutate(tempBias = case_when(is.na(muNeon) == F ~ muNeon - muDaymet,
	                               TRUE ~ 0)) %>%
	   select(yday, tempBias, siteID)

	  daymet.temp.bias <- left_join(df.temp, tempbias, 
	                                by = c("siteID","yday")) %>%
		  mutate(TempCorrect = .data[[daymet.col]] + tempBias)

	  if (minimum) {
		  daymet.temp.bias <- daymet.temp.bias %>%
			  rename(minTempCorrect = TempCorrect)
	  } else {
		  daymet.temp.bias <- daymet.temp.bias %>%
			  rename(maxTempCorrect = TempCorrect)
	  }
	  
	return(daymet.temp.bias)
}

## relative humidity ==========================================================================

daymet_rh <- function(sites) {
  if(all(c("HNRY", "GREN", "TEA") %in% sites)){
    rh.cary <- read_csv("./Data/Cary_vaporPressure.csv",
                        show_col_types = F) 
    cary.sites <- rep(c("GREN", "HNRY", "TEA"), each = nrow(rh.cary))
    
    rh.cary <- bind_rows(rh.cary, rh.cary, rh.cary) %>%
      mutate(siteID = cary.sites) %>%
      rename(maxRHCorrect=maxRH, minRHCorrect=minRH) %>%
      select(-c(year, yday))
  }
  
  # Calculate rh from existing daymet vars
  df.vpd <- read_csv("./Data/daymetSite_vaporPressure.csv",
                     show_col_types = F) %>%
    filter(siteID %in% sites)
	
  df.temp <- read_csv("./Data/daymetSite_maxTemperature.csv",
                      show_col_types = F) %>%
      filter(siteID %in% sites) 
  
  df.join <- left_join(df.vpd, df.temp, by = c("siteID","year",
                                               "yday","Date"))
	
  df.dew <- df.join %>%
		  ungroup() %>%
		  mutate(rh = plantecophys::VPDtoRH(vaporPressure / 1000, maxTemperature))

  # Bias correction
  neon.temp <- read_csv("./Data/RelativeHumidityDaily.csv",
                        show_col_types = F)
	
  neon.sub <- neon.temp %>%
    filter(siteID %in% sites) %>%
    mutate(yday = yday(Date)) %>%
    select(siteID, Date, yday, RHMaximum, RHMinimum)

  neon.doy <- neon.sub %>%
    group_by(yday, siteID) %>%
    summarise(muRHmax = mean(RHMaximum), muRHmin = mean(RHMinimum))
	  
  daymet.doy <- df.dew %>%
    group_by(yday, siteID) %>%
    summarise(muDaymet = mean(rh))

  df.join <- left_join(neon.doy, daymet.doy, by = c("yday", "siteID")) %>%
		  mutate(biasMax = muRHmax - muDaymet, biasMin = muRHmin - muDaymet)

  daymet.temp.bias <- left_join(df.dew, df.join, by = c("yday", "siteID")) %>%
    mutate(
			  maxRHCorrect = pmin(rh + biasMax, 100),
			  minRHCorrect = pmin(rh + biasMin, 100)
		  ) %>%
    select(Date, maxRHCorrect, minRHCorrect, siteID)
  
  # Add Cary sites
  daymet.temp.bias <- bind_rows(daymet.temp.bias, rh.cary)
	  
  return(daymet.temp.bias)
}

## Precipitation ==========================================================================

daymet_precip <- function(site) {
  # Read in Cary sites, if present
  if(all(c("HNRY", "GREN", "TEA") %in% sites)){
    precip.cary <- read_csv("./Data/Cary_precipitation.csv",
                        show_col_types = F) 
    cary.sites <- rep(c("GREN", "HNRY", "TEA"), each = nrow(precip.cary))
    
    precip.cary <- bind_rows(precip.cary, precip.cary, precip.cary) %>%
      mutate(siteID = cary.sites) %>%
      select(-c(year, yday))
  }
  
  # Read in NEON data for some reason?
	# neon.precip <- read_csv("./Data/precipDaily.csv")
	# 
	# neon.sub <- neon.precip %>%
	#   filter(siteID %in% sites) %>%
	#   mutate(year = year(Date)) %>%
	#   group_by(year) %>%
	#   summarise(sum.precip = sum(priPrecipTotal)) %>%
	#   pull(sum.precip) %>%
	#   mean()

	# Daymet data
	df <- read_csv("./Data/daymetSite_precipitation.csv")

	df.p <- df %>%
	  filter(siteID %in% sites)
	
	# Add Cary data
	df.p <- bind_rows(df.p, precip.cary)
	  
	return(df.p)
}
