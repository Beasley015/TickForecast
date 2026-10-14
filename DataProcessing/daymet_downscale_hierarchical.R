# =========================================================================== #
# functions for extracting and working with daymet
# daymet data has already been downloaded with R/0_intakeDayMet.R
# =========================================================================== #

library(tidyverse)

#' function that calculates cumulative growing degree days for each plot
#' @param site the sites being modeled
#' @param org either "tick" or "smam"
daymet_cumGDD <- function(sites) {
  df.all <- read.csv("./Data/daymetPlot_maxTemperature.csv") %>%
    mutate(siteID = str_extract(plotID, pattern = "[A-Z]+")) %>%
    mutate(siteID = case_when(siteID %in% c("GREN","HNRY","TEA") ~ "CARY",
                              TRUE ~ siteID)) %>%
    select(year, yday, Date, maxTemperature, siteID, plotID) %>%
    filter(siteID %in% sites) %>%
    mutate(Date = as.Date(Date, format = "%Y-%m-%d"))
	
	df <- df.all %>%
	  filter(year > 2015) %>%
		group_by(year,plotID) %>%
		mutate(growingDegree = if_else(maxTemperature > 10, maxTemperature - 10, 0),
			cumGDD = cumsum(growingDegree)
		) %>%
		select(Date, plotID, cumGDD, year)

	return(df)
}

## max temperature ==================================================================
daymet_temp <- function(sites, minimum) {
	if (minimum) {
	  df.all <- read.csv("./Data/daymetPlot_minTemperature.csv") %>%
	    mutate(siteID = str_extract(plotID, pattern = "[A-Z]+")) %>%
	    mutate(siteID = case_when(siteID %in% c("GREN","HNRY","TEA") ~ "CARY",
	                              TRUE ~ siteID)) %>%
	    select(year, yday, Date, minTemperature, siteID) %>%
	    filter(siteID %in% sites) %>%
	    mutate(Date = as.Date(Date, format = "%Y-%m-%d"))
		
		neon.col <- "tempTripleMinimum"
		daymet.col <- "minTemperature"
		
	} else {
	  
	  df.all <- read.csv("./Data/daymetPlot_maxTemperature.csv") %>%
	    mutate(siteID = str_extract(plotID, pattern = "[A-Z]+")) %>%
	    mutate(siteID = case_when(siteID %in% c("GREN","HNRY","TEA") ~ "CARY",
	                              TRUE ~ siteID)) %>%
	    select(year, yday, Date, maxTemperature, siteID) %>%
	    filter(siteID %in% sites) %>%
	    mutate(Date = as.Date(Date, format = "%Y-%m-%d"))
		
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
  # Calculate rh from existing daymet vars
  df.vpd <- read_csv("./Data/daymetPlot_vaporPressure.csv",
                     show_col_types = F) %>%
    mutate(siteID = str_extract(plotID, pattern = "[A-Z]+")) %>%
    mutate(siteID = case_when(siteID %in% c("GREN","HNRY","TEA") ~ "CARY",
                              TRUE ~ siteID)) %>%
    filter(siteID %in% sites)
	
  df.temp <- read_csv("./Data/daymetPlot_maxTemperature.csv",
                      show_col_types = F) %>%
    mutate(siteID = str_extract(plotID, pattern = "[A-Z]+")) %>%
    mutate(siteID = case_when(siteID %in% c("GREN","HNRY","TEA") ~ "CARY",
                              TRUE ~ siteID)) %>%
    filter(siteID %in% sites) 
  
  df.join <- left_join(df.vpd, df.temp, by = c("siteID","year",
                                               "yday","Date"),
                       relationship= 'many-to-many')
	
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
  
  return(daymet.temp.bias)
}

## Precipitation ==========================================================================

daymet_precip <- function(site) {
	# Daymet data
	df <- read_csv("./Data/daymetPlot_precipitation.csv") %>%
	  mutate(siteID = str_extract(plotID, pattern = "[A-Z]+")) %>%
	  mutate(siteID = case_when(siteID %in% c("GREN","HNRY","TEA") ~ "CARY",
	                            TRUE ~ siteID)) %>%
	  suppressMessages()

	df.p <- df %>%
	  filter(siteID %in% sites)
	  
	return(df.p)
}
