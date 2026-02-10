Data and code for forecasting tick densities across sampling locations of the National Ecological Observatory Network (NEON) and Cary Institute for Ecosystem Studies (hereafter, Cary). 

## Associated Publications

Coming Soon

## Data Preparation

All scripts required for data processing can be found in the DataProcessing folder. These scripts read from and save to the Data folder.

All NEON data can be downloaded using the script [neon_download.R](/DataProcessing/neon_download.R). DayMet meteorological data can be downloaded using the script [intakeDayMet.R](intakeDayMet.R). The most up-to-date Cary data must be obtained from one of [their repositories](https://www.caryinstitute.org/science/data).  

### Tick Drags
Script: [TickData.R](/DataProcessing/TickData.R)

Summary: Basic data cleaning such as removing dates in which sampling did not occur, removing tick species which are not analyzed in the forecasting model, and removing variables that are not used in the model. Formats Cary data to match NEON formatting and merges the two datasets. 

Data inputs: 
* NEON tick drag and tick taxonomy data (from local NEON database after running neon_download.R)
* Cary tick drag data, current file is [Ticks2006_2021.csv](/Data/Ticks2006_2021.csv)

Outputs:
* [tickTargets.csv](/Data/tickTargets.csv): used as observed tick data in model workflow
* [tickLong.csv](/Data/tickLong.csv): used to download DayMet meteorological data for each sampling plot

### Small Mammal Trapping
Scripts: [intakeSmallMammal.R](/DataProcessing/intakeSmallMammal.R); [PrepCaryMouseData.R](/DataProcessing/PrepCaryMouseData.R)

Summary: intakeSmallMammal.R adds sampling days where 0 animals were captured, marks animals that died, and formats small mammal capture data so it can be used to calculate the minimum number alive (MNA) per day. Cary mouse data is already in MNA, and PrepCaryMouseData.R re-formats the data for easier integration with the NEON data in the model workflow.

Data inputs:
* NEON small mammal capture data (from local NEON database after running neon_download.R)
* Cary MNA: current file is [cary_mna_1991-2022.csv](/Data/cary_mna_1991-2022.csv)

Outputs:
* [allSmallMammals.csv](/Data/allSmallMammals.csv): used in model workflow to calculate MNA and for downloading DayMet meteorological data
* [cary_mouse_formatted.csv](/Data/cary_mouse_formatted.csv): added to NEON MNA calculations in model workflow

### Weather
Scripts: [intakeDayMet.R](/DataProcessing/intakeDayMet.R)

Summary: Extracts latitude and longitude of each sampling plot, calculates site-level latitude and longitude (if necessary), and downloads DayMet meteorological data for these coordinates.

Data inputs: 
* [tickLong.csv](/Data/tickLong.csv)
* [allSmallMammals.csv](/Data/allSmallMammals.csv)

Outputs: site-level and plot-level DayMet data for meteorological variables day length, max temperature, min temperature, vapor pressure, and precipitation. All files are located in /Data and file names follow the format "daymet[site or plot]_[variable].csv"

### Remotely sensed data

Coming Soon

## Model Workflow

Coming Soon
