Data and code for forecasting tick densities across sampling locations of the National Ecological Observatory Network (NEON) and Cary Institute for Ecosystem Studies (hereafter, Cary). 

### Known Issues

- Duplicate entries in plot "STEI_019" in NEON data. The site is not currently being used in the model but we may need to follow up on it in the future.

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

The model workflows include the last data processing steps and initializing/running the tick forecasting model in both hierarchical and non-hierarchical formats. Main workflow scripts are located in the [R](/R) directory. Data inputs are the same for both the hierarchical and non-hierarchical models. Required scripts vary slightly: scripts required for the hierarchical version of the model have "hierarchical" in the file name. The null model, which is a generalized additive model that calculates the average tick abundance for each day of the year, is analyzed differently than the process model and can be found under the sub-heading below.

MAIN SCRIPTS: [workflow_process_model.R](/R/workflow_process_models.R)/[workflow_hierarchical_models.R](/R/workflow_hierarchical_models.R)

Supporting scripts:
* [functions.R](/DataProcessing/functions.R) and [functions_hierarchical.R](/DataProcessing/functions_hierarchical.R): Contains several miscellaneous functions for, e.g., importing tick and small mammal data, scaling meteorological data, and processing and saving model outputs.
* [capture_matrix.R](/DataProcessing/capture_matrix.R) and [capture_matrix_hierarchical.R](/DataProcessing/capture_matrix_hierarchical.R): Creates capture matrix from NEON small mammal data for calculation of MNA
* [daymet_downscale.R](/DataProcessing/daymet_downscale.R) and [daymet_downscale_hierarchical.R](/DataProcessing/daymet_downscale_hierarchical.R): Downscales DayMet meteorological data to match resolution needed for forecast model
* [nimble_forecast.R](/R/nimble_forecast.R) and [nimble_forecast_hierarchical.R](/R/nimble_forecast_hierarchical.R): Scripts for the forecast model itself
* [run_transfer_nimble.R](/R/run_transfer_nimble.R) and [run_transfer_nimble_hierarchical.R](/R/run_transfer_nimble_hierarchical.R): collects initial values, observed data, forecast model scripts, etc. and executes forecasting model using Nimble

Data inputs not listed in Data Processing subheading:
* [dormantNymphTimeSeries.csv](/Data/dormantNymphTimeSeries.csv): data on dormant nymph abundance, used as an informative prior for the corresponding latent state in the forecasting model
* [dormantNymphParams.csv](/Data/dormantNymphParams.csv): transition parameters to and from the dormant nymph state originally calibrated by Foster et al. (in preparation), used as informative priors for dormant nymph transition parameters

Outputs: Forecasts, estimated parameter values, and forecast scores from the forecasting model. Outputs are too large to be pushed to GitHub and so can't be found here. When executing the scripts, outputs are saved to a directory called /outs. Single-site models are organized into directories labeled by site; hierarchical multi-site models are arranged into directories by species.

### Null model workflow

Scripts:
* [workflow_null_model.R](/R/workflow_null_model.R): Executes the GAM for all sites and species included in Foster et al. (in preparation)
* [functions.R](/DataProcessing/functions.R): Support functions for data processing

Data inputs: [tickTargets.csv](/Data/tickTargets.csv)

Outputs: GAM results and forecast scores. Results are saved to /outs/Null.

## Post-Processing

Coming Soon