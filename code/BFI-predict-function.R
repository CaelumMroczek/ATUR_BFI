BFI.predictor <- function(input_dataframe, model_path) {
  here::i_am("code/BFI-predict-function.R")
  
  # Load required packages
  packages <- c("dplyr", "sf", "raster", "terra", "ggplot2", "readr", "boot", "progress", "here", "xgboost")
  invisible(lapply(packages, library, character.only = TRUE))

  set.seed(313) # Set random seed for reproducibility
  
  # Load the XGBoost model
  xgb_model <- model_path
  River_Points <- input_dataframe
  
  # Prepare the input data
  year_list <- 1991:2020
  River_Points$ID <- 1:nrow(River_Points)
  
  # Add years to each site and merge
  expanded_years <- expand.grid(LAT = unique(River_Points$LAT), YEAR = year_list)
  River_Points <- merge(River_Points, expanded_years, by = "LAT", all.x = TRUE)
  River_Points <- River_Points[order(River_Points$ID, River_Points$YEAR), ]
  River_Points <- River_Points[, c("ID", "YEAR", "LAT", "LONG")]
  colnames(River_Points) <- c("ID", "YEAR", "LAT", "LONG")
  
  # Load required datasets
  
  # Load GWBasin shapefile
  GW_Basins <- terra::vect(here("data/GW_shapefile"))
  GW_Basins <- project(GW_Basins, "+proj=longlat +datum=WGS84")
  
  # Load DEM raster
  ###RASTER IS TOO LARGE, USE 30M RASTER OF AZ
  DEM <- terra::rast(here("data/variables/AZ_DEM_30M_latlong.tif"))

  #Load precip csv
  precip_df <- read.csv(here("data/variables/GW_Precipitation_ANNUAL.csv"))

  # Load temp data
  temp_df <- read.csv(here("data/variables/GW_Temperature_ANNUAL.csv"))

  #Load ET data
  et_df <- read.csv(here("data/variables/GW_ET_ANNUAL.csv"))

  # Load GWBasin predictors
  GW_Predictors <- read_csv(here("data/variables/GWBasin_Data_FULL.csv"), show_col_types = FALSE)

  
  # Initialize progress bar
  pb <- progress_bar$new(total = nrow(River_Points), format = "[:bar] :percent eta: :eta")
  pb$tick(0)
  
  # Assign GWBasin and elevation data to points
  for (i in 1:nrow(River_Points)) {
    p <- vect(River_Points[i, ], geom = c("LONG", "LAT"))
    GWBasin <- terra::extract(GW_Basins, p)
    elev <- terra::extract(DEM, p)
    River_Points$GWBasin[i] <- as.character(GWBasin[, 3])
    River_Points$ELEV_FT[i] <- (elev[, 2]) * 3.281
    pb$tick()
  }
  
  print("Added GWBasin and Elevation to points")
  
  # Pre-calculate indices for ppt, temp, and ET data
  pptGWBasin <- match(River_Points$GWBasin, precip_df$GWBasin)
  tempGWBasin <- match(River_Points$GWBasin, temp_df$GWBasin)
  etGWBasin <- match(River_Points$GWBasin, et_df$GWBasin)
  
  # Initialize progress bar
  pb <- progress_bar$new(total = nrow(River_Points), format = "[:bar] :percent eta: :eta")
  pb$tick(0)
  
  # Assign yearly data to points
  for (i in 1:nrow(River_Points)) {
    year <- River_Points$YEAR[i]
    GWBasin <- River_Points$GWBasin[i]
    whichYear <- paste0("X", year)
    
    temp <- temp_df[tempGWBasin[i], whichYear]
    River_Points$TEMP_C[i] <- round(temp, 2)
    
    ppt <- precip_df[pptGWBasin[i], whichYear]
    River_Points$PRECIP_MM[i] <- round(ppt, 2)
    
    et <- et_df[etGWBasin[i], whichYear]
    River_Points$ET_MM[i] <- round(et, 2)
    
    pb$tick()
  }
  
  # Merge all data into one dataframe
  RiverPoints_AllData <- merge(River_Points, GW_Predictors, by = "GWBasin", all.x = TRUE)
  RiverPoints_AllData <- RiverPoints_AllData[, c(1, 3:51)]
  
  # Predict BFI using the XGBoost model
  feature_names <- xgb.model$feature_names
  RiverPoints_AllData <- RiverPoints_AllData[, c("GWBasin", "YEAR", "LAT", "LONG", feature_names)]
  
  RiverPoints_AllData$predictedBFI <- inv.logit(predict(object = xgb.model, newdata = as.matrix(RiverPoints_AllData[, 5:50])))
  
  return(RiverPoints_AllData)
}
