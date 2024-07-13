BFI.predictor <- function(input_dataframe, model_path) {
  here::i_am("code/BFI-predict-function.R")
  
  # Load required packages
  packages <- c("dplyr", "sf", "raster", "terra", "ggplot2", "readr", "boot", "progress", "here")
  invisible(lapply(packages, library, character.only = TRUE))

  set.seed(313) # Set random seed for reproducibility
  
  # Load the XGBoost model
  xgb_model <- readRDS(model_path)
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
  
  # Load HUC8 basin raster
  HUC8_Basins <- terra::rast(here("data/variables/AZ_DEM_30M_latlong.tif"))
  HUC8_Basins <- project(HUC8_Basins, "+proj=longlat +datum=WGS84")
  
  # Load DEM raster
  ###RASTER IS TOO LARGE, USE 30M RASTER OF AZ
  #DEM <- rast(here("data/variables/huc8.tif"))

  #Load precip csv
  precip_df <- read.csv(here("data/variables/GW_Precipitation_ANNUAL.csv"))

  # Load temp data
  temp_df <- read.csv(here("data/variables/GW_Temperature_ANNUAL.csv"))

  #Load ET data
  et_df <- read.csv(here("data/variables/GW_ET_ANNUAL.csv"))

  # Load HUC predictors
  HUC_Predictors <- read_csv(here("data/variables/HUC_Data_FULL.csv"))

  
  # Initialize progress bar
  pb <- progress_bar$new(total = nrow(River_Points), format = "[:bar] :percent eta: :eta")
  pb$tick(0)
  
  # Assign HUC and elevation data to points
  for (i in 1:nrow(River_Points)) {
    p <- vect(River_Points[i, ], geom = c("LONG", "LAT"))
    huc <- terra::extract(HUC8_Basins, p)
    elev <- terra::extract(DEM, p)
    River_Points$HUC8[i] <- as.numeric(as.character(huc[, 2]))
    River_Points$ELEVATION_FT[i] <- (elev[, 2]) * 3.281
    pb$tick()
  }
  
  print("Added HUC and Elevation to points")
  
  # Pre-calculate indices for ppt, temp, and ET data
  pptHUC <- match(River_Points$HUC8, precip_df$HUC8)
  tempHUC <- match(River_Points$HUC8, temp_df$HUC8)
  etHUC <- match(River_Points$HUC8, et_df$HUC8)
  
  # Initialize progress bar
  pb <- progress_bar$new(total = nrow(River_Points), format = "[:bar] :percent eta: :eta")
  pb$tick(0)
  
  # Assign yearly data to points
  for (i in 1:nrow(River_Points)) {
    year <- River_Points$YEAR[i]
    huc <- River_Points$HUC8[i]
    whichYear <- paste0("X", year)
    
    temp <- temp_df[tempHUC[i], whichYear]
    River_Points$TEMP_C[i] <- round(temp, 2)
    
    ppt <- precip_df[pptHUC[i], whichYear]
    River_Points$PRECIP_MM[i] <- round(ppt, 2)
    
    et <- et_df[etHUC[i], whichYear]
    River_Points$ET_MM[i] <- round(et, 2)
    
    pb$tick()
  }
  
  # Merge all data into one dataframe
  RiverPoints_AllData <- merge(River_Points, HUC_Predictors, by = "HUC8", all.x = TRUE)
  RiverPoints_AllData <- RiverPoints_AllData[, -which(names(RiverPoints_AllData) == "NAME")]
  RiverPoints_AllData <- RiverPoints_AllData[, c(1, 3:51)]
  
  # Predict BFI using the XGBoost model
  feature_names <- xgb_model$feature_names
  RiverPoints_AllData <- RiverPoints_AllData[, c("HUC8", "YEAR", "LAT", "LONG", feature_names)]
  
  RiverPoints_AllData$predictedBFI <- inv.logit(predict(object = xgb_model, newdata = as.matrix(RiverPoints_AllData)[, 5:50]))
  
  return(RiverPoints_AllData)
}
