library(reproducible)
library(sf)
library(terra)
library(data.table)
library(trend)
source("scripts/fcnt4analysis.R")

ecoprovinces <- reproducible::prepInputs(url = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/province/ecoprovince_shp.zip",
                                         targetFile = "ecoprovinces.shp",
                                         destinationPath = "inputs",
                                         fun = "sf::st_read")


borealForest <- prepInputs(
  url = "https://d278fo2rk9arr5.cloudfront.net/downloads/boreal.zip",
  targetFile = "NABoreal.shp",
  destinationPath = "inputs",
  fun = "sf::st_read"
)

ecoprovinces <- sf::st_transform(ecoprovinces, 3978)
borealForest <- sf::st_transform(borealForest, 3978)

ecoprovinces <- sf::st_make_valid(ecoprovinces)
borealForest <- sf::st_make_valid(borealForest)

borealForest <- sf::st_union(borealForest[borealForest$TYPE == "BOREAL" | borealForest$TYPE == "B_ALPINE", ])

# use this to loop across ecoregions
eco_boreal <- sf::st_intersection(ecoprovinces, borealForest)

yearRanges <- c(2000:2020, 2080:2100)

outputPath <- "outputs/ecoprovince/"

# Create a data frame with all combinations of CO2 scenarios, and climate models
ecoprovinces <- unique(eco_boreal$ECOPROVINC)

# For each scenario x model, create a raster of median NPP over the entire boreal forest
NPP_RCP45 <- combineResults(
  vars = "daily_npp",
  ecoregions = ecoprovinces,
  outputPath = outputPath,
  yearRange = 2071:2100,
  model = c("GCM4", "RCM4", "Hadley"),
  scenario = "RCP45"
)

NPP_RCP85 <- combineResults(
  vars = "daily_npp",
  ecoregions = ecoprovinces,
  outputPath = outputPath,
  yearRange = 2071:2100,
  model = c("GCM4", "RCM4", "Hadley"),
  scenario = "RCP85"
)

NPP_present <- combineResults(
  vars = "daily_npp",
  ecoregions = ecoprovinces,
  outputPath = outputPath,
  yearRange = 2001:2030,
  model = c("GCM4", "RCM4", "Hadley"),
  scenario = c("RCP45", "RCP85")
)
writeRaster(NPP_present, "data/processed/NPP_present.tif", overwrite = TRUE)
writeRaster(NPP_RCP45, "data/processed/NPP_RCP45.tif", overwrite = TRUE)
writeRaster(NPP_RCP85, "data/processed/NPP_RCP85.tif", overwrite = TRUE)

# For each scenario x model, create a raster of stdev NPP over the entire boreal forest
NPP_RCP45_sd <- combineResults(
  vars = "daily_npp",
  ecoregions = ecoprovinces,
  outputPath = outputPath,
  yearRange = 2071:2100,
  model = c("GCM4", "RCM4", "Hadley"),
  scenario = "RCP45",
  fun = "sd"
)

NPP_RCP85_sd <- combineResults(
  vars = "daily_npp",
  ecoregions = ecoprovinces,
  outputPath = outputPath,
  yearRange = 2071:2100,
  model = c("GCM4", "RCM4", "Hadley"),
  scenario = "RCP85",
  fun = "sd"
)

NPP_present_sd <- combineResults(
  vars = "daily_npp",
  ecoregions = ecoprovinces,
  outputPath = outputPath,
  yearRange = 2001:2030,
  model = c("GCM4", "RCM4", "Hadley"),
  scenario = c("RCP45", "RCP85"),
  fun = "sd"
)

writeRaster(NPP_present_sd, "data/processed/NPP_present_sd.tif", overwrite = TRUE)
writeRaster(NPP_RCP45_sd, "data/processed/NPP_RCP45_sd.tif", overwrite = TRUE)
writeRaster(NPP_RCP85_sd, "data/processed/NPP_RCP85_sd.tif", overwrite = TRUE)

# For each scenario x model, create a raster of trend NPP over the entire boreal forest
NPP_present_slope <- getSlopes(
  vars = "daily_npp",
  ecoregions = ecoprovinces,
  outputPath = outputPath,
  yearRange = 2001:2030,
  model = c("GCM4", "RCM4", "Hadley"),
  scenario = c("RCP45", "RCP85")
)

NPP_RCP45_slope <- getSlopes(
  vars = "daily_npp",
  ecoregions = ecoprovinces,
  outputPath = outputPath,
  yearRange = 2071:2100,
  model = c("GCM4", "RCM4", "Hadley"),
  scenario = "RCP45"
)

NPP_RCP85_slope <- getSlopes(
  vars = "daily_npp",
  ecoregions = ecoprovinces,
  outputPath = outputPath,
  yearRange = 2071:2100,
  model = c("GCM4", "RCM4", "Hadley"),
  scenario = "RCP85"
)

writeRaster(NPP_present_slope, "data/processed/NPP_present_slope.tif", overwrite = TRUE)
writeRaster(NPP_RCP45_slope, "data/processed/NPP_RCP45_slope.tif", overwrite = TRUE)
writeRaster(NPP_RCP85_slope, "data/processed/NPP_RCP85_slope.tif", overwrite = TRUE)
