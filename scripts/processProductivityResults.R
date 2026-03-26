library(reproducible)
library(sf)
library(terra)
library(data.table)
source("scripts/fcnt4analysis.R")

ecoregions <- prepInputs(
  url = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/region/ecoregion_shp.zip",
  targetFile = "ecoregions.shp",
  destinationPath = "inputs",
  fun = "sf::st_read"
)

borealForest <- prepInputs(
  url = "https://d278fo2rk9arr5.cloudfront.net/downloads/boreal.zip",
  targetFile = "NABoreal.shp",
  destinationPath = "inputs",
  fun = "sf::st_read"
)

ecoregions <- st_transform(ecoregions, 3978)
borealForest <- st_transform(borealForest, 3978)

ecoregions <- st_make_valid(ecoregions)
borealForest <- st_make_valid(borealForest)

borealForest <- st_union(borealForest[
  borealForest$TYPE == "BOREAL" | borealForest$TYPE == "B_ALPINE",
])

# use this to loop across ecoregions
eco_boreal <- st_intersection(ecoregions, borealForest)

yearRanges <- c(2000:2010, 2090:2100)

outputPath <- "~/../Downloads/outputs/outputs/"

# Create a data frame with all combinations of CO2 scenarios, and climate models
ecoregions <- unique(eco_boreal$ECOREGION)

# for each scenario x model, create a raster of NPP over the entire boreal forest
NPP_RCP45 <- combineResults(
  vars = "daily_npp",
  ecoregions = ecoregions,
  outputPath = outputPath,
  yearRange = 2091:2100,
  model = c("GCM4", "RCM4", "Hadley"),
  scenario = "RCP45"
)

NPP_RCP85 <- combineResults(
  vars = "daily_npp",
  ecoregions = ecoregions,
  outputPath = outputPath,
  yearRange = 2091:2100,
  model = c("GCM4", "RCM4", "Hadley"),
  scenario = "RCP85"
)

NPP_present <- combineResults(
  vars = "daily_npp",
  ecoregions = ecoregions,
  outputPath = outputPath,
  yearRange = 2001:2020,
  model = c("GCM4", "RCM4", "Hadley"),
  scenario = c("RCP45", "RCP85"),
  summarize = TRUE
)

writeRaster(NPP_present, "data/processed/NPP_present.tif", overwrite = TRUE)
writeRaster(NPP_RCP45, "data/processed/NPP_RCP45.tif", overwrite = TRUE)
writeRaster(NPP_RCP85, "data/processed/NPP_RCP85.tif", overwrite = TRUE)
