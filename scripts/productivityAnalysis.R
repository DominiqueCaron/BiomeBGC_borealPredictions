library(reproducible)
library(sf)
library(terra)
library(data.table)
source("fcnt4analysis.R")

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

# Setup the experiment
ecoregions <- unique(eco_boreal$ECOREGION)
co2scenarios <- c("RCP45", "RCP85")
climModel <- c("RCM4", "GCM4", "Hadley")

expt_df <- expand.grid(
  co2scenario = co2scenarios,
  climModel = climModel
)

futureNPPRCM4RCP45 <- combineResults(
  vars = "daily_npp",
  ecoregions,
  outputPath,
  2090:2100,
  "RCM4",
  "RCP45"
)
futureNPPGCM4CRP45 <- combineResults(
  vars = "daily_npp",
  ecoregions,
  outputPath,
  2090:2100,
  "GCM4",
  "RCP45"
)
futureNPPHadlyRCP45 <- combineResults(
  vars = "daily_npp",
  ecoregions,
  outputPath,
  2090:2100,
  "Hadley",
  "RCP45"
)

presentNPP <- combineResults(
  vars = "daily_npp",
  ecoregions,
  outputPath,
  2000:2020,
  "RCM4",
  "RCP45"
)
writeRaster(futureNPP, "futureNPP.tif")
