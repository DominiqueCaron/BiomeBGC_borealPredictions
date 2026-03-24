# Get climate data for boreal forest ecodistricts and calculate bioclimatic indices

# load packages and functions
source("scripts/fcnt4analysis.R")
library(reproducible)
library(sf)
library(data.table)

# extract ecodistricts in the boreal forest
ecod <- prepInputs(
  url = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/district/ecodistrict_shp.zip",
  destinationPath = "inputs",
  fun = "sf::st_read"
)

ecodistricts <- prepInputs(
  url = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/district/ecodistrict_shp.zip",
  targetFile = "ecodistricts.shp",
  destinationPath = "inputs",
  fun = "sf::st_read"
)

borealForest <- prepInputs(
  url = "https://d278fo2rk9arr5.cloudfront.net/downloads/boreal.zip",
  targetFile = "NABoreal.shp",
  destinationPath = "inputs",
  fun = "sf::st_read"
)

ecodistricts <- st_transform(ecodistricts, 3978)
borealForest <- st_transform(borealForest, 3978)

ecodistricts <- st_make_valid(ecodistricts)
borealForest <- st_make_valid(borealForest)

borealForest <- st_union(borealForest[
  borealForest$TYPE == "BOREAL" |
    borealForest$TYPE == "B_ALPINE",
])

eco_boreal <- st_intersection(ecodistricts, borealForest)

# Define the path to the climate data
metDataPath <- "~/../Downloads/metdata/metdata/"

# Create a data frame with all combinations of ecodistricts, CO2 scenarios, and climate models
ecodistricts <- unique(eco_boreal$ECODISTRIC)
co2scenarios <- c("RCP45", "RCP85")
climModel <- c("RCM4", "GCM4", "Hadley")

run_df <- expand.grid(
  ecodistrict = ecodistricts,
  co2scenario = co2scenarios,
  climModel = climModel
)
run_df <- setorder(run_df, ecodistrict, co2scenario)

# Set up an empty data table to store the results
out <- data.table(
  year = c(),
  model = c(),
  scenario = c(),
  ivpd = c(),
  itmin = c(),
  iphoto = c()
)

# Loop through each combination of ecodistrict, CO2 scenario, and climate model and process the climate data
for (i in 1:nrow(run_df)) {
  imodel <- run_df$climModel[i]
  iscenario <- run_df$co2scenario[i]
  iecodistrict <- run_df$ecodistrict[i]

  metDataFile <- file.path(
    metDataPath,
    tolower(
      paste0(iecodistrict, "_", imodel, iscenario, "_", "19732100.mtc43")
    )
  )
  if (file.exists(metDataFile)) {
    metData <- try(metRead(metDataFile) |> as.data.table(), silent = TRUE)
    if (inherits(metData, "data.table")) {
      metData <- metData[year %in% c(2000:2010, 2090:2100)]

      # calculate bioclimatic index
      metData$ivpd <- iVPD(metData$vpd)
      metData$itmin <- iTmin(metData$tmin)
      metData$iphoto <- iPhoto(metData$daylen / 3600)

      metData <- metData[,
        .(
          tmin = mean(tmin),
          prcp = sum(prcp),
          ivpd = sum(ivpd),
          itmin = sum(itmin),
          iphoto = sum(iphoto)
        ),
        by = .(year)
      ]

      out <- rbind(
        out,
        data.table(
          ecodistrict = iecodistrict,
          year = metData$year,
          model = imodel,
          scenario = iscenario,
          tmin = metData$tmin,
          prcp = metData$prcp,
          ivpd = metData$ivpd,
          itmin = metData$itmin,
          iphoto = metData$iphoto
        )
      )
    }
  }
}

# create a new folder data/processed if it doesn't exist
if (!dir.exists("data/processed")) {
  dir.create("data/processed", recursive = TRUE)
}
fwrite(out, "data/processed/climate_bioclimatic_indices.csv")
