source("fcnt4analysis.R")
library(reproducible)
library(sf)
library(rnaturalearth)
library(ggtern)

ecod <- reproducible::prepInputs(
  url = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/district/ecodistrict_shp.zip",
  destinationPath = "inputs",
  fun = "sf::st_read"
)

ecodistricts <- reproducible::prepInputs(
  url = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/district/ecodistrict_shp.zip",
  targetFile = "ecodistricts.shp",
  destinationPath = "inputs",
  fun = "sf::st_read"
)

borealForest <- reproducible::prepInputs(
  url = "https://d278fo2rk9arr5.cloudfront.net/downloads/boreal.zip",
  targetFile = "NABoreal.shp",
  destinationPath = "inputs",
  fun = "sf::st_read"
)

ecodistricts <- sf::st_transform(ecodistricts, 3978)
borealForest <- sf::st_transform(borealForest, 3978)

ecodistricts <- sf::st_make_valid(ecodistricts)
borealForest <- sf::st_make_valid(borealForest)

borealForest <- st_union(borealForest[
  borealForest$TYPE == "BOREAL" |
    borealForest$TYPE == "B_ALPINE",
])

# use this to loop across ecodistricts
eco_boreal <- sf::st_intersection(ecodistricts, borealForest)


metDataPath <- "~/../Downloads/metdata/metdata/"


# Setup the experiment
ecodistricts <- unique(eco_boreal$ECODISTRIC)
co2scenarios <- c("RCP45", "RCP85")
climModel <- c("RCM4", "GCM4", "Hadley")

expt_df <- expand.grid(
  ecodistrict = ecodistricts,
  co2scenario = co2scenarios,
  climModel = climModel
)
expt_df <- data.table::setorder(expt_df, ecodistrict, co2scenario)

out <- data.table(
  year = c(),
  model = c(),
  scenario = c(),
  ivpd = c(),
  itmin = c(),
  iphoto = c()
)
for (i in 1:nrow(expt_df)) {
  imodel <- expt_df$climModel[i]
  iscenario <- expt_df$co2scenario[i]
  iecodistrict <- expt_df$ecodistrict[i]

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

p1 <- mapClimate(out, eco_boreal, scenario = "RCP45")
p2 <- mapClimaticControl(out, eco_boreal, scenario = "RCP45")
