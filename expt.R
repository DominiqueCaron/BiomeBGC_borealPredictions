repos <- c("predictiveecology.r-universe.dev", getOption("repos"))
if (!require("SpaDES.project")){
  Require::Install(c("SpaDES.project", "SpaDES.core", "reproducible"), repos = repos, dependencies = TRUE)
}

projectPath <- "~/repos/BiomeBGC_BorealPredictions/"
setwd(projectPath)

# We will run Biome-BGC by Ecoregions within the canadian boreal forest
borealForest <- reproducible::prepInputs(url = "https://d278fo2rk9arr5.cloudfront.net/downloads/boreal.zip",
                           targetFile = "boreal.shp",
                           destinationPath = "inputs",
                           fun = "terra::vect")
ecoregions <- reproducible::prepInputs(url = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/region/ecoregion_shp.zip",
                         targetFile = "ecoregion_shp.shp",
                         destinationPath = "inputs",
                         fun = "terra::vect")

borealEcoregions <- terra::intersect(ecoregions, borealForest)



# Setup the experiment
cores <- 47
ecoRegions <-
co2scenarios <- c("RCP45", "RCP85")
climModel <- c("RCM4", "GCM4", "Hadley")

