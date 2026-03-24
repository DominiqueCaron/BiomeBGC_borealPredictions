repos <- c("predictiveecology.r-universe.dev", getOption("repos"))
if (!require("SpaDES.project")){
  Require::Install(c("SpaDES.project", "SpaDES.core", "reproducible"), repos = repos, dependencies = TRUE)
}

projectPath <- "~/repos/BiomeBGC_BorealPredictions/"
setwd(projectPath)

# We will run Biome-BGC by Ecoregions within the canadian boreal forest
# use this to loop across ecoregions
ecoregions <- reproducible::prepInputs(url = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/region/ecoregion_shp.zip",
                                       targetFile = "ecoregions.shp",
                                       destinationPath = "inputs",
                                       fun = "sf::st_read")

borealForest <- reproducible::prepInputs(url = "https://d278fo2rk9arr5.cloudfront.net/downloads/boreal.zip",
                                         targetFile = "NABoreal.shp",
                                         destinationPath = "inputs",
                                         fun = "sf::st_read")

ecoregions <- sf::st_transform(ecoregions, 3978)
borealForest <- sf::st_transform(borealForest, 3978)

ecoregions <- sf::st_make_valid(ecoregions)
borealForest <- sf::st_make_valid(borealForest)

borealForest <- sf::st_union(borealForest[borealForest$TYPE == "BOREAL" | borealForest$TYPE == "B_ALPINE", ])

# use this to loop across ecoregions
eco_boreal <- sf::st_intersection(ecoregions, borealForest)

# Setup the experiment
ecoregions <- unique(eco_boreal$ECOREGION)
co2scenarios <- c("RCP45", "RCP85")
climModel <- c("RCM4", "GCM4", "Hadley")

expt_df <- expand.grid(cores = 40L, ecoregion = ecoregions, co2scenario = co2scenarios, climModel = climModel)
expt_df <- data.table::setorder(expt_df, ecoregion, co2scenario)

for (i in 169:200) {
  pars <- expt_df[i, ]
  with(pars, {
    source("global.R")
  })
}
