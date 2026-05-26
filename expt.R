repos <- c("predictiveecology.r-universe.dev", getOption("repos"))
if (!require("SpaDES.project")){
  Require::Install(c("SpaDES.project", "SpaDES.core", "reproducible"), repos = repos, dependencies = TRUE)
}

projectPath <- "~/repos/BiomeBGC_BorealPredictions/"
setwd(projectPath)

# We will run Biome-BGC by ecoprovince within the canadian boreal forest
# use this to loop across ecoprovinces
ecoprovinces <- reproducible::prepInputs(url = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/province/ecoprovince_shp.zip",
                                         targetFile = "ecoprovinces.shp",
                                         destinationPath = "inputs",
                                         fun = "sf::st_read")

borealForest <- reproducible::prepInputs(url = "https://d278fo2rk9arr5.cloudfront.net/downloads/boreal.zip",
                                         targetFile = "NABoreal.shp",
                                         destinationPath = "inputs",
                                         fun = "sf::st_read")

ecoprovinces <- sf::st_transform(ecoprovinces, 3978)
borealForest <- sf::st_transform(borealForest, 3978)

ecoprovinces <- sf::st_make_valid(ecoprovinces)
borealForest <- sf::st_make_valid(borealForest)

borealForest <- sf::st_union(borealForest[borealForest$TYPE == "BOREAL" | borealForest$TYPE == "B_ALPINE", ])

# use this to loop across ecoregions
eco_boreal <- sf::st_intersection(ecoprovinces, borealForest)

# Setup the experiment
ecoprovinces <- unique(eco_boreal$ECOPROVINC)
co2scenarios <- c("RCP45", "RCP85")
climModel <- c("RCM4", "GCM4", "Hadley")

expt_df <- expand.grid(cores = 45L, ecoprovince = ecoprovinces, co2scenario = co2scenarios, climModel = climModel)
expt_df <- data.table::setorder(expt_df, ecoprovince, co2scenario)

for (i in 1:nrow(expt_df)) {
  pars <- expt_df[i, ]
  with(pars, {
    source("global.R")
  })
}

