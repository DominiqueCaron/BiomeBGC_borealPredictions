###
###
# This script runs Biome-BGC for a random point in Canadian boreal forest
###
###

repos <- c("predictiveecology.r-universe.dev", getOption("repos"))
if (!require("SpaDES.project")){
  Require::Install(c("SpaDES.project", "SpaDES.core", "reproducible"), repos = repos, dependencies = TRUE)
}

out <- SpaDES.project::setupProject(
  paths = list(projectPath = getwd(),
               inputPath = "~/inputs",
               outputPath = "outputs",
               cachePath = "cache"),
  options = options(
    repos = c(repos = repos),
    Require.cloneFrom = Sys.getenv("R_LIBS_USER"),
    spades.moduleCodeChecks = FALSE,
    spades.recoveryMode = FALSE),
  times = list(start = 2013, end = 2100),
  modules = c(
    "PredictiveEcology/BiomeBGC_dataPrep@main",
    "PredictiveEcology/BiomeBGC_core@main"
  ),
  useGit = FALSE,
  studyArea = {
    ecod <- reproducible::prepInputs(
      url = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/district/ecodistrict_shp.zip",
      destinationPath = "inputs",
      fun = "terra::vect",
      projectTo = SpaDES.tools::randomStudyArea()
    )
    ecod <- ecod[ecod$ECODISTRIC == 935, ]
    ecod <- terra::buffer(ecod, -250)
    ecod
  },
  rasterToMatch = {
    targetCRS <- terra::crs(studyArea)
    rtm <- terra::rast(studyArea, res = c(250, 250))
    terra::crs(rtm) <- targetCRS
    rtm[] <- 1
    rtm <- terra::mask(rtm, studyArea)
    rtm
  },
  params = list(
    BiomeBGC_dataPrep = list(siteNames = "test",
                             co2scenario = "RCP85",
                             climModel = "RCM4"),
    BiomeBGC_core = list(parallel.cores = 20L,
                         bbgcPath = "biomeBGCtmp",
                         returnDailyEstimates = FALSE,
                         returnMonthlyEstimates = FALSE)
  )
)

out$loadOrder <- unlist(out$modules)

initOut <- SpaDES.core::simInit2(out)
simOut <- SpaDES.core::spades(initOut)
