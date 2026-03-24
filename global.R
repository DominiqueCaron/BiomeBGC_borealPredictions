###
###
# This script runs Biome-BGC for a random point in Canadian boreal forest
###
###

repos <- c("https://predictiveecology.r-universe.dev", getOption("repos"))
if (!exists("out")){
  out <- SpaDES.project::setupProject(
  paths = list(projectPath = getwd(),
               inputPath = "~/inputs",
               outputPath = file.path("outputs", pars[,"ecoregion"], pars[,"co2scenario"], pars[,"climModel"]),
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
  studyArea = terra::vect(eco_boreal[eco_boreal$ECOREGION == pars[,"ecoregion"],]),
  rasterToMatch = {
    rtm <- terra::rast(studyArea, res = c(250, 250))
    terra::crs(rtm) <- terra::crs(studyArea)
    rtm[] <- 1
    rtm <- terra::mask(rtm, studyArea, touches = FALSE)
    rtm
  },
  params = list(
    BiomeBGC_dataPrep = list(siteNames = "test",
                             co2scenario = pars[,"co2scenario"],
                             climModel = pars[,"climModel"],
                             savePixelGroupMap = TRUE),
    BiomeBGC_core = list(parallel.cores = pars[,"cores"],
                         bbgcPath = "biomeBGCtmp",
                         returnDailyEstimates = FALSE,
                         returnMonthlyEstimates = FALSE,
                         saveYears = c(2013, 2100))
  )
  ) } else {
    out$studyArea = terra::vect(eco_boreal[eco_boreal$ECOREGION == pars[,"ecoregion"],])
    out$rasterToMatch = {
      rtm <- terra::rast(out$studyArea, res = c(250, 250))
      terra::crs(rtm) <- terra::crs(out$studyArea)
      rtm[] <- 1
      rtm <- terra::mask(rtm, out$studyArea, touches = FALSE)
      rtm
    }
    out$params$BiomeBGC_dataPrep$climModel <- pars[,"climModel"]
    out$params$BiomeBGC_dataPrep$co2scenario <- pars[,"co2scenario"]
    out$paths$outputPath <- file.path("outputs", pars[,"ecoregion"], pars[,"co2scenario"], pars[,"climModel"])
}

out$loadOrder <- unlist(out$modules)

initOut <- SpaDES.core::simInit2(out)
simOut <- SpaDES.core::spades(initOut)
