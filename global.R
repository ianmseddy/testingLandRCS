
getOrUpdatePkg <- function(p, minVer, repo) {
  if (!isFALSE(try(packageVersion(p) < minVer, silent = TRUE) )) {
    if (missing(repo)) repo = c("predictiveecology.r-universe.dev", getOption("repos"))
    install.packages(p, repos = repo)
  }
}

#you want as little requird packages here as possible
# remotes::install_github("PredictiveEcology/Require@simplify2")

getOrUpdatePkg("SpaDES.project", "0.0.8.9038")
getOrUpdatePkg("LandR", "1.1.0.9079") #this just makes sure you have some eastern species in sppEquiv

################ SPADES CALL
#this assumes you have the LandR package (my fault).
speciesOfConcern <- c("Pice_mar", "Pice_gla", "Popu_tre",
                      "Pinu_con", "Betu_pap", "Pinu_ban")
sppEquiv <- LandR::sppEquivalencies_CA
sppEquiv <- sppEquiv[LandR %in% speciesOfConcern,]


library(SpaDES.project)
out <- SpaDES.project::setupProject(
  runName = "testingLandRCS",
  updateRprofile = TRUE, #TODO: Eliot
  Restart = TRUE, #TODO: ask Eliot
  paths = list(projectPath = "testingLandRCS",
               modulePath = file.path("modules"),
               cachePath = file.path("cache"),
               scratchPath = tempdir(),
               inputPath = file.path("inputs"),
               outputPath = file.path("outputs")
  ),
  modules = c("PredictiveEcology/canClimateData@newClimate",
              "PredictiveEcology/Biomass_core@newClimate",
              "PredictiveEcology/Biomass_borealDataPrep@development",
              "ianmseddy/gmcsDataPrep@NewBrunswick"
  ),
  options = list(spades.allowInitDuringSimInit = TRUE,
                 LandR.assertions = FALSE,
                 spades.moduleCodeChecks = FALSE,
                 reproducible.shapefileRead = "terra::vect", #required if gadm is down as terra:project won't work on sf
                 spades.recoveryMode = 1
  ),
  times = list(start = 2011, end = 2021),
  params = list(
    .globals = list(.studyAreaName = "randomSA",
                    dataYear = 2011,
                    sppEquivCol = 'LandR'),
    Biomass_core = list("growthAndMortalityDrivers" = "LandR.CS")
  ),
  objects = list(studyArea = terra::vect("inputs/randomSA.shp"),
                 rasterToMatch = terra::rast("inputs/randomRTM.tif"),
                 studyAreaLarge = terra::vect("inputs/randomSAL.shp"),
                 rasterToMatchLarge = terra::rast("inputs/randomRTML.tif"),
                 sppEquiv = sppEquiv,
                 cceArgs = list(projectedClimateRasters = quote(projectedClimateRasters),
                                historicalClimateRasters = quote(historicalClimateRasters),
                                mcsModel = quote(mcsModel),
                                gcsModel = quote(gcsModel)
                 )
  ),
  require = c("PredictiveEcology/reproducible@modsForLargeArchives (HEAD)"),
  useGit = FALSE
)

#make sure you can debug this issue with parallel

googledrive::drive_auth("ianmseddy@gmail.com")
pkgload::load_all("~/git/PSPclean")
pkgload::load_all("~/git/LandR.CS")
inSim <- SpaDES.core::simInitAndSpades(objects = out$objects, params = out$params,
                                       modules = out$modules,
                                       times = out$times, paths = out$paths)
