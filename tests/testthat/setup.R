if (Sys.getenv("GITHUB_ACTIONS") == "true") {
  # Download the PostreSQL driver ---------------------------
  # If DATABASECONNECTOR_JAR_FOLDER exists, assume driver has been downloaded
  jarFolder <- Sys.getenv("DATABASECONNECTOR_JAR_FOLDER", unset = "")
  if (jarFolder == "") {
    tempJarFolder <- tempfile("jdbcDrivers")
    dir.create(tempJarFolder)
    Sys.setenv("DATABASECONNECTOR_JAR_FOLDER" = tempJarFolder)
    DatabaseConnector::downloadJdbcDrivers("postgresql")
  }
}


if (rlang::is_installed("curl")) {
  internet <- curl::has_internet()
  message("Internet: ", internet)
} else {
  internet <- FALSE
  message("Internet: ", internet)
}


  # Environment variables ####
# library(testthat)
# library(Eunomia)
# library(PLPBenchmarks)
# library(checkmate)

seed = 42
cdmDatabaseSchema = "main"
cdmDatabaseName = "Eunomia"
cdmDatabaseId = "Eunomia"
cohortDatabaseSchema = "main"
outcomeDatabaseSchema = "main"
cohortTable = "cohort"

library(PLPBenchmarks)

if (internet && rlang::is_installed("Eunomia")) {
  # PLPDATA
  connectionDetails <- Eunomia::getEunomiaConnectionDetails()
  Eunomia::createCohorts(connectionDetails)
  
  }

 ## Calling data objects ####
data("eunomiaDesigns")
data("eunomiaTasks")
data("tasks")
data("modelDesigns")
## end ####

  ## Creating cohorts ----
# connectionDetails <- getEunomiaConnectionDetails()
# createCohorts(connectionDetails = connectionDetails)

  ### PLP variables ----
databaseDetails <- PatientLevelPrediction::createDatabaseDetails(connectionDetails = connectionDetails, 
                                                                 cdmDatabaseSchema = cdmDatabaseSchema,
                                                                 cdmDatabaseName = cdmDatabaseName,
                                                                 cdmDatabaseId = cdmDatabaseId, 
                                                                 cohortDatabaseSchema = cohortDatabaseSchema,
                                                                 cohortTable = cohortTable,
                                                                 outcomeDatabaseSchema = outcomeDatabaseSchema,
                                                                 outcomeTable = cohortTable 
)

  ## Directories
saveDirectory = file.path(tempdir(), "example")
# create_local_file <- function(filename, env = parent.env()){
#   withr::local_file(file = list("filename" = ))
# }
# # withr::local_dir()
# 
# saveDirectory <- create_local_dir(directory = "example")

  ## Designs  ----
benchmarkDesign <- createBenchmarkDesign(modelDesign = modelDesigns, 
                                         databaseDetails = databaseDetails,
                                         saveDirectory = file.path(saveDirectory, "rwd_designs"))

eunomiaBenchmarkDesign <- createBenchmarkDesign(modelDesign = eunomiaDesigns, 
                                                databaseDetails = databaseDetails, 
                                                saveDirectory = file.path(saveDirectory, "eunomia_designs"))

eunomiaBenchmarkDesign4 <- createBenchmarkDesign(modelDesign = eunomiaDesigns, 
                                                databaseDetails = databaseDetails, 
                                                saveDirectory = file.path(saveDirectory, "eunomia_designs_4"))

eunomiaBenchmarkDesign_1 <- createBenchmarkDesign(modelDesign = eunomiaDesigns[1],
                                                databaseDetails = databaseDetails,
                                                saveDirectory = file.path(saveDirectory, "eunomia_design_1"))

cohortDefinitionSet <- CohortGenerator::getCohortDefinitionSet(
  settingsFileName = "testdata/name/Cohorts.csv",
  jsonFolder = "testdata/name/cohorts",
  sqlFolder = "testdata/name/sql/sql_server",
  cohortFileNameFormat = "%s",
  cohortFileNameValue = c("cohortName"),
  packageName = "CohortGenerator",
  verbose = FALSE
)

smallCohortDefinitionSet <- cohortDefinitionSet[1,]
# class(smallCohortDefinitionSet)

# test-sampling.R
eunomiaDesignsToSample <- eunomiaDesigns[1:3]

for (i in seq_along(eunomiaDesignsToSample)) {
  eunomiaDesignsToSample[[i]]$sampleSettings <- list(createSampleTrainSetSettings(numberTrainSetOutcomes = 30, sampleSeed = 42))
  eunomiaDesignsToSample[[i]]$executeSettings <- createExecuteSettings(runSplitData = T, runSampleData = T, runPreprocessData = T, runModelDevelopment = T)
}

eunomiaBenchmarkDesign_toSample <- createBenchmarkDesign(modelDesign = eunomiaDesignsToSample, 
                                                         databaseDetails = databaseDetails,
                                                         saveDirectory = file.path(saveDirectory, "eunomia_toSample_1"))

eunomiaDesignsToSample2 <- eunomiaDesigns[1:2]

for (i in seq_along(eunomiaDesignsToSample2)) {
  eunomiaDesignsToSample2[[i]]$sampleSettings <- list(createSampleTrainSetSettings(numberTrainSetOutcomes = 48, sampleSeed = 42))
  eunomiaDesignsToSample2[[i]]$executeSettings <- createExecuteSettings(runSplitData = T, runSampleData = T, runPreprocessData = T, runModelDevelopment = T)
}

eunomiaBenchmarkDesign_toSample2 <- createBenchmarkDesign(modelDesign = eunomiaDesignsToSample2, 
                                                          databaseDetails = databaseDetails,
                                                          saveDirectory = file.path(saveDirectory, "eunomia_toSample2"))

# test-getPerformance.R
eunomiaBenchmarkDesign_2 <- createBenchmarkDesign(modelDesign = eunomiaDesigns[1:2], 
                                                  databaseDetails = databaseDetails,
                                                  saveDirectory = file.path(saveDirectory, "eunomia_designs_2"))

extractBenchmarkData(benchmarkDesign = eunomiaBenchmarkDesign_2 , createStudyPopulation = T)
# runBenchmarkDesign(benchmarkDesign = eunomiaBenchmarkDesign_2)  
# res <- getBenchmarkModelPerformance(eunomiaBenchmarkDesign_2)

extractBenchmarkData(benchmarkDesign = eunomiaBenchmarkDesign_1)
# runBenchmarkDesign(benchmarkDesign = eunomiaBenchmarkDesign_1)

# register_cleanup(function() {
#   if (Sys.getenv("GITHUB_ACTIONS") == "true") {
#     # Remove the JDBC driver folder
#     jarFolder <- Sys.getenv("DATABASECONNECTOR_JAR_FOLDER", unset = "")
#     if (jarFolder != "") {
#       unlink(jarFolder, recursive = TRUE)
#     }
#   }
#   # unlink(saveLoc, recursive = TRUE)
#   if (internet && rlang::is_installed("Eunomia")) {
#     unlink(connectionDetails$server())
#   }
# })
