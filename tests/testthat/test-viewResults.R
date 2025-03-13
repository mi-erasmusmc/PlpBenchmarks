library(PLPBenchmarks)
library(testthat)
library(checkmate)
library(Eunomia)

saveDirectory = file.path(tempdir(), "example5")
seed = 42
cdmDatabaseSchema = "main"
cdmDatabaseName = "Eunomia"
cdmDatabaseId = "Eunomia"
cohortDatabaseSchema = "main"
outcomeDatabaseSchema = "main"
cohortTable = "cohort"

eunomiaTasks <- read.csv(system.file(package = "PLPBenchmarks", "extdata", "EunomiaProblemSpecification.csv")) 
eunomiaDesigns <-  readRDS(system.file(package = "PLPBenchmarks", "extdata", "EunomiaBenchmarkDesignList.Rds"))
connectionDetails <- getEunomiaConnectionDetails()
databaseDetails <- PatientLevelPrediction::createDatabaseDetails(connectionDetails = connectionDetails, 
                                                                 cdmDatabaseSchema = cdmDatabaseSchema,
                                                                 cdmDatabaseName = cdmDatabaseName,
                                                                 cdmDatabaseId = cdmDatabaseId, 
                                                                 cohortDatabaseSchema = cohortDatabaseSchema,
                                                                 cohortTable = cohortTable,
                                                                 outcomeDatabaseSchema = outcomeDatabaseSchema,
                                                                 outcomeTable = cohortTable 
)
Eunomia::createCohorts(connectionDetails = connectionDetails)

benchmarkDesign <- createBenchmarkDesign(modelDesign = eunomiaDesigns[1], 
                                         databaseDetails = databaseDetails,
                                         saveDirectory = saveDirectory)
extractBenchmarkData(benchmarkDesign = benchmarkDesign, createStudyPopulation = F)
runBenchmarkDesign(benchmarkDesign = benchmarkDesign)
cohortDefinitions <- dplyr::tibble(cohortId = c(1:5), cohortName = eunomiaTasks$analysisName )

test_that("viewBenchmarkResults works", {
 expect_no_error(viewBenchmarkResults(benchmarkDesign = benchmarkDesign, databaseDirectory = saveDirectory, viewShiny = F))
  expect_directory(file.path(saveDirectory, "sqlite"))
  expect_file(file.path(saveDirectory, "sqlite", "databaseFile.sqlite"))
  expect_error(viewBenchmarkResults(eunomiaDesigns[1], databaseDirectory = saveDirectory))
  expect_error(viewBenchmarkResults(benchmarkDesign))
})
