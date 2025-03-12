library(testthat)
library(Eunomia)
library(PLPBenchmarks)
library(checkmate)

saveDirectory = file.path(tempdir(), "example3")
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


test_that("runs without population creation", {
  designNames <- names(benchmarkDesign)
  expect_no_error(runBenchmarkDesign(benchmarkDesign = benchmarkDesign)) 
  expect_equal(as.vector(sapply(benchmarkDesign, "[[", "saveDirectory")), file.path(saveDirectory, designNames))
  expect_directory(x = file.path(saveDirectory, designNames, "plpResult"))
  expect_file(x = file.path(saveDirectory, designNames, "plpResult", "runPlp.Rds"))
})

