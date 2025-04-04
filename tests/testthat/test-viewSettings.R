library(testthat)
library(Eunomia)
library(PLPBenchmarks)
library(checkmate)

saveDirectory = file.path(tempdir(), "example4")
seed = 42
cdmDatabaseSchema = "main"
cdmDatabaseName = "Eunomia"
cdmDatabaseId = "Eunomia"
cohortDatabaseSchema = "main"
outcomeDatabaseSchema = "main"
cohortTable = "cohort"

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

benchmarkDesign <- createBenchmarkDesign(modelDesign = eunomiaDesigns[1], 
                                         databaseDetails = databaseDetails,
                                         saveDirectory = saveDirectory)

test_that("view works", {
  expect_no_error(viewBenchmarkSettings(benchmarkDesign = benchmarkDesign))
  expect_data_frame(viewBenchmarkSettings(benchmarkDesign = benchmarkDesign))
  expect_equal(ncol(viewBenchmarkSettings(benchmarkDesign = benchmarkDesign)), length(benchmarkDesign)+2)
})
