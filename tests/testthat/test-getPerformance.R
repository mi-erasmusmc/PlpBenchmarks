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

# tasks <- viewBenchmarkTasks()
# analysisDesigns <- loadModelDesigns(designs = 1)
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

benchmarkDesign <- createBenchmarkDesign(modelDesign = eunomiaDesigns[1:3], 
                                         databaseDetails = databaseDetails,
                                         saveDirectory = saveDirectory)
extractBenchmarkData(benchmarkDesign = benchmarkDesign, createStudyPopulation = T)
runBenchmarkDesign(benchmarkDesign = benchmarkDesign)  
res <- getBenchmarkModelPerformance(benchmarkDesign)

test_that("function works properly", {
  expect_list(res)
  expect_subset(names(res), choices = c("performanceMetrics", "executionTimes"))
  expect_error(getBenchmarkModelPerformance(eunomiaDesigns))
  expect_error(getBenchmarkModelPerformance(benchmarkDesign[[1]]))
})

test_that("plpPerformance collected properly", {
  expect_data_frame(res$performanceMetrics)
  expect_tibble(res$performanceMetrics)
  expect_subset(names(res$performanceMetrics), choices = c("analysisName", "metric", "Test", "Train", "CV"))
})

test_that("executionTimes collected properly", {
  expect_data_frame(res$executionTimes)
  expect_tibble(res$executionTimes)
  expect_subset(names(res$executionTimes), choices = c("analysisName", "TotalExecutionElapsedTime"))
})