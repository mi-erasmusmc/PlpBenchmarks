library(testthat)
library(Eunomia)
library(PLPBenchmarks)
library(checkmate)

saveDirectory = file.path(tempdir(), "example")
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

for (i in seq_along(eunomiaDesigns)) {
  eunomiaDesigns[[i]]$sampleSettings <- list(createSampleTrainSetSettings(numberTrainSetOutcomes = 30, sampleSeed = 42))
  eunomiaDesigns[[i]]$executeSettings <- createExecuteSettings(runSplitData = T, runSampleData = T, runPreprocessData = T, runModelDevelopment = T)
}

benchmarkDesign <- createBenchmarkDesign(modelDesign = eunomiaDesigns[1:3], 
                                         databaseDetails = databaseDetails,
                                         saveDirectory = saveDirectory)

test_that("sampling works", {
  extractBenchmarkData(benchmarkDesign = benchmarkDesign, createStudyPopulation = T)
  runBenchmarkDesign(benchmarkDesign = benchmarkDesign)
  expect_equal(as.double(getBenchmarkModelPerformance(benchmarkDesign = benchmarkDesign) %>% dplyr::filter(metric == "outcomeCount") %>% dplyr::pull(Train)), as.vector(sapply(sapply(benchmarkDesign, "[[", "sampleSettings"), "[[", "numberTrainSetOutcomes")))
  expect_equal(as.double(getBenchmarkModelPerformance(benchmarkDesign = benchmarkDesign) %>% dplyr::filter(metric == "outcomeCount") %>% dplyr::pull(CV)), as.vector(sapply(sapply(benchmarkDesign, "[[", "sampleSettings"), "[[", "numberTrainSetOutcomes")))
})

for (i in seq_along(eunomiaDesigns)) {
  eunomiaDesigns[[i]]$sampleSettings <- list(createSampleTrainSetSettings(numberTrainSetOutcomes = 48, sampleSeed = 42))
  eunomiaDesigns[[i]]$executeSettings <- createExecuteSettings(runSplitData = T, runSampleData = T, runPreprocessData = T, runModelDevelopment = T)
}

saveDirectory = file.path(tempdir(), "example2")
benchmarkDesign <- createBenchmarkDesign(modelDesign = eunomiaDesigns[1:3], 
                                         databaseDetails = databaseDetails,
                                         saveDirectory = saveDirectory)

test_that("sampling works", {
  extractBenchmarkData(benchmarkDesign = benchmarkDesign, createStudyPopulation = T)
  runBenchmarkDesign(benchmarkDesign = benchmarkDesign)
  expect_equal(as.double(getBenchmarkModelPerformance(benchmarkDesign = benchmarkDesign) %>% dplyr::filter(metric == "outcomeCount") %>% dplyr::pull(Train)), as.vector(sapply(sapply(benchmarkDesign, "[[", "sampleSettings"), "[[", "numberTrainSetOutcomes")))
  expect_equal(as.double(getBenchmarkModelPerformance(benchmarkDesign = benchmarkDesign) %>% dplyr::filter(metric == "outcomeCount") %>% dplyr::pull(CV)), as.vector(sapply(sapply(benchmarkDesign, "[[", "sampleSettings"), "[[", "numberTrainSetOutcomes")))
})
