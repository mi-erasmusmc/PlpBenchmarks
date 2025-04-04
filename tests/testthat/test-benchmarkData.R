library(testthat)
library(Eunomia)
library(PLPBenchmarks)
library(checkmate)

saveDirectory = file.path(tempdir(), "example2")
seed = 42
cdmDatabaseSchema = "main"
cdmDatabaseName = "Eunomia"
cdmDatabaseId = "Eunomia"
cohortDatabaseSchema = "main"
outcomeDatabaseSchema = "main"
cohortTable = "cohort"

tasks <- viewBenchmarkTasks()
analysisDesigns <- loadModelDesigns(designs = "all")
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
benchmarkDesign <- createBenchmarkDesign(modelDesign = eunomiaDesigns, 
                                         databaseDetails = databaseDetails,
                                         saveDirectory = saveDirectory)

extractBenchmarkData(benchmarkDesign = benchmarkDesign, createStudyPopulation = FALSE)
test_that("unique plp data objects are created", {
  expect_equal(length(attributes(benchmarkDesign)$uniquePlpData$dataLocation), length(list.files(file.path(saveDirectory, "rawData"), pattern = "^GIB")))
})

extractBenchmarkData(benchmarkDesign = benchmarkDesign, createStudyPopulation = TRUE)
test_that("unique populations are created", {
  designNames <- names(benchmarkDesign)
  expect_equal(attributes(benchmarkDesign)$uniquePopulation$populationLocation, file.path(saveDirectory, "rawData", designNames, "studyPopulation") )
})

