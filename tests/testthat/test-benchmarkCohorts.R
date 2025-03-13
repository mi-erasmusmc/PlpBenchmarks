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
cohortTable = "test"

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
                                                                 outcomeTable = cohortTable, 
)

benchmarkDesign <- createBenchmarkDesign(modelDesign = analysisDesigns, 
                                         databaseDetails = databaseDetails,
                                         saveDirectory = saveDirectory)

eunomiaBenchmarkDesign <- createBenchmarkDesign(modelDesign = eunomiaDesigns[1],
                                                databaseDetails = databaseDetails,
                                                saveDirectory = saveDirectory )

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
class(smallCohortDefinitionSet)
test_that("function runs correctly", {
  expect_no_error(createBenchmarkCohorts(cohorts = smallCohortDefinitionSet,
                                         connectionDetails = connectionDetails, 
                                         cdmDatabaseSchema = cdmDatabaseSchema,
                                         cohortDatabaseSchema = cohortDatabaseSchema,
                                         cohortTable = cohortTable,
                                         saveDirectory = saveDirectory))
  # The functiom below builds two different cohort definitions
  expect_no_error(createBenchmarkCohorts(benchmarkDesign = eunomiaBenchmarkDesign ,
                                         connectionDetails = connectionDetails, 
                                         cdmDatabaseSchema = cdmDatabaseSchema,
                                         cohortDatabaseSchema = cohortDatabaseSchema,
                                         cohortTable = cohortTable,
                                         saveDirectory = saveDirectory))
  # The functiom below builds two different cohort definitions
  expect_no_error(createBenchmarkCohorts(cohorts = eunomiaTasks[1,],
                                         connectionDetails = connectionDetails, 
                                         cdmDatabaseSchema = cdmDatabaseSchema,
                                         cohortDatabaseSchema = cohortDatabaseSchema,
                                         cohortTable = cohortTable,
                                         saveDirectory = saveDirectory))
})

test_that("it does not accepts other cohort definitions", {
  expect_error(createBenchmarkCohorts(cohorts = 1,
                                         connectionDetails = connectionDetails, 
                                         cdmDatabaseSchema = cdmDatabaseSchema,
                                         cohortDatabaseSchema = cohortDatabaseSchema,
                                         cohortTable = cohortTable,
                                         saveDirectory = saveDirectory))
  # The functiom below builds two different cohort definitions
  expect_error(createBenchmarkCohorts(benchmarkDesign = eunomiaDesigns[[1]] ,
                                         connectionDetails = connectionDetails, 
                                         cdmDatabaseSchema = cdmDatabaseSchema,
                                         cohortDatabaseSchema = cohortDatabaseSchema,
                                         cohortTable = cohortTable,
                                         saveDirectory = saveDirectory))
  # The functiom below builds two different cohort definitions
  expect_error(createBenchmarkCohorts(benchmarkDesign = eunomiaTasks[1,],
                                         connectionDetails = connectionDetails, 
                                         cdmDatabaseSchema = cdmDatabaseSchema,
                                         cohortDatabaseSchema = cohortDatabaseSchema,
                                         cohortTable = cohortTable,
                                         saveDirectory = saveDirectory))
  expect_error(createBenchmarkCohorts(benchmarkDesign = benchmarkDesign, 
                                      cohorts = eunomiaTasks,
                                      connectionDetails = connectionDetails, 
                                      cdmDatabaseSchema = cdmDatabaseSchema,
                                      cohortDatabaseSchema = cohortDatabaseSchema,
                                      cohortTable = cohortTable,
                                      saveDirectory = saveDirectory))
  expect_error(createBenchmarkCohorts(cohorts = data.frame("cohortid" = 1, "cohortName" = "cohort1", "outcomeId" = 2),
                                      connectionDetails = connectionDetails, 
                                      cdmDatabaseSchema = cdmDatabaseSchema,
                                      cohortDatabaseSchema = cohortDatabaseSchema,
                                      cohortTable = cohortTable,
                                      saveDirectory = saveDirectory))
  expect_error(createBenchmarkCohorts(cohorts = data.frame("targetid" = 1, "cohortName" = "cohort1", "outcomeId" = 2),
                                      connectionDetails = connectionDetails, 
                                      cdmDatabaseSchema = cdmDatabaseSchema,
                                      cohortDatabaseSchema = cohortDatabaseSchema,
                                      cohortTable = cohortTable,
                                      saveDirectory = saveDirectory))
})

test_that("directories are created correctly", {
  createBenchmarkCohorts(cohorts = smallCohortDefinitionSet ,
                         connectionDetails = connectionDetails, 
                         cdmDatabaseSchema = cdmDatabaseSchema,
                         cohortDatabaseSchema = cohortDatabaseSchema,
                         cohortTable = cohortTable,
                         saveDirectory = saveDirectory)
  expect_directory(file.path(saveDirectory, "rawData"))
})

test_that("directories are created correctly 2", {
  createBenchmarkCohorts(cohorts = smallCohortDefinitionSet ,
                         connectionDetails = connectionDetails, 
                         cdmDatabaseSchema = cdmDatabaseSchema,
                         cohortDatabaseSchema = cohortDatabaseSchema,
                         cohortTable = cohortTable,
                         incremental = TRUE, 
                         rawDataFolder = file.path(dirname(saveDirectory), "rawFolder"),
                         saveDirectory = saveDirectory)
  expect_directory(file.path(dirname(saveDirectory), "rawFolder"))
  
  createBenchmarkCohorts(cohorts = smallCohortDefinitionSet ,
                         connectionDetails = connectionDetails, 
                         cdmDatabaseSchema = cdmDatabaseSchema,
                         cohortDatabaseSchema = cohortDatabaseSchema,
                         cohortTable = cohortTable,
                         incremental = TRUE, 
                         rawDataFolder = file.path(tempdir(), "someOtherFolder"),
                         saveDirectory = saveDirectory)
  expect_directory(file.path(tempdir(), "someOtherFolder"))
  
  createBenchmarkCohorts(cohorts = smallCohortDefinitionSet ,
                         connectionDetails = connectionDetails, 
                         cdmDatabaseSchema = cdmDatabaseSchema,
                         cohortDatabaseSchema = cohortDatabaseSchema,
                         cohortTable = cohortTable,
                         incremental = TRUE, 
                         rawDataFolder = file.path("someOtherFolder"),
                         saveDirectory = saveDirectory)
  expect_directory(file.path(saveDirectory, "someOtherFolder"))
})

