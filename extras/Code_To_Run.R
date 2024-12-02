library(PLPBenchmarks)
library(keyring)
library(renv)

renv::activate(profile = "benchmarkAnalysis")
renv::restore()
devtools::install()

###########################################
#User specified input
saveDirectory = "directory to save the results"
requiredTrainPositiveEvents = 3500
seed = 42
cdmDatabaseSchema = "your cdm database schema"
cdmDatabaseName = "name of your database"
cdmDatabaseId = "your_database_Id"
cohortDatabaseSchema = Sys.getenv("sqlSchema")
outcomeDatabaseSchema = Sys.getenv("sqlSchema")
cohortTable = "benchmark_models"


dbms = "dbms of your database"
KeyringConnectionDetailsName = "the keyring name for the connection Details"
###########################################
#Do not edit 

server = keyring::key_get("server", keyring = "ccaeConncectionDetails")
user = keyring::key_get("user", keyring = "ccaeConncectionDetails")
password = keyring::key_get("password", keyring = "ccaeConncectionDetails")

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = password)

PLPBenchmarks::createBenchmarkCohorts(
  cohortsToCreate = read.csv(system.file(package = "PLPBenchmarks", "settings", "CohortsToCreate.csv")), 
  connectionDetails = connectionDetails, 
  cdmDatabaseSchema = cdmDatabaseSchema, 
  cohortDatabaseSchema = cohortDatabaseSchema, 
  cohortTable = cohortTable
)

databaseDetails <- PatientLevelPrediction::createDatabaseDetails(connectionDetails = connectionDetails, 
                                                                 cdmDatabaseSchema = cdmDatabaseSchema,
                                                                 cdmDatabaseName = cdmDatabaseName,
                                                                 cdmDatabaseId = cdmDatabaseId, 
                                                                 cohortDatabaseSchema = cohortDatabaseSchema,
                                                                 cohortTable = cohortTable,
                                                                 outcomeDatabaseSchema = outcomeDatabaseSchema,
                                                                 outcomeTable = cohortTable 
)

analysisDesigns <- readRDS(system.file(package = "PLPBenchmarks", "extdata", "BenchmarkModelDesignList.Rds"))

## Adding sampling to restrict number of train outcomes to 3500
analysisDesigns2 <- analysisDesigns
identical(analysisDesigns2, analysisDesigns)

for (i in seq_along(analysisDesigns2)) {
  
  analysisDesigns2[[i]]$sampleSettings <- PatientLevelPrediction::createSampleSettings(type = "reduceTrainOutcomes", numberTrainOutcomes = 3500, sampleSeed = 42)
  analysisDesigns2[[i]]$executeSettings <- PatientLevelPrediction::createExecuteSettings(runSplitData = T, 
                                                                                         runSampleData = T,
                                                                                         runfeatureEngineering = F,
                                                                                         runPreprocessData = T, 
                                                                                         runModelDevelopment = T, 
                                                                                         runCovariateSummary = T)
}


benchmarkDesign <- createBenchmarkDesign(modelDesign = analysisDesigns2, 
                                         databaseDetails = databaseDetails,
                                         saveDirectory = saveDirectory)

PLPBenchmarks::extractBenchmarkData(benchmarkDesign = benchmarkDesign, createStudyPopulation = TRUE)

PLPBenchmarks::runBenchmarkDesign(benchmarkDesign = benchmarkDesign)





