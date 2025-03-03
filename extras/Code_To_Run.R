library(PLPBenchmarks)
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
server = keyring::key_get("server", keyring = KeyringConnectionDetailsName)
user = keyring::key_get("user", keyring = KeyringConnectionDetailsName)
password = keyring::key_get("password", keyring = KeyringConnectionDetailsName)

###########################################
#Do not edit 
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = password)

databaseDetails <- PatientLevelPrediction::createDatabaseDetails(connectionDetails = connectionDetails, 
                                                                 cdmDatabaseSchema = cdmDatabaseSchema,
                                                                 cdmDatabaseName = cdmDatabaseName,
                                                                 cdmDatabaseId = cdmDatabaseId, 
                                                                 cohortDatabaseSchema = cohortDatabaseSchema,
                                                                 cohortTable = cohortTable,
                                                                 outcomeDatabaseSchema = outcomeDatabaseSchema,
                                                                 outcomeTable = cohortTable 
)

cohortDefinitions <- CohortGenerator::getCohortDefinitionSet(
  settingsFileName = system.file("settings", "CohortsToCreate.csv", package = "PLPBenchmarks"),
  jsonFolder = system.file("cohorts", package = "PLPBenchmarks"),
  sqlFolder = system.file("sql", "sql_server", package = "PLPBenchmarks"))

PLPBenchmarks::createBenchmarkCohorts(cohorts = cohortDefinitions, 
                                      connectionDetails = connectionDetails, 
                                      cdmDatabaseSchema = cdmDatabaseSchema, 
                                      cohortDatabaseSchema = cohortDatabaseSchema, 
                                      cohortTable = cohortTable, 
                                      incremental = TRUE,
                                      saveDirectory = saveDirectory
)

analysisDesigns <- PLPBenchmarks::loadModelDesigns(designs = "all")

benchmarkDesign <-PLPBenchmarks::createBenchmarkDesign(modelDesign = analysisDesigns, 
                                         databaseDetails = databaseDetails,
                                         saveDirectory = saveDirectory)

PLPBenchmarks::extractBenchmarkData(benchmarkDesign = benchmarkDesign, 
                                    createStudyPopulation = TRUE)

PLPBenchmarks::runBenchmarkDesign(benchmarkDesign = benchmarkDesign)

results <- PLPBenchmarks::getBenchmarkModelPerformance(benchmarkDesign = benchmarkDesign)

PLPBenchmarks::viewBenchmarkPlpResults(benchmarkDesign = benchmarkDesign, 
                                       databaseList = list(cdmDatabaseName),
                                       cohortDefinitions = cohortDefinitions, 
                                       createPlpResultTables = T, 
                                       viewShiny = F, 
                                       databaseDirectory = dirname(saveDirectory))
