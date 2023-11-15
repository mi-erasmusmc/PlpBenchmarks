#User specified input
cdmDatabaseSchema = ""
cohortDatabaseSchema = ""
cdmDatabaseName = ""

dbms = "postgresql"
user = ""
pw = ""
server = ""
port = 5432
  
cohortTable = "benchmark_models"

#choose which benchmarks to run
runHfint2dm <- TRUE
runCoverModels <- TRUE
runRCRI <- TRUE
runAfib <- TRUE


#Do not edit 
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms <- dbms,
                                                                user <- user,
                                                                pw <- pw,
                                                                server <- server,
                                                                port = port)

# Create cohorts
cohortsToCreate <- CohortGenerator::createEmptyCohortDefinitionSet()
cohortJsonFiles <- list.files("./inst/cohorts", full.names = TRUE)
for (i in 1:length(cohortJsonFiles)) {
  cohortJsonFileName <- cohortJsonFiles[i]
  cohortName <- tools::file_path_sans_ext(basename(cohortJsonFileName))
  cohortJson <- readChar(cohortJsonFileName, file.info(cohortJsonFileName)$size)
  cohortExpression <- CirceR::cohortExpressionFromJson(cohortJson)
  cohortSql <- CirceR::buildCohortQuery(cohortExpression, options = CirceR::createGenerateOptions(generateStats = FALSE))
  cohortsToCreate <- rbind(cohortsToCreate, data.frame(cohortId = i,
                                                       cohortName = cohortName, 
                                                       sql = cohortSql,
                                                       stringsAsFactors = FALSE))
}

# Create the cohort tables to hold the cohort generation results
cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = cohortTable)
CohortGenerator::createCohortTables(connectionDetails = connectionDetails,
                                    cohortDatabaseSchema = cohortDatabaseSchema,
                                    cohortTableNames = cohortTableNames)
# Generate the cohorts
cohortsGenerated <- CohortGenerator::generateCohortSet(connectionDetails = connectionDetails,
                                                       cdmDatabaseSchema = cdmDatabaseSchema,
                                                       cohortDatabaseSchema = cohortDatabaseSchema,
                                                       cohortTableNames = cohortTableNames,
                                                       cohortDefinitionSet = cohortsToCreate)

# Get the cohort counts
cohortCounts <- CohortGenerator::getCohortCounts(connectionDetails = connectionDetails,
                                                 cohortDatabaseSchema = cohortDatabaseSchema,
                                                 cohortTable = cohortTableNames$cohortTable)
print(cohortCounts)

### run diabetes heartfailure benchmark
if(runHfint2dm){
  runBenchmarkModel(targetId = 2,
                    outcomeId = 1,
                    analysisId = 1000,
                    analysisName = "hfint2dm", 
                    riskWindowStart = 1,
                    riskWindowEnd = 365,
                    connectionDetails = connectionDetails,
                    cdmDatabaseSchema = cdmDatabaseSchema,
                    cohortDatabaseSchema = cohortDatabaseSchema,
                    cohortTable = cohortTable
                    )
}

if(runCoverModels){
  runBenchmarkModel(targetId = 7,
                    outcomeId = 8,
                    analysisId = 1001,
                    analysisName = "coverF", 
                    riskWindowStart = 1,
                    riskWindowEnd = 30,
                    connectionDetails = connectionDetails,
                    cdmDatabaseSchema = cdmDatabaseSchema,
                    cohortDatabaseSchema = cohortDatabaseSchema,
                    cohortTable = cohortTable
  )
  
  runBenchmarkModel(targetId = 7,
                    outcomeId = 9,
                    analysisId = 1002,
                    analysisName = "coverI", 
                    riskWindowStart = 1,
                    riskWindowEnd = 30,
                    connectionDetails = connectionDetails,
                    cdmDatabaseSchema = cdmDatabaseSchema,
                    cohortDatabaseSchema = cohortDatabaseSchema,
                    cohortTable = cohortTable
  )
  
  runBenchmarkModel(targetId = 7,
                    outcomeId = 10,
                    analysisId = 1003,
                    analysisName = "coverH", 
                    riskWindowStart = 1,
                    riskWindowEnd = 30,
                    connectionDetails = connectionDetails,
                    cdmDatabaseSchema = cdmDatabaseSchema,
                    cohortDatabaseSchema = cohortDatabaseSchema,
                    cohortTable = cohortTable
  )
 }  

if(runRCRI){
  runBenchmarkModel(targetId = 3,
                    outcomeId = 5,
                    analysisId = 1004,
                    analysisName = "MNCSwithCreatine", 
                    riskWindowStart = 1,
                    riskWindowEnd = 30,
                    connectionDetails = connectionDetails,
                    cdmDatabaseSchema = cdmDatabaseSchema,
                    cohortDatabaseSchema = cohortDatabaseSchema,
                    cohortTable = cohortTable
  )
  runBenchmarkModel(targetId = 4,
                    outcomeId = 5,
                    analysisId = 1005,
                    analysisName = "MNCSwithoutCreatine", 
                    riskWindowStart = 1,
                    riskWindowEnd = 30,
                    connectionDetails = connectionDetails,
                    cdmDatabaseSchema = cdmDatabaseSchema,
                    cohortDatabaseSchema = cohortDatabaseSchema,
                    cohortTable = cohortTable
  )

}

if (runAfib){
  runBenchmarkModel(targetId = 11,
                    outcomeId = 12,
                    analysisId = 1006,
                    analysisName = "strokeInAfib", 
                    riskWindowStart = 1,
                    riskWindowEnd = 365,
                    connectionDetails = connectionDetails,
                    cdmDatabaseSchema = cdmDatabaseSchema,
                    cohortDatabaseSchema = cohortDatabaseSchema,
                    cohortTable = cohortTable
  )
}
