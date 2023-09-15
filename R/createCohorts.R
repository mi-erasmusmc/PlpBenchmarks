#' Instantiates a set of cohorts form a folder containing the JSON cohort defintions
#'
#' @details
#' The user specifies the location of the JSON definitions as well as the connection details for the data abse
#' @param jsonfileLocation, 
#' @param connectionDetails, 
#' @param cdmDatabaseSchema, 
#' @param cohortDatabaseSchema, 
#' @param cohortTable
#'
#' @return
#' NULL
#'
#' @export

createCohorts <- function(jsonfileLocation, 
                          connectionDetails, 
                          cdmDatabaseSchema, 
                          cohortDatabaseSchema, 
                          cohortTable){
  # Create cohorts
  cohortsToCreate <- CohortGenerator::createEmptyCohortDefinitionSet()
  cohortJsonFiles <- list.files(jsonfileLocation, full.names = TRUE)
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
  }