#' @export
createBenchmarkCohorts <-  function(jsonfileLocation, 
                                    cohortsToCreate,
                                    connectionDetails, 
                                    cdmDatabaseSchema, 
                                    cohortDatabaseSchema, 
                                    cohortTable, 
                                    saveDirectory){
  
  if (is.null(jsonfileLocation) && is.null(cohortsToCreate)){
    stop("jsonfileLocation and cohortsToCreate are not specified. Define one of the two.")
  }
  
  # Create cohorts
  if (!is.null(jsonfileLocation) && is.null(cohortsToCreate)){
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
  }
  
  if (!is.null(cohortsToCreate) && is.null(jsonfileLocation)){
    
    cohortJsonFiles <- cohortsToCreate$jsonLocation
    
    for (i in 1:length(cohortJsonFiles)) {
      cohortJsonFileName <- cohortJsonFiles[i]
      cohortName <- tools::file_path_sans_ext(basename(cohortJsonFileName))
      cohortJson <- readChar(cohortJsonFileName, file.info(cohortJsonFileName)$size)
      cohortExpression <- CirceR::cohortExpressionFromJson(cohortJson)
      cohortSql <- CirceR::buildCohortQuery(cohortExpression, options = CirceR::createGenerateOptions(generateStats = FALSE))
      # cohortsToCreate$sql[i] <- cohortSql
      # cohortsToCreate$sql[i] <- rbind(cohortsToCreate, data.frame(cohortId = i,
      #                                                      cohortName = cohortName, 
      #                                                      sql = cohortSql,
      #                                                      stringsAsFactors = FALSE))
      cohortsToCreate$cohortName[i] <- cohortName
      cohortsToCreate$sql[i] <- cohortSql
    }
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
  
  ParallelLogger::logInfo(paste("Cohorts created."))
  ParallelLogger::logInfo(paste(cohortCounts))
  
  utils::write.csv(cohortCounts, file.path(saveDirectory, "rawData", paste0("cohortCounts_", gsub("-", "", Sys.Date()), ".csv")))
  }