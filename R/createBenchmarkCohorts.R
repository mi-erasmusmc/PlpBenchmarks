# Instantiates a set of cohorts form a folder containing the JSON cohort definitions
#
# @details
# The user specifies the location of the JSON definitions as well as the connection details for the data abse
# @param jsonfileLocation The location of the json files for the cohorts to be created. Either \code{jsonFileLocation} or \code{cohortdToCreate} need to be specified.
# @param cohortsToCreate A data frame with a column named jsonLocation. Either \code{jsonFileLocation} or \code{cohortdToCreate} need to be specified.
# @param connectionDetails The connection details object to connect to the database
# @param cdmDatabaseSchema The schema name where the data is located. 
# @param cohortDatabaseSchema The schema where the cohort's will be stored.  
# @param cohortTable The name of the table under which the cohort/s will be created
# @param saveDirectory Directory to save cohort counts. 
#
# @return
# NULL
#
# @export
# createBenchmarkCohorts <-  function(jsonfileLocation = NULL, 
#                                     cohortsToCreate = NULL,
#                                     connectionDetails, 
#                                     cdmDatabaseSchema, 
#                                     cohortDatabaseSchema, 
#                                     cohortTable, 
#                                     saveDirectory){
#   
#   if (is.null(jsonfileLocation) && is.null(cohortsToCreate)){
#     stop("jsonfileLocation and cohortsToCreate are not specified. Define one of the two.")
#   }
#   
#   if (dir.exists(file.path(saveDirectory, "rawData")) == F){ 
#     dir.create(file.path(saveDirectory, "rawData"), recursive = T)
#   }
#   
#   # Create cohorts
#   if (!is.null(jsonfileLocation) && is.null(cohortsToCreate)){
#     cohortsToCreate <- CohortGenerator::createEmptyCohortDefinitionSet()
#     cohortJsonFiles <- list.files(jsonfileLocation, full.names = TRUE)
#     
#     for (i in 1:length(cohortJsonFiles)) {
#       cohortJsonFileName <- cohortJsonFiles[i]
#       cohortName <- tools::file_path_sans_ext(basename(cohortJsonFileName))
#       cohortJson <- readChar(cohortJsonFileName, file.info(cohortJsonFileName)$size)
#       cohortExpression <- CirceR::cohortExpressionFromJson(cohortJson)
#       cohortSql <- CirceR::buildCohortQuery(cohortExpression, options = CirceR::createGenerateOptions(generateStats = FALSE))
#       cohortsToCreate <- rbind(cohortsToCreate, data.frame(cohortId = i,
#                                                            cohortName = cohortName,
#                                                            sql = cohortSql,
#                                                            stringsAsFactors = FALSE))
#     }
#   }
#   
#   if (!is.null(cohortsToCreate) && is.null(jsonfileLocation)){
#     
#     cohortJsonFiles <- file.path(system.file(package = "PLPBenchmarks", "cohorts"), paste0(cohortsToCreate$cohort_name, ".json"))
#     
#     for (i in 1:length(cohortJsonFiles)) {
#       cohortJsonFileName <- cohortJsonFiles[i]
#       cohortName <- tools::file_path_sans_ext(basename(cohortJsonFileName))
#       cohortJson <- readChar(cohortJsonFileName, file.info(cohortJsonFileName)$size)
#       cohortExpression <- CirceR::cohortExpressionFromJson(cohortJson)
#       cohortSql <- CirceR::buildCohortQuery(cohortExpression, options = CirceR::createGenerateOptions(generateStats = FALSE))
#       # cohortsToCreate$sql[i] <- cohortSql
#       # cohortsToCreate$sql[i] <- rbind(cohortsToCreate, data.frame(cohortId = i,
#       #                                                      cohortName = cohortName, 
#       #                                                      sql = cohortSql,
#       #                                                      stringsAsFactors = FALSE))
#       cohortsToCreate$cohortName[i] <- cohortName
#       cohortsToCreate$sql[i] <- cohortSql
#     }
#   }
#   
#   # Create the cohort tables to hold the cohort generation results
#   cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = cohortTable)
#   CohortGenerator::createCohortTables(connectionDetails = connectionDetails,
#                                       cohortDatabaseSchema = cohortDatabaseSchema,
#                                       cohortTableNames = cohortTableNames)
#   # Generate the cohorts
#   cohortsGenerated <- CohortGenerator::generateCohortSet(connectionDetails = connectionDetails,
#                                                          cdmDatabaseSchema = cdmDatabaseSchema,
#                                                          cohortDatabaseSchema = cohortDatabaseSchema,
#                                                          cohortTableNames = cohortTableNames,
#                                                          cohortDefinitionSet = cohortsToCreate)
#   
#   # Get the cohort counts
#   cohortCounts <- CohortGenerator::getCohortCounts(connectionDetails = connectionDetails,
#                                                    cohortDatabaseSchema = cohortDatabaseSchema,
#                                                    cohortTable = cohortTableNames$cohortTable)
#   
#   ParallelLogger::logInfo(paste("Cohorts created."))
#   ParallelLogger::logInfo(paste(cohortCounts))
#   
#   utils::write.csv(cohortCounts, file.path(saveDirectory, "rawData", paste0("cohortCounts_", gsub("-", "", Sys.Date()), ".csv")))
# }

#' #' Instantiates a set of cohorts 
#' 
#' @details
#' The user specifies either `cohorts` or inserts a `benchmarkDesign` object to create the desired cohorts.
#' @param cohorts A data frame with a column named `cohortId` or a data frame with two columns named `targetId` and `outcomeId`. 
#' This can be a data frame such as provided by \code{CohortGenerator::getCohortDefinitionSet()} or a similar data frame such as the one obtained with \code{viewBenchmarkTasks()}.
#' Either \code{cohorts} or \code{benchmarkDesign} need to be specified.
#' @param benchmarkDesign A list of class `benchmarkDesign` as generated by \code{createBenchmarkDesign()}.
#' @param connectionDetails The connection details object to connect to the database.
#' @param cdmDatabaseSchema The schema name where the data is located. 
#' @param cohortDatabaseSchema The schema where the cohort's will be stored.  
#' @param cohortTable The name of the table under which the cohort/s will be created
#' @param incremental A logical to define whether the `cohortTable` will be regenerated if it already exists, 
#' or whether to regenerated all cohorts or only those that have yet been generated. 
#' @param saveDirectory Directory to save cohort counts and/or which cohorts have been generated. 
#'
#' @return
#' NULL
#'
#' @export
createBenchmarkCohorts <-  function(cohorts = NULL,
                                    benchmarkDesign = NULL, 
                                    connectionDetails, 
                                    cdmDatabaseSchema, 
                                    cohortDatabaseSchema, 
                                    cohortTable, 
                                    incremental = FALSE,
                                    saveDirectory){
  
  assertColumns <- checkmate::makeAssertCollection()
  
  checkmate::assertDataFrame(
    x = cohorts,
    types = c("integerish", "character", "double"),
    any.missing = TRUE,
    all.missing = FALSE,
    min.cols = 1,
    min.rows = 1,
    null.ok = TRUE,
    col.names = "named",
    add = assertColumns,
    .var.name = "cohorts"
  )
  
  checkmate::assertList(
    x = benchmarkDesign,
    min.len = 1, 
    null.ok = TRUE, 
    add = assertColumns, 
    .var.name = "benchmarkDesign")
  
  if(is.null(cohorts) && is.null(benchmarkDesign)){
    stop(message("Either cohorts or benchmarkDesign should be defined. Please specify one of the two."))
  }
  
  checkmate::assertSubset(
    x = names(cohorts),
    choices = c("cohortId", "targetId"),
    add = assertColumns,
    .var.name = "cohorts"
  )
  
  checkmate::assertCharacter(
    x = cdmDatabaseSchema,
    null.ok = TRUE,
    len = 1,
    add = assertColumns,
    .var.name = "cdmDatabaseSchema"
  )
  
  checkmate::assertClass(
    x = connectionDetails,
    classes = "ConnectionDetails",
    null.ok = TRUE,
    add = assertColumns,
    .var.name = "connectionDetails"
  )
  
  checkmate::assertCharacter(
    x = connectionDetails$dbms,
    len = 1,
    null.ok = TRUE,
    add = assertColumns,
    .var.name = "connectionDetailsDbms"
  )
  
  checkmate::assertCharacter(
    x = cohortTable,
    null.ok = TRUE,
    len = 1,
    add = assertColumns,
    .var.name = "cohortTable"
  )
  
  checkmate::assertLogical(
    x = incremental,
    null.ok = FALSE,
    len = 1,
    add = assertColumns,
    .var.name = "incremental"
  )
  
  checkmate::assertCharacter(
    x = saveDirectory,
    null.ok = FALSE,
    len = 1,
    add = assertColumns,
    .var.name = "saveDirectory"
  )
  
  checkmate::reportAssertions(collection = assertColumns)
  
  if (dir.exists(file.path(saveDirectory, "rawData")) == F){ 
    dir.create(file.path(saveDirectory, "rawData"), recursive = T)
  }
  
  if (!is.null(cohorts) && is.null(benchmarkDesign)) {
    if (CohortGenerator::isCohortDefinitionSet(cohorts)){
      cohortsToCreate <- cohorts
    } else if (names(cohorts) %in% c("targetId, outcomeId")){
      cohortsToCreate <- CohortGenerator::getCohortDefinitionSet(
        settingsFileName = file.path("inst", "settings", "CohortsToCreate.csv"),
        jsonFolder = file.path("inst", "cohorts"),
        sqlFolder = file.path("inst", "sql", "sql_server"))
      cohortsToCreate <- cohortsToCreate %>%
        dplyr::filter(cohortId %in% unique(c(cohorts$targetId, cohorts$outcomeId)))
    } else if ((names(cohorts) %in% c("cohortId")) && !(names(cohorts) %in% c("targetId", "outcomeId"))){
      cohortsToCreate <- CohortGenerator::getCohortDefinitionSet(
        settingsFileName = file.path("inst", "settings", "CohortsToCreate.csv"),
        jsonFolder = file.path("inst", "cohorts"),
        sqlFolder = file.path("inst", "sql", "sql_server"))
      cohortsToCreate <- cohortsToCreate %>%
        dplyr::filter(cohortId %in% unique(c(cohorts$cohortId)))
    } 
  } else if (is.null(cohorts) && !is.null(benchmarkDesign)){
    cohortsToCreate <- CohortGenerator::getCohortDefinitionSet(
      settingsFileName = file.path("inst", "settings", "CohortsToCreate.csv"),
      jsonFolder = file.path("inst", "cohorts"),
      sqlFolder = file.path("inst", "sql", "sql_server"))
    uniqueCohorts <- attr(benchmarkDesign, "uniquePlpData") %>%
      dplyr::select(targetId, outcomeId)
    uniqueCohortIds <- unique(c(uniqueCohorts$targetId, uniqueCohorts$outcomeId)) 
    cohortsToCreate <- cohortsToCreate %>%
      dplyr::filter(cohortId %in% uniqueCohortIds)
  }
  
  # Create cohorts
  # if (!is.null(cohortsToCreate)){
  #   
  #   cohortJsonFiles <- file.path(system.file(package = "PLPBenchmarks", "cohorts"), paste0(cohortsToCreate$cohort_name, ".json"))
  #   
  #   for (i in 1:length(cohortJsonFiles)) {
  #     cohortJsonFileName <- cohortJsonFiles[i]
  #     cohortName <- tools::file_path_sans_ext(basename(cohortJsonFileName))
  #     cohortJson <- readChar(cohortJsonFileName, file.info(cohortJsonFileName)$size)
  #     cohortExpression <- CirceR::cohortExpressionFromJson(cohortJson)
  #     cohortSql <- CirceR::buildCohortQuery(cohortExpression, options = CirceR::createGenerateOptions(generateStats = FALSE))
  #     # cohortsToCreate$sql[i] <- cohortSql
  #     # cohortsToCreate$sql[i] <- rbind(cohortsToCreate, data.frame(cohortId = i,
  #     #                                                      cohortName = cohortName, 
  #     #                                                      sql = cohortSql,
  #     #                                                      stringsAsFactors = FALSE))
  #     cohortsToCreate$cohortName[i] <- cohortName
  #     cohortsToCreate$sql[i] <- cohortSql
  #   }
  # }
  
  # Create the cohort tables to hold the cohort generation results
  cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = cohortTable)
  CohortGenerator::createCohortTables(connectionDetails = connectionDetails,
                                      cohortDatabaseSchema = cohortDatabaseSchema,
                                      cohortTableNames = cohortTableNames, 
                                      incremental = incremental)
  # Generate the cohorts
  cohortsGenerated <- CohortGenerator::generateCohortSet(connectionDetails = connectionDetails,
                                                         cdmDatabaseSchema = cdmDatabaseSchema,
                                                         cohortDatabaseSchema = cohortDatabaseSchema,
                                                         cohortTableNames = cohortTableNames,
                                                         cohortDefinitionSet = cohortsToCreate,
                                                         incremental = incremental,
                                                         incrementalFolder = file.path(saveDirectory, "rawData"))
  
  # Get the cohort counts
  cohortCounts <- CohortGenerator::getCohortCounts(connectionDetails = connectionDetails,
                                                   cohortDatabaseSchema = cohortDatabaseSchema,
                                                   cohortTable = cohortTableNames$cohortTable)
  
  ParallelLogger::logInfo(paste("Cohorts created."))
  ParallelLogger::logInfo(paste(cohortCounts))
  
  utils::write.csv(cohortCounts, file.path(saveDirectory, "rawData", paste0("cohortCounts_", gsub("-", "", Sys.Date()), ".csv")))
}