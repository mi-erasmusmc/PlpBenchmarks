#' @title Create a benchmark analysis design 
#' 
#' @description
#' Create a benchmark analysis design by providing a list of objects each of class \code{modelDesign}. The result is a list of class \code{benchmarkDesign}
#' that holds the necessary information to run each problem. Note that, for problems that share the same target and outcome cohorts, and/or same population settings
#' the design specifies which of them are unique to avoid duplicating tasks. The directories where each plpData or study population object is to be saved are found 
#' in the attributes of the returned object.
#' 
#' @param modelDesign A named list of model designs. Each object of the list should be another list of class \code{modelDesign} created using 
#' \code{PatientLevelPrediction::createModelDesign()}
#' @param databaseDetails An object of class databaseDetails created using \code{PatientLevelPrediction::createDatabaseDetails()}.
#' @param saveDirectory The directory to save the benchmark analysis.
#'
#' @export
createBenchmarkDesign <- function(modelDesign, 
                                  databaseDetails, 
                                  saveDirectory
){

  checkmate::check_list(modelDesign)
  checkmate::check_list(databaseDetails)
  checkmate::check_character(saveDirectory)
  
  result <- modelDesign
  
  for (i in seq_along(result)) {
    result[[i]]$databaseDetails <- databaseDetails
    result[[i]]$databaseDetails$targetId <- modelDesign[[i]]$targetId
    result[[i]]$databaseDetails$outcomeIds <- modelDesign[[i]]$outcomeId
    # result[[i]]$requiredTrainPositiveEvents <- requiredTrainPositiveEvents
    result[[i]]$analysisName <- names(modelDesign[i])
    result[[i]]$saveDirectory <- file.path(saveDirectory, names(modelDesign[i]))
  }
  
  settings <- convertToJson(result)
  uniquePlpData <- attr(settings, "uniqueCohorts")
  uniquePopulation <- attr(settings, "uniquePopulation")
  uniqueSettings <- attr(settings, "uniqueSettings")
  
  attr(result, "uniquePlpData") <- uniquePlpData
  attr(result, "uniquePopulation") <- uniquePopulation
  attr(result, "benchmarkSettings") <- settings %>%
    dplyr::select(.data$analysisId, .data$targetId, .data$outcomeId, .data$problemId, .data$sameTargetAsProblemId, .data$plpDataName, .data$populationLocation, .data$dataLocation)
 
  class(result) <- "benchmarkDesign"
  return(result)
}


convertToJson <- function(
    analysisDesignList,
    cohortDefinitions = NULL
){
  
  if(is.null(cohortDefinitions)){
    
    cohortIds <- unlist(
      lapply(
        X = 1:length(analysisDesignList), 
        FUN = function(i){
          c(analysisDesignList[[i]]$targetId
          )
        }
      )
    )
    
    outcomeIds <- unlist(
      lapply(
        X = 1:length(analysisDesignList), 
        FUN = function(i){
          c(analysisDesignList[[i]]$outcomeId
          )
        }
      )
    )
    
    cohortNames <- unlist(
      lapply(
        X = 1:length(analysisDesignList), 
        FUN = function(i){
          c(analysisDesignList[[i]]$analysisName
          )
        }
      )
    )
    
    # cohortDefinitions <- data.frame(
    #   cohortId = cohortIds,
    #   plpDataName = paste0('Cohort: ', cohortIds), 
    #   outcomeId = outcomeIds 
    # ) %>%
    #   dplyr::mutate(problemId = dplyr::row_number()) %>%
    #   dplyr::distinct(cohortId, plpDataName, outcomeId, .keep_all = TRUE)
    
    cohortDefinitions <- data.frame(
      cohortId = cohortIds,
      plpDataName = cohortNames, 
      outcomeId = outcomeIds 
    ) %>%
      dplyr::mutate(problemId = dplyr::row_number()) %>%
      dplyr::distinct(.data$cohortId, .data$plpDataName, .data$outcomeId, .keep_all = TRUE)
    
  } else{
    
    cohortDefinitions <- cohortDefinitions %>% 
      dplyr::select(
        "plpDataName", 
        "targetId", 
        "outcomeId", 
        "targetJsonLocation",
        "outcomeJsonLocation"
      ) %>%
      dplyr::rename(cohortId = targetId) %>%
      dplyr::mutate(problemId = dplyr::row_number()) 
  }
  
  result <- data.frame(
    analysisId = unlist(lapply(analysisDesignList, function(x) x$analysisName)),
    problemId = c(1:length(analysisDesignList)), 
    targetId = unlist(lapply(analysisDesignList, function(x) ifelse(is.null(x$targetId), x$cohortId, x$targetId))),
    outcomeId = unlist(lapply(analysisDesignList, function(x) x$outcomeId)),
    covariateSettings = unlist(lapply(analysisDesignList, function(x) convertToJsonString(x$covariateSettings))),
    restrictPlpDataSettings = unlist(lapply(analysisDesignList, function(x)  convertToJsonString(x$restrictPlpDataSettings))),
    populationSettings = unlist(lapply(analysisDesignList, function(x)  convertToJsonString(x$populationSettings))),
    sampleSettings = unlist(lapply(analysisDesignList, function(x)  convertToJsonString(x$sampleSettings))),
    splitSettings = unlist(lapply(analysisDesignList, function(x)  convertToJsonString(x$splitSettings))),
    featureEngineeringSettings = unlist(lapply(analysisDesignList, function(x)  convertToJsonString(x$featureEngineeringSettings))),
    preprocessSettings = unlist(lapply(analysisDesignList, function(x)  convertToJsonString(x$preprocessSettings))),
    modelSettings = unlist(lapply(analysisDesignList, function(x)  convertToJsonString(x$modelSettings))),
    executeSettings = unlist(lapply(analysisDesignList, function(x)  convertToJsonString(x$executeSettings))), 
    saveDirectory = unlist(lapply(analysisDesignList, function(x) x$saveDirectory))
  )
  
  result <- result %>%
    dplyr::left_join(cohortDefinitions, by = c("outcomeId" = "outcomeId", "targetId" = "cohortId")) %>%
    dplyr::rename("problemId" = "problemId.x", "sameTargetAsProblemId" = "problemId.y")
  
  # get the names
  uniquePlpData <-  result %>% 
    dplyr::distinct(
      .data$targetId,
      .data$outcomeId,
      .data$covariateSettings, 
      .data$restrictPlpDataSettings, 
      # .data$plpDataName, 
      # .data$problemId, 
      .keep_all = TRUE
    ) %>%
    # dplyr::group_by(.data$targetId) %>% 
    dplyr::mutate(dataLocation =  file.path(dirname(.data$saveDirectory), "rawData", basename(.data$saveDirectory), "plpData", .data$plpDataName)) %>%
    dplyr::select(.data$targetId, .data$outcomeId, .data$covariateSettings, .data$restrictPlpDataSettings, .data$plpDataName, .data$problemId, .data$dataLocation)
  
  uniquePopulation <- result %>%
    dplyr::distinct(
      .data$targetId,
      .data$outcomeId,
      .data$covariateSettings, 
      .data$restrictPlpDataSettings, 
      .data$populationSettings, 
      .keep_all = TRUE
    ) %>%
    # dplyr::group_by(.data$targetId) %>% 
    dplyr::mutate(populationLocation =  file.path(dirname(.data$saveDirectory), "rawData", basename(.data$saveDirectory), "studyPopulation", .data$plpDataName)) %>%
    dplyr::select(.data$targetId, .data$outcomeId, .data$covariateSettings, .data$restrictPlpDataSettings, .data$populationSettings, .data$plpDataName, .data$problemId, .data$populationLocation)
    
  uniqueSettings <- dplyr::left_join(uniquePlpData, uniquePopulation, 
                                     by = c(
                                       "targetId" = "targetId",
                                       "outcomeId" = "outcomeId", 
                                       "covariateSettings" = "covariateSettings",
                                       "restrictPlpDataSettings" = "restrictPlpDataSettings" 
                                       )) %>%
    dplyr::rename("plpDataName" = "plpDataName.y", "problemId" = "problemId.y", "sameTargetAsProblemId" = "problemId.x" ) %>%
    dplyr::select(-c(.data$plpDataName.x)) %>%
    dplyr::arrange(.data$problemId)

  # add the data names
  result <- result %>% 
    dplyr::left_join(
      uniqueSettings, 
      by = c(
        "targetId" = "targetId",
        "outcomeId" = "outcomeId", 
        "plpDataName" = "plpDataName",
        "covariateSettings" = "covariateSettings",
        "restrictPlpDataSettings" = "restrictPlpDataSettings", 
        "populationSettings" = "populationSettings",
        "sameTargetAsProblemId" = "sameTargetAsProblemId" 
      )) %>%
    dplyr::rename("problemId" = "problemId.x") %>%
    dplyr::select(-c("problemId.y")) %>%
    dplyr::filter(!is.na(dataLocation))
  
  attr(result, "uniqueCohorts") <- uniquePlpData
  attr(result, "uniquePopulations") <- uniquePopulation
  attr(result, "uniqueSettings") <- uniqueSettings
  return(result)
}


createBenchmarkAnalysisSettings <- function(analysisDesignList, cohortDefinitions = NULL) {
  
  checkmate::check_list(analysisDesignList)
  
  settingsTable <- convertToJson(analysisDesignList = analysisDesignList, cohortDefinitions = cohortDefinitions)
  
  dataSettings <- settingsTable %>% 
    dplyr::group_by(
      .data$targetId,
      .data$covariateSettings,
      .data$restrictPlpDataSettings,
      .data$dataLocation, 
      .data$targetJsonLocation, 
      .data$outcomeJsonLocation,
      .data$covariateComparisson
    ) %>% 
    dplyr::summarise(
      outcomeIds = paste(unique(.data$outcomeId), collapse = ','), .groups = "drop"
    )

  # save the settings - TODO change this to save jsons in csv
  # utils::write.csv(
  #   x = settingsTable %>% dplyr::select(
  #     "analysisId",
  #     "targetId", 
  #     "targetName",
  #     "outcomeId", 
  #     "outcomeName",
  #     "dataLocation"
  #   ), 
  #   file.path(saveDirectory,'settings.csv'), 
  #   row.names = F
  # )
  saveDirectory <- unique(settingsTable$saveDirectory)
  
  # utils::write.csv(
  #   x = settingsTable, 
  #   file = file.path(saveDirectory,'settings.csv'), 
  #   row.names = F
  # )
  
  return(list(analysisSettings = settingsTable, 
              dataSettings = dataSettings))
}

convertToJsonString <- function(x){as.character(ParallelLogger::convertSettingsToJson(x))}

convertFromJsonString <- function(x){ParallelLogger::convertJsonToSettings(x)}
