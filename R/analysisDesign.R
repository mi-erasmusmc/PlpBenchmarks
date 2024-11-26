#' @title Create a benchmark analysis design list
#' @param covariateComparisson Options: \code{"demographics", "demographics and conditions", "demographics and drugs", "demographics and procedures", 
#' "demographics, conditions and drugs", "demographics, conditions and procedures", "demographics, drugs and procedures", 
#' "demographics, conditions, drugs and procedures"}
#' @export
createBenchmarkDesign <- function(modelDesign, 
                                  databaseDetails, 
                                  requiredTrainPositiveEvents = NULL, 
                                 # covariateComparisson = c("demographics", "demographics and conditions", "demographics and drugs", "demographics and procedures", 
                                 #                          "demographics, conditions and drugs", "demographics, conditions and procedures", "demographics, drugs and procedures", 
                                 #                          "demographics, conditions, drugs and procedures"),
                                 # analysisNames,
                                 saveDirectory
){
  # checkmate::check_character(analysisNames)
  checkmate::check_list(modelDesign)
  checkmate::check_list(databaseDetails)
  checkmate::check_integerish(requiredTrainPositiveEvents, null.ok = TRUE)
  # checkmate::check_string(covariateComparisson)
  checkmate::check_character(saveDirectory)
  
  # if (length(modelDesign) != length(analysisNames)) {
  #   stop("Model design list not the same length as analysis names vector.")
  # }
  # 
  
  result <- modelDesign
  
  for (i in seq_along(result)) {
    result[[i]]$databaseDetails <- databaseDetails
    result[[i]]$databaseDetails$targetId <- modelDesign[[i]]$targetId
    result[[i]]$databaseDetails$outcomeIds <- modelDesign[[i]]$outcomeId
    result[[i]]$requiredTrainPositiveEvents <- requiredTrainPositiveEvents
    result[[i]]$analysisName <- names(modelDesign[i])
    result[[i]]$saveDirectory <- file.path(saveDirectory, names(modelDesign[i]))
  }
  
  
  # result <- list(
  #   databaseDetails = databaseDetails, 
  #   modelDesign = modelDesign, 
  #   requiredTrainPositiveEvents = requiredTrainPositiveEvents, 
  #   covariateComparisson = covariateComparisson,
  #   analysisName = analysisNames,
  #   saveDirectory = saveDirectory
  # )
  settings <- convertToJson(result)
  uniqueCohortSettings <- attr(settings, "uniqueCohorts")
  attr(result, "uniqueCohortSettings") <- uniqueCohortSettings
  attr(result, "settings") <- settings %>% dplyr::select(.data$analysisId, .data$targetId, .data$outcomeId, .data$problemId, .data$dataLocation)
  class(result) <- "benchmarkDesign"
  return(result)
}

#' @export
convertToJson <-function(
    analysisDesignList,
    cohortDefinitions = NULL
){
  
  if(is.null(cohortDefinitions)){
    
    cohortIds <- unlist(
      lapply(
        X = 1:length(analysisDesignList), 
        FUN = function(i){
          c(analysisDesignList[[i]]$targetId
            # analysisDesignList[[i]]$modelDesign$targetId
            # analysisDesignList[[i]]$modelDesign$outcomeId
          )
        }
      )
    )
    # cohortIds <- unique(cohortIds)
    
    outcomeIds <- unlist(
      lapply(
        X = 1:length(analysisDesignList), 
        FUN = function(i){
          c(analysisDesignList[[i]]$outcomeId
            # analysisDesignList[[i]]$modelDesign$targetId
            # analysisDesignList[[i]]$modelDesign$outcomeId
          )
        }
      )
    )
    
    # covariateSettings <- unlist(
    #   lapply(
    #     X = 1:length(analysisDesignList), 
    #     FUN = function(i){
    #       c(analysisDesignList[[i]]$covariateSettings)
    #     })
    # )
    
    # restrictPlpDataSettings <- unlist(
    #   lapply(
    #     X = 1:length(analysisDesignList), 
    #     FUN = function(i){
    #       c(analysisDesignList[[i]]$restrictPlpDataSettings)
    #     })
    # )
    
    cohortDefinitions <- data.frame(
      cohortId = cohortIds,
      plpDataName = paste0('Cohort: ', cohortIds), 
      outcomeId = outcomeIds 
      # covariateSettings = covariateSettings, 
      # restrictPlpDataSettings = restrictPlpDataSettings, 
    ) %>%
      dplyr::mutate(problemId = dplyr::row_number()) %>%
      dplyr::distinct(cohortId, plpDataName, outcomeId, .keep_all = TRUE)
    
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
      dplyr::mutate(problemId = dplyr::row_number()) %>%
      dplyr::distinct(plpDataName, targetId, outcomeId, .keep_all = TRUE)
  }
  
  result <- data.frame(
    analysisId = unlist(lapply(analysisDesignList, function(x) x$analysisName)),
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
    dplyr::left_join(cohortDefinitions, dplyr::join_by(outcomeId == outcomeId, targetId == cohortId)) 
    # dplyr::rename("targetName" = "cohortName")
  
  # get the names
  uniqueSettings <-  result %>% 
    dplyr::distinct(
      .data$targetId,
      .data$outcomeId,
      .data$covariateSettings, 
      .data$restrictPlpDataSettings, 
      .data$plpDataName, 
      .data$problemId, 
      .keep_all = TRUE
    ) %>%
    dplyr::group_by(.data$targetId) %>% 
    dplyr::mutate(dataLocation =  file.path(.data$saveDirectory, "rawData", "plpData", .data$plpDataName)) %>%
    dplyr::select(targetId, outcomeId, covariateSettings, restrictPlpDataSettings, plpDataName, problemId, dataLocation)

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
        "problemId" = "problemId" 
        # "analysisId" = "analysisId"
      ))
  
  attr(result, "uniqueCohorts") <- uniqueSettings
  return(result)
}

#' @export
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

.getJsonFileLocation <- function(dataSettings){
  result <- dataSettings %>%
    dplyr::select(targetId, targetJsonLocation) %>%
    dplyr::rename(cohortId = targetId, 
                  jsonLocation = targetJsonLocation) %>%
    dplyr::bind_rows(dataSettings %>%
                       dplyr::select(outcomeIds, outcomeJsonLocation) %>% 
                       dplyr::mutate(outcomeIds = as.numeric(outcomeIds)) %>%
                       dplyr::rename(cohortId = outcomeIds, 
                                     jsonLocation = outcomeJsonLocation))  %>%
    dplyr::distinct()
  
  return(result)
}

convertToJsonString <- function(x){as.character(ParallelLogger::convertSettingsToJson(x))}

convertFromJsonString <- function(x){ParallelLogger::convertJsonToSettings(x)}
