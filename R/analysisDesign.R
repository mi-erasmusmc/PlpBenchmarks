#' @title Create a benchmark analysis design list
#' @param covariateComparisson Options: \code{"demographics", "demographics and conditions", "demographics and drugs", "demographics and procedures", 
#' "demographics, conditions and drugs", "demographics, conditions and procedures", "demographics, drugs and procedures", 
#' "demographics, conditions, drugs and procedures"}
#' @export
createBenchmarkDesign <- function(modelDesign, 
                                 databaseDetails, 
                                 requiredTrainPositiveEvents = NULL, 
                                 covariateComparisson = c("demographics", "demographics and conditions", "demographics and drugs", "demographics and procedures", 
                                                          "demographics, conditions and drugs", "demographics, conditions and procedures", "demographics, drugs and procedures", 
                                                          "demographics, conditions, drugs and procedures"),
                                 analysisNames,
                                 saveDirectory
){
  checkmate::check_character(analysisNames)
  checkmate::check_list(modelDesign)
  checkmate::check_list(databaseDetails)
  checkmate::check_integerish(requiredTrainPositiveEvents, null.ok = TRUE)
  checkmate::check_string(covariateComparisson)
  checkmate::check_character(saveDirectory)
  
  # if (length(modelDesign) != length(analysisNames)) {
  #   stop("Model design list not the same length as analysis names vector.")
  # }
  # 
  
  result <- list(
    databaseDetails = databaseDetails, 
    modelDesign = modelDesign, 
    requiredTrainPositiveEvents = requiredTrainPositiveEvents, 
    covariateComparisson = covariateComparisson,
    analysisName = analysisNames,
    saveDirectory = saveDirectory
  )
  class(result) <- "benchmarkDesignList"
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
          c(
            analysisDesignList[[i]]$modelDesign$targetId
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
          c(
            # analysisDesignList[[i]]$modelDesign$targetId
            analysisDesignList[[i]]$modelDesign$outcomeId
          )
        }
      )
    )
    
    cohortDefinitions <- data.frame(
      cohortId = cohortIds,
      plpDataName = paste0('Cohort: ', cohortIds), 
      outcomeId = outcomeIds
    ) %>%
      dplyr::distinct()
    
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
      dplyr::distinct()
  }
  
  result <- data.frame(
    analysisId = unlist(lapply(analysisDesignList, function(x) x$analysisName)),
    targetId = unlist(lapply(analysisDesignList, function(x) ifelse(is.null(x$modelDesign$targetId), x$modelDesign$cohortId, x$modelDesign$targetId))),
    outcomeId = unlist(lapply(analysisDesignList, function(x) x$modelDesign$outcomeId)),
    covariateSettings = unlist(lapply(analysisDesignList, function(x) convertToJsonString(x$modelDesign$covariateSettings))),
    restrictPlpDataSettings = unlist(lapply(analysisDesignList, function(x)  convertToJsonString(x$modelDesign$restrictPlpDataSettings))),
    populationSettings = unlist(lapply(analysisDesignList, function(x)  convertToJsonString(x$modelDesign$populationSettings))),
    sampleSettings = unlist(lapply(analysisDesignList, function(x)  convertToJsonString(x$modelDesign$sampleSettings))),
    splitSettings = unlist(lapply(analysisDesignList, function(x)  convertToJsonString(x$modelDesign$splitSettings))),
    featureEngineeringSettings = unlist(lapply(analysisDesignList, function(x)  convertToJsonString(x$modelDesign$featureEngineeringSettings))),
    preprocessSettings = unlist(lapply(analysisDesignList, function(x)  convertToJsonString(x$modelDesign$preprocessSettings))),
    modelSettings = unlist(lapply(analysisDesignList, function(x)  convertToJsonString(x$modelDesign$modelSettings))),
    executeSettings = unlist(lapply(analysisDesignList, function(x)  convertToJsonString(x$modelDesign$executeSettings))), 
    requiredTrainPositiveEvents =  unlist(lapply(analysisDesignList, function(x) x$requiredTrainPositiveEvents)),
    covariateComparisson = unlist(lapply(analysisDesignList, function(x) x$covariateComparisson)),
    saveDirectory = unlist(lapply(analysisDesignList, function(x) x$saveDirectory))
  )
  
  # if (!is.null(problemSpecification)){
  #   checkmate::checkSubset(c("analysisName", "targetCohortName", "outcomeCohortName"), names(problemSpecification))
  # } else {
  #   problemSpecification <- data.frame(
  #     analysisName = unlist(lapply(analysisDesignList, function(x) x$analysisName))) %>%
  #     dplyr::mutate(
  #     cohortName = paste0("Cohort :", .data$analysisName)
  #   )
  # }
  # 
  # result <- result %>%
  #   dplyr::left_join(problemSpecification, by = c("analysisId" = "analysisName"))
  
  # result <- result %>% 
  #   dplyr::left_join(cohortDefinitions, by = c("outcomeId" = "cohortId")) %>%
  #   dplyr::rename(outcomeName = "cohortName") %>%
  #   dplyr::left_join(cohortDefinitions, by = c('targetId' = 'cohortId')) %>%
  #   dplyr::rename(targetName = "cohortName") # new
  
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
      .data$plpDataName
    ) %>%
    dplyr::group_by(.data$targetId) %>% 
    dplyr::mutate(dataLocation =  file.path(saveDirectory, "rawData", .data$plpDataName))

  # add the data names
  result <- result %>% 
    dplyr::left_join(
      uniqueSettings, 
      by = c(
        "targetId" = "targetId",
        "outcomeId" = "outcomeId", 
        "plpDataName" = "plpDataName",
        "covariateSettings" = "covariateSettings",
        "restrictPlpDataSettings" = "restrictPlpDataSettings"
      )) %>% 
    dplyr::mutate(analysisLocation = file.path(paste0(.data$saveDirectory), paste0(.data$analysisId)))
  
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
