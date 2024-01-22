#' @export
runModel <- function(plpDataList, outcomeId, analysisId, analysisName, populationSettings, splitSettings, sampleSettings, 
                     featureEngineeringSettings, preprocessSettings, modelSettings, logSettings, executeSettings, saveDirectory){
  
  # modelList <- purrr::map2(.x = plpDataList, 
  #                          .y = names(plpDataList), 
  #                          .f = ~runPlp(plpData = .x, 
  #                                       outcomeId = outcomeId, 
  #                                       # analysisId = analysisId + match(.y, names(plpDataList)),
  #                                       analysisId = .y,
  #                                       analysisName = analysisName, 
  #                                       populationSettings = populationSettings,
  #                                       splitSettings = splitSettings, 
  #                                       sampleSettings = sampleSettings, 
  #                                       featureEngineeringSettings = featureEngineeringSettings,
  #                                       preprocessSettings = preprocessSettings, 
  #                                       modelSettings = modelSettings, 
  #                                       logSettings = logSettings, 
  #                                       executeSettings = executeSettings,
  #                                       saveDirectory = file.path(saveDirectory, analysisName)))
  
  # modelList <- vector("list", length(plpDataList))
  modelList <- list()
  for (i in seq_along(plpDataList)) {
    
    analysisExists <- file.exists(file.path(saveDirectory, analysisName, paste0(attr(modelSettings$param, "settings")$modelType, "Models"), paste(names(plpDataList)[i], sep = "_"), "plpResult", "runPlp.rds"))
    if (!analysisExists){
    PatientLevelPrediction::runPlp(plpData = plpDataList[[i]],
                    outcomeId = outcomeId,
                    analysisId = names(plpDataList)[i],
                    analysisName = analysisName,
                    populationSettings = populationSettings,
                    splitSettings = splitSettings,
                    sampleSettings = sampleSettings,
                    featureEngineeringSettings = featureEngineeringSettings,
                    preprocessSettings = preprocessSettings,
                    modelSettings = modelSettings,
                    logSettings = logSettings,
                    executeSettings = executeSettings,
                    saveDirectory = file.path(saveDirectory, analysisName, paste0(attr(modelSettings$param, "settings")$modelType, "Models")))
    } else {
      ParallelLogger::logInfo(paste('Analysis for', analysisName, 'exists at', file.path(saveDirectory, paste(analysisName, names(plpDataList)[i], sep = "_"))))
    }
  }
  
  return(invisible())
  
}
