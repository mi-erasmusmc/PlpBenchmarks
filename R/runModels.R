#' @export
runModel <- function(dataSettings, 
                     outcomeId, 
                     analysisId, 
                     analysisName, 
                     populationSettings,
                     splitSettings, 
                     sampleSettings, 
                     featureEngineeringSettings,
                     preprocessSettings, 
                     modelSettings, 
                     logSettings, 
                     executeSettings,
                     saveDirectory){
  
  modelList <- createProcessedModelSettings(covariateComparisson = dataSettings$covariateComparisson, 
                                            modelSettings = modelSettings, 
                                            analysisName = analysisName,
                                            saveDirectory = saveDirectory) %>%
    dplyr::mutate(analysisId = basename(modelName))
  
  modelTimes <- data.frame(analysisName = character(), 
                           modelStart = as.POSIXct(character()), 
                           modelEnd = as.POSIXct(character()),
                           modelDuration = as.difftime(character(), units = "mins"))
  
  for (i in 1:nrow(modelList)) {
    
    # analysisExists <- file.exists(file.path(saveDirectory, analysisName, paste0(attr(modelSettings$param, "settings")$modelType, "Models"), paste(names(plpDataList)[i], sep = "_"), "plpResult", "runPlp.rds"))
    analysisExists <- file.exists(file.path(modelList$modelName[i], "plpResult", "runPlp.rds"))
          if (!analysisExists){
          plpData <- PatientLevelPrediction::loadPlpData(modelList$processedPlpDataName[i])
          tStart <- Sys.time()
    PatientLevelPrediction::runPlp(plpData = plpData,
                    outcomeId = outcomeId,
                    analysisId = modelList$analysisId[i],
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
    tEnd <- Sys.time()
    modelTimes <- modelTimes %>%
      dplyr::bind_rows(tibble::tibble(analysisName = modelList$analysisId[i],
                                      modelStart = tStart, modelEnd = tEnd, modelDuration = tEnd - tStart))
    write.csv(modelTimes, file = file.path(saveDirectory, analysisName, "modelRunningTimes.csv"), append = TRUE)
    
    } else {
      ParallelLogger::logInfo(paste('Analysis for', analysisName, 'exists at', modelList$modelName))
    }
  }
  
  return(invisible())
  
}

#' @export
createProcessedModelSettings <- function(covariateComparisson,
                                         modelSettings,
                                        analysisName, 
                                        saveDirectory){
  
  
  if(covariateComparisson == "demographics"){
    processedModelSettings <- data.frame(analysisName = analysisName, 
                                        covariateComparisson = covariateComparisson, 
                                        saveDirectory = saveDirectory, 
                                        covariates = c("demographics_only"),
                                        processedPlpDataName = file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_only")), 
                                        modelName = file.path(saveDirectory, analysisName, paste0(attr(modelSettings$param, "settings")$modelType, "Models"), paste(analysisName, "demo_only", sep = "_")))
  }
  
  if(covariateComparisson == "demographics and conditions"){
    
    processedModelSettings <- data.frame(analysisName = rep(analysisName, 2), 
                                        covariateComparisson = rep(covariateComparisson, 2), 
                                        saveDirectory = rep(saveDirectory, 2), 
                                        covariates = c("demographics only", "demographics and conditions"),
                                        processedPlpDataName = c(file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_only")), 
                                                                 file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_conds"))), 
                                        modelName = c(file.path(saveDirectory, analysisName, paste0(attr(modelSettings$param, "settings")$modelType, "Models"), paste(analysisName, "demo_only", sep = "_")), 
                                                      file.path(saveDirectory, analysisName, paste0(attr(modelSettings$param, "settings")$modelType, "Models"), paste(analysisName, "demo_conds", sep = "_")))
    )
  }
  
  if(covariateComparisson == "demographics and drugs"){
    
    processedModelSettings <- data.frame(analysisName = rep(analysisName, 2), 
                                        covariateComparisson = rep(covariateComparisson, 2), 
                                        saveDirectory = rep(saveDirectory, 2), 
                                        covariates = c("demographics only", "demographics and drugs"),
                                        processedPlpDataName = c(file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_only")), 
                                                                 file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_drugs"))), 
                                        modelName = c(file.path(saveDirectory, analysisName, paste0(attr(modelSettings$param, "settings")$modelType, "Models"), paste(analysisName, "demo_only", sep = "_")), 
                                                      file.path(saveDirectory, analysisName, paste0(attr(modelSettings$param, "settings")$modelType, "Models"), paste(analysisName, "demo_drugs", sep = "_")))
    )
  }
  
  if(covariateComparisson == "demographics and procedures"){
    
    processedModelSettings <- data.frame(analysisName = rep(analysisName, 2), 
                                        covariateComparisson = rep(covariateComparisson, 2), 
                                        saveDirectory = rep(saveDirectory, 2), 
                                        covariates = c("demographics only", "demographics and procedures"),
                                        processedPlpDataName = c(file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_only")), 
                                                                 file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_prcdrs"))), 
                                        modelName = c(file.path(saveDirectory, analysisName, paste0(attr(modelSettings$param, "settings")$modelType, "Models"), paste(analysisName, "demo_only", sep = "_")), 
                                                      file.path(saveDirectory, analysisName, paste0(attr(modelSettings$param, "settings")$modelType, "Models"), paste(analysisName, "demo_prcdrs", sep = "_")))
    )
  }
  
  if(covariateComparisson == "demographics, conditions and drugs"){
    
    processedModelSettings <- data.frame(analysisName = rep(analysisName, 4), 
                                        covariateComparisson = rep(covariateComparisson, 4), 
                                        saveDirectory = rep(saveDirectory, 4), 
                                        covariates = c("demographics only", "demographics and conds", "demographics and drugs", "demographics, conditions and drugs"),
                                        processedPlpDataName = c(file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_only")), 
                                                                 file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_conds")), 
                                                                 file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_druds")), 
                                                                 file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_conds_drugs"))), 
                                        modelName = c(file.path(saveDirectory, analysisName, paste0(attr(modelSettings$param, "settings")$modelType, "Models"), paste(analysisName, "demo_only", sep = "_")), 
                                                      file.path(saveDirectory, analysisName, paste0(attr(modelSettings$param, "settings")$modelType, "Models"), paste(analysisName, "demo_conds", sep = "_")), 
                                                      file.path(saveDirectory, analysisName, paste0(attr(modelSettings$param, "settings")$modelType, "Models"), paste(analysisName, "demo_drugs", sep = "_")),
                                                      file.path(saveDirectory, analysisName, paste0(attr(modelSettings$param, "settings")$modelType, "Models"), paste(analysisName, "demo_conds_drugs", sep = "_")))
    )
  }
  
  if(covariateComparisson == "demographics, conditions and procedures"){
    
    processedModelSettings <- data.frame(analysisName = rep(analysisName, 4), 
                                        covariateComparisson = rep(covariateComparisson, 4), 
                                        saveDirectory = rep(saveDirectory, 4), 
                                        covariates = c("demographics only", "demographics and conditions", "demographics and procedures", "demographics, conditions and procedures"),
                                        processedPlpDataName = c(file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_only")), 
                                                                 file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_conds")), 
                                                                 file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_prcdrs")), 
                                                                 file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_conds_prcdrs")), 
                                        modelName = c(file.path(saveDirectory, analysisName, paste0(attr(modelSettings$param, "settings")$modelType, "Models"), paste(analysisName, "demo_only", sep = "_")), 
                                                      file.path(saveDirectory, analysisName, paste0(attr(modelSettings$param, "settings")$modelType, "Models"), paste(analysisName, "demo_conds", sep = "_")), 
                                                      file.path(saveDirectory, analysisName, paste0(attr(modelSettings$param, "settings")$modelType, "Models"), paste(analysisName, "demo_prcdrs", sep = "_")),
                                                      file.path(saveDirectory, analysisName, paste0(attr(modelSettings$param, "settings")$modelType, "Models"), paste(analysisName, "demo_conds_prcdrs", sep = "_"))))
    )
  }
  
  if(covariateComparisson == "demographics, drugs and procedures"){
    
    processedModelSettings <- data.frame(analysisName = rep(analysisName, 4), 
                                        covariateComparisson = rep(covariateComparisson, 4), 
                                        saveDirectory = rep(saveDirectory, 4), 
                                        covariates = c("demographics only", "demographics and drugs", "demographics and procedures", "demographics, drugs and procedures"),
                                        processedPlpDataName = c(file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_only")), 
                                                                 file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_drugs")), 
                                                                 file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_prcdrs")), 
                                                                 file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_drugs_prcdrs"))), 
                                        modelName = c(file.path(saveDirectory, analysisName, paste0(attr(modelSettings$param, "settings")$modelType, "Models"), paste(analysisName, "demo_only", sep = "_")), 
                                                      file.path(saveDirectory, analysisName, paste0(attr(modelSettings$param, "settings")$modelType, "Models"), paste(analysisName, "demo_drugs", sep = "_")), 
                                                      file.path(saveDirectory, analysisName, paste0(attr(modelSettings$param, "settings")$modelType, "Models"), paste(analysisName, "demo_prcdrs", sep = "_")),
                                                      file.path(saveDirectory, analysisName, paste0(attr(modelSettings$param, "settings")$modelType, "Models"), paste(analysisName, "demo_drugs_prcdrs", sep = "_")))
    )
  }
  
  if(covariateComparisson == "demographics, conditions, drugs and procedures"){
    
    processedModelSettings <- data.frame(analysisName = rep(analysisName, 8), 
                                        covariateComparisson = rep(covariateComparisson, 8), 
                                        saveDirectory = rep(saveDirectory, 8), 
                                        covariates = c("demographics only", "demographics and conditions", "demographics and drugs", "demographics and procedures", "demographics, conditions and drugs", "demographics, conditions and procedures", "demographics, drugs and procedures", "demographics, conditions, drugs and procedures"),
                                        processedPlpDataName = c(file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_only")), 
                                                                 file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_conds")),
                                                                 file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_drugs")), 
                                                                 file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_prcdrs")), 
                                                                 file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_conds_drugs")),
                                                                 file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_conds_prcdrs")), 
                                                                 file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_drugs_prcdrs")), 
                                                                 file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_conds_drugs_prcdrs"))), 
                                        modelName = c(file.path(saveDirectory, analysisName, paste0(attr(modelSettings$param, "settings")$modelType, "Models"), paste(analysisName, "demo_only", sep = "_")), 
                                                      file.path(saveDirectory, analysisName, paste0(attr(modelSettings$param, "settings")$modelType, "Models"), paste(analysisName, "demo_conds", sep = "_")),
                                                      file.path(saveDirectory, analysisName, paste0(attr(modelSettings$param, "settings")$modelType, "Models"), paste(analysisName, "demo_drugs", sep = "_")), 
                                                      file.path(saveDirectory, analysisName, paste0(attr(modelSettings$param, "settings")$modelType, "Models"), paste(analysisName, "demo_prcdrs", sep = "_")),
                                                      file.path(saveDirectory, analysisName, paste0(attr(modelSettings$param, "settings")$modelType, "Models"), paste(analysisName, "demo_conds_drugs", sep = "_")),
                                                      file.path(saveDirectory, analysisName, paste0(attr(modelSettings$param, "settings")$modelType, "Models"), paste(analysisName, "demo_conds_prcdrs", sep = "_")),
                                                      file.path(saveDirectory, analysisName, paste0(attr(modelSettings$param, "settings")$modelType, "Models"), paste(analysisName, "demo_drugs_prcdrs", sep = "_")),
                                                      file.path(saveDirectory, analysisName, paste0(attr(modelSettings$param, "settings")$modelType, "Models"), paste(analysisName, "demo_conds_drugs_prcdrs", sep = "_")))
    )
  }
  
  return(processedModelSettings)
}


