#' @export
prepareData <- function(databaseDetails, 
                        restrictPlpDataSettings,
                        populationSettings, 
                        covariateSettings, 
                        requiredTrainPositiveEvents, 
                        testSplitFraction, 
                        seed = 42, 
                        analysisName,
                        saveDirectory){
  
  ParallelLogger::logInfo(paste("Extracting raw plpData object..."))
  analysisExists <- file.exists(file.path(saveDirectory, analysisName, "rawData", paste(analysisName, "plpDataRaw", sep = "_")))
  if (!analysisExists){
  plpDataRaw <- PatientLevelPrediction::getPlpData(databaseDetails = databaseDetails, 
                                                   covariateSettings = covariateSettings,
                                                   restrictPlpDataSettings = restrictPlpDataSettings)
  PatientLevelPrediction::savePlpData(plpData = plpDataRaw, file = file.path(saveDirectory, analysisName, "rawData", paste(analysisName, "plpDataRaw", sep = "_")) )
  } else {
    ParallelLogger::logInfo(paste('Raw data for', analysisName, 'exists at', file.path(saveDirectory, analysisName, "rawData")))
    plpDataRaw = PatientLevelPrediction::loadPlpData(file.path(saveDirectory, analysisName, "rawData", paste(analysisName, "plpDataRaw", sep = "_")))
  }
  ParallelLogger::logInfo(paste("Done."))
  ParallelLogger::logInfo(paste("Creating raw study population..."))
  analysisExists <- file.exists(file.path(saveDirectory, analysisName, "rawData", paste(analysisName, "studyPopulationRaw.Rds", sep = "_")))
  if (!analysisExists){
  studyPopulationRaw <- PatientLevelPrediction::createStudyPopulation(plpData = plpDataRaw,
                                                                      outcomeId = databaseDetails$outcomeIds, 
                                                                      populationSettings = populationSettings)
  saveRDS(object = studyPopulationRaw, file = file.path(saveDirectory, analysisName, "rawData", paste(analysisName, "studyPopulationRaw.Rds", sep = "_")))
  } else {
    ParallelLogger::logInfo(paste('Study population for', analysisName, 'exists at', file.path(saveDirectory, analysisName, "rawData")))
    studyPopulationRaw = readRDS(file = file.path(saveDirectory, analysisName, "rawData", paste(analysisName, "studyPopulationRaw.Rds", sep = "_")))
  }
  ParallelLogger::logInfo(paste("Done."))
  ParallelLogger::logInfo(paste("Reducing population to specified number for requiredTrainPositiveEvents (", requiredTrainPositiveEvents, ")." ))
  analysisExists <- file.exists(file.path(saveDirectory, analysisName, "processedData", paste(analysisName, "plpData_demo_conds_drugs", sep = "_")))
  if (!analysisExists){
  reducedData <- reduceData(studyPopulation = studyPopulationRaw,
                            plpData = plpDataRaw,
                            requiredTrainPositiveEvents = requiredTrainPositiveEvents,
                            testSplitFraction = testSplitFraction, 
                            seed = seed)
  # the reduced data has covariates for demo, conds and drugs
  PatientLevelPrediction::savePlpData(reducedData$plpData, file = file.path(saveDirectory, analysisName, "processedData", paste(analysisName, "plpData_demo_conds_drugs", sep = "_")))
  saveRDS(object = reducedData$studyPopulation, file = file.path(saveDirectory, analysisName, "processedData", paste(analysisName, "studyPopulation.Rds", sep = "_")))
  saveRDS(object = reducedData$summaryStats,  file = file.path(saveDirectory, analysisName, "processedData", paste(analysisName, "summaryStats.Rds", sep = "_")))
  ParallelLogger::logInfo(print(reducedData$summaryStats))
  } else {
    ParallelLogger::logInfo(paste('Sampled data with all covariates (demo/conds/drugs) for', analysisName, 'exists at', file.path(saveDirectory, analysisName, "processedData")))
    reducedData <- vector("list", 2)
    reducedData$studyPopulation <- readRDS(file = file.path(saveDirectory, analysisName, "processedData", paste(analysisName, "studyPopulation.Rds", sep = "_")))
    reducedData$plpData <- PatientLevelPrediction::loadPlpData(file = file.path(saveDirectory, analysisName, "processedData", paste(analysisName, "plpData_demo_conds_drugs", sep = "_")))
  }
  ParallelLogger::logInfo(paste("Done."))
  ParallelLogger::logInfo(paste("Filtering covariates to create candidate predictor sets..."))
  analysisExists <- file.exists(file.path(saveDirectory, analysisName, "processedData", paste(analysisName, "plpData_demo_only", sep = "_")))
  if (!analysisExists){
  demo_only <- createComparissonCovariates(plpData = reducedData$plpData, 
                                           studyPopulation = reducedData$studyPopulation, 
                                           covariates = "demographics only")
  PatientLevelPrediction::savePlpData(demo_only, file = file.path(saveDirectory, analysisName, "processedData", paste(analysisName, "plpData_demo_only", sep = "_")))
  ParallelLogger::logInfo(paste("plpData with demographics only created."))
  } else {
    ParallelLogger::logInfo(paste('Sampled data with demographics for', analysisName, 'exists at', file.path(saveDirectory, analysisName, "processedData")))
  }
  
  analysisExists <- file.exists(file.path(saveDirectory, analysisName, "processedData", paste(analysisName, "plpData_demo_conds", sep = "_")))
  if (!analysisExists){
  demo_conds <- createComparissonCovariates(plpData = reducedData$plpData, 
                                            studyPopulation = reducedData$studyPopulation,
                                            covariates = "demographics and conditions")
  PatientLevelPrediction::savePlpData(demo_conds, file = file.path(saveDirectory, analysisName, "processedData", paste(analysisName, "plpData_demo_conds", sep = "_")))
  ParallelLogger::logInfo(paste("plpData with demographics and conditions created."))
  } else {
    ParallelLogger::logInfo(paste('Sampled data with demographics and conditions for', analysisName, 'exists at', file.path(saveDirectory, analysisName, "processedData")))
  }
  
  analysisExists <- file.exists(file.path(saveDirectory, analysisName, "processedData", paste(analysisName, "plpData_demo_drugs", sep = "_")))
  if (!analysisExists){
  demo_drugs <- createComparissonCovariates(plpData = reducedData$plpData,
                                            studyPopulation = reducedData$studyPopulation, 
                                            covariates = "demographics and drugs")
  PatientLevelPrediction::savePlpData(demo_drugs, file = file.path(saveDirectory, analysisName, "processedData", paste(analysisName, "plpData_demo_drugs", sep = "_")))
  ParallelLogger::logInfo(paste("plpData with demographics and drugs created."))
  } else {
    ParallelLogger::logInfo(paste('Sampled data with demographics and drugs for', analysisName, 'exists at', file.path(saveDirectory, analysisName, "processedData")))
  }
  
  return(invisible())
}
