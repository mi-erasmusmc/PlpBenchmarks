library(PatientLevelPrediction)
library(Eunomia)
library(dplyr)
library(PLPBenchmarks)
library(ParallelLogger)

connectionDetails <- getEunomiaConnectionDetails()
Eunomia::createCohorts(connectionDetails = connectionDetails)

# analysisName = "GIBinCLXB"
saveDirectory = "eunomiaResultsParallel12"
requiredTrainPositiveEvents = 450
seed = 42
cdmDatabaseSchema = "main"
cdmDatabaseName = "Eunomia"
cdmDatabaseId = "Eunomia"
cohortDatabaseSchema = "main"
outcomeDatabaseSchema = "main"
cohortTable = "cohort"

# multipleAnalyses <- tibble(
#   targetCohortIds = c(1, 2, 4), 
#   outcomeCohortIds = c(3, 3, 3), 
#   modelNames = c("GIBinCLXB", "GIBinDCLFNC", "GIBinNSAIDs")
# )
multipleAnalyses <- tibble( targetId = c(1, 2, 4, 1, 2), 
                            outcomeId = c(3, 3, 3, 3, 3),
                            riskStart = c(0,0,0, 0, 0), 
                            riskEnd = c(365, 365, 365, 90, 90), 
                            analysisName = c("GIBinCLXB", "GIBinDCLFNC", "GIBinNSAIDs", "GIBinCLXB_90", "GIBinDCLFNC_90"),
                            plpDataName = c("GIBinCLXB", "GIBinDCLFNC", "GIBinNSAIDs", "GIBinCLXB", "GIBinDCLFNC"), 
                            targetJsonLocation = rep("Eunomia", 5), 
                            outcomeJsonLocation = rep("Eunomia", 5)) 

# lassoModel <- PatientLevelPrediction::setLassoLogisticRegression(seed=seed)
databaseDetailsList <- list()
for (i in seq_along(1:nrow(multipleAnalyses))) {
  
  databaseDetailsList[[i]] <- PatientLevelPrediction::createDatabaseDetails(connectionDetails = connectionDetails, 
                                                                            cdmDatabaseSchema = cdmDatabaseSchema,
                                                                            cdmDatabaseName = cdmDatabaseName,
                                                                            cdmDatabaseId = cdmDatabaseId, 
                                                                            cohortDatabaseSchema = cohortDatabaseSchema,
                                                                            cohortTable = cohortTable,
                                                                            outcomeDatabaseSchema = outcomeDatabaseSchema,
                                                                            outcomeTable = cohortTable, 
                                                                            targetId = multipleAnalyses$targetCohortIds[i], 
                                                                            outcomeIds = multipleAnalyses$outcomeCohortIds[i])
}
names(databaseDetailsList) <- paste0(multipleAnalyses$modelNames, "_DbDetails")

restrictPlpDataSettings <- PatientLevelPrediction::createRestrictPlpDataSettings()

studyPopulationSettingsList <- list()
for (i in seq_along(1:nrow(multipleAnalyses))) {
  studyPopulationSettingsList[[i]]<- PatientLevelPrediction::createStudyPopulationSettings(binary = T,
                                                                                           includeAllOutcomes = F,
                                                                                           firstExposureOnly = T, 
                                                                                           washoutPeriod = 0, 
                                                                                           removeSubjectsWithPriorOutcome = T, 
                                                                                           priorOutcomeLookback = 99999,
                                                                                           requireTimeAtRisk = T, 
                                                                                           minTimeAtRisk = 1, 
                                                                                           riskWindowStart = 1, 
                                                                                           riskWindowEnd = 365)
}
names(studyPopulationSettingsList) <- paste0(multipleAnalyses$modelNames, "_studyPopSettings")


lassoModel <- PatientLevelPrediction::setLassoLogisticRegression(seed=seed)

covariateSettings_demo_conds_drugs <- FeatureExtraction::createCovariateSettings(useDemographicsGender = T, 
                                                                                 useDemographicsAge = T, 
                                                                                 useConditionOccurrenceLongTerm = T,
                                                                                 useDrugGroupEraLongTerm = T, 
                                                                                 longTermStartDays = -365,
                                                                                 endDays = -1)

# analysisList <- vector("list", length(nrow(multipleAnalyses)))
# for (i in seq_along(1:nrow(multipleAnalyses))) {
#   analysisList[[i]] <- list(
#     databaseDetails = databaseDetailsList[[i]], 
#     restrictPlpDataSettings = restrictPlpDataSettings,
#     requiredTrainPositiveEvents = 50, 
#     populationSettings = studyPopulationSettingsList[[i]], 
#     analysisName = multipleAnalyses$modelNames[i], 
#     covariateSettings = covariateSettings_demo_conds_drugs,
#     saveDirectory = saveDirectory,
#     modelSettings = lassoModel
#   )
# }

modelDesignList <- list()
for (i in seq_along(1:nrow(multipleAnalyses))) {
  modelDesignList[[i]] <- PatientLevelPrediction::createModelDesign(
    targetId = multipleAnalyses$targetId[i], 
    outcomeId = multipleAnalyses$outcomeId[i], 
    # databaseDetails = databaseDetailsList[[i]], 
    restrictPlpDataSettings = restrictPlpDataSettings,
    # requiredTrainPositiveEvents = 50, 
    populationSettings = studyPopulationSettingsList[[i]], 
    # analysisName = multipleAnalyses$modelNames[i], 
    covariateSettings = covariateSettings_demo_conds_drugs,
    # saveDirectory = saveDirectory,
    modelSettings = lassoModel
  )
}

# benchmarkDesignList <- list()
# for(i in seq_along(1:nrow(multipleAnalyses))){
#   
# }
analysisDesigns <- purrr::map(.x = 1:nrow(multipleAnalyses), ~createBenchmarkDesign(modelDesign = modelDesignList[[.x]], 
                                                                                   databaseDetails = databaseDetailsList[[.x]], 
                                                                                   requiredTrainPositiveEvents = requiredTrainPositiveEvents,
                                                                                   covariateComparisson = "demographics, conditions, drugs and procedures",
                                                                                   analysisNames = multipleAnalyses$analysisName[[.x]], 
                                                                                   saveDirectory = saveDirectory
))
names(analysisDesigns) <- paste0(multipleAnalyses$analysisName, "_analysisDesignSettings")

cohortDefinitions <- multipleAnalyses
benchmarkSettings <- createBenchmarkAnalysisSettings(analysisDesignList = analysisDesigns, cohortDefinitions = cohortDefinitions)

# analysisDesigns$cohortDefinitions <- cohortDefinitions
# analysisDesigns$analysisDesign <- benchmarkSettings

wrappedExecute <- function(analysisList, analysisSettings, cohortDefinitions, seed = 42){
  
  databaseDetails = analysisList$databaseDetails
  analysisName = analysisList$analysisName
  saveDirectory = analysisList$saveDirectory
  # benchmarkSettings = benchmarkSettings
  # cohortDefinitions = cohortDefinitions
  
  PLPBenchmarks::executeBenchmark(analysisSettings = analysisSettings,
                                  cohortDefinitions = cohortDefinitions, 
                                  databaseDetails = databaseDetails, 
                                  jsonFileLocation = NULL,
                                  benchmarkDesign = analysisList,
                                  analysisName = analysisName,
                                  saveDirectory = saveDirectory, 
                                  createCohorts = F,
                                  extractData = T,
                                  prepareData = T, 
                                  runPlp = T )
  
  return(invisible())
}

executeBenchmarkParallel <- function(largeVector, analysisSettings, cohortDefinitions) {
  
  cluster <- ParallelLogger::makeCluster(numberOfThreads = 4)
  on.exit(ParallelLogger::stopCluster(cluster))
  ParallelLogger::clusterApply(cluster, largeVector, wrappedExecute, analysisSettings = benchmarkSettings, cohortDefinitions = cohortDefinitions)
  
}

executeBenchmarkParallel(largeVector = analysisDesigns, analysisSettings =  benchmarkSettings, cohortDefinitions = cohortDefinitions)
# debugonce(executeBenchmarkParallel)
