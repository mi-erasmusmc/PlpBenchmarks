#' Trains one of the benchmark prediction problem with standard settingsfor a Lasso LR model
#'
#' @details
#' The user specifies the target and outcome pair as well as the TAR. Combining this with connection details and pre instantiated cohorts allows for the running of the benchmark model
#'
#' @param targetId The targetId of the cohort, this is available in the cohort_table of the database where the cohorts were instantiated 
#' @param outcomeId The outcomeId of the cohort, this is available in the cohort_table of the database where the cohorts were instantiate
#' @param analysisId The analysisId which matches the benchmark problem
#' @param analysisName the analysisName which matches the benchmark problem
#' @param riskWindowStart The start of the time at risk period 
#' @param riskWindowEnd The end of the time at risk period
#' @param connectionDetails databaseconnector connectionDetails object
#' @param cdmDatabaseSchema the name of the table where the cdm is found in the dtaabase
#' @param cohortDatabaseSchema a writable table where the cohorts are found
#' @param cohortTable the name of the table where the cohorts are stored
#' @param saveLoc location to save the model too
#'
#' @return
#' The trained benchmark model
#'
#' @export
runBenchmarkModel <- function(targetId, 
                              outcomeId, 
                              analysisId,
                              analysisName,
                              # riskWindowStart,
                              # riskWindowEnd,
                              connectionDetails, 
                              cdmDatabaseSchema, 
                              cohortDatabaseSchema,
                              cohortTable,
                              populationSettings,
                              saveLoc = getwd()){
  # Select cohorts
  cohortNames <- cohortsToCreate$cohortName[targetId]
  outcomeNames <- cohortsToCreate$cohortName[outcomeId]
  
  covariateSettings <- FeatureExtraction::createCovariateSettings(useDemographicsGender = TRUE,
                                                                  useDemographicsAge = TRUE,                                                                  useConditionGroupEraLongTerm = TRUE,
                                                                  useConditionEraLongTerm = TRUE,
                                                                  useConditionEraMediumTerm = TRUE,
                                                                  useConditionEraShortTerm = TRUE,
                                                                  useDrugEraLongTerm = TRUE,
                                                                  useDrugEraMediumTerm = TRUE,
                                                                  useDrugEraShortTerm = TRUE,
                                                                  useProcedureOccurrenceLongTerm = TRUE,
                                                                  useProcedureOccurrenceMediumTerm = TRUE,
                                                                  useProcedureOccurrenceShortTerm = TRUE,
                                                                  useObservationLongTerm = TRUE,
                                                                  useObservationMediumTerm = TRUE,
                                                                  useObservationShortTerm = TRUE,
                                                                  longTermStartDays = -365,
                                                                  endDays = -1)
  
  databaseDetails <- PatientLevelPrediction::createDatabaseDetails(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cdmDatabaseName = '',
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable = cohortTable,
    targetId = targetId,
    outcomeDatabaseSchema = cohortDatabaseSchema,
    outcomeTable = cohortTable,
    outcomeIds = outcomeId,
    cdmVersion = 5
  )
  
  
  restrictPlpDataSettings <- PatientLevelPrediction::createRestrictPlpDataSettings()
  
  plpData <- PatientLevelPrediction::getPlpData(
    databaseDetails = databaseDetails, 
    covariateSettings = covariateSettings,
    restrictPlpDataSettings = restrictPlpDataSettings
  )
  # populationSettings <- PatientLevelPrediction::createStudyPopulationSettings(
  #   # washoutPeriod = 1095,
  #   washoutPeriod = 365,
  #   firstExposureOnly = FALSE,
  #   removeSubjectsWithPriorOutcome = FALSE,
  #   priorOutcomeLookback = 99999,
  #   riskWindowStart = riskWindowStart,
  #   riskWindowEnd = riskWindowEnd,
  #   startAnchor =  'cohort start',
  #   endAnchor =  'cohort start',
  #   minTimeAtRisk = 364,
  #   requireTimeAtRisk = FALSE,
  #   includeAllOutcomes = TRUE
  # )
  populationSettings <- populationSettings
  
  splitSettings <- PatientLevelPrediction::createDefaultSplitSetting(
    trainFraction = 0.75,
    testFraction = 0.25,
    type = 'stratified',
    nfold = 2, 
    splitSeed = 1234
  )
  
  preprocessSettings <- PatientLevelPrediction::createPreprocessSettings(
    minFraction = 0.01, 
    normalize = T, 
    removeRedundancy = T
  )
  
  sampleSettings <- PatientLevelPrediction::createSampleSettings()
  
  lrModel <- PatientLevelPrediction::setLassoLogisticRegression(seed = 42)
  
  lrResults <- PatientLevelPrediction::runPlp(
    plpData = plpData,
    outcomeId = outcomeId, 
    analysisId = analysisId,
    analysisName = analysisName,
    populationSettings = populationSettings, 
    splitSettings = splitSettings,
    sampleSettings = sampleSettings, 
    # featureEngineeringSettings = featureEngineeringSettings, 
    preprocessSettings = preprocessSettings,
    modelSettings = lrModel,
    logSettings = PatientLevelPrediction::createLogSettings(), 
    executeSettings = PatientLevelPrediction::createExecuteSettings(
    runSplitData = T, 
    runSampleData = T, 
    runfeatureEngineering = F, 
    runPreprocessData = T, 
    runModelDevelopment = T, 
    runCovariateSummary = T
    ), 
    saveDirectory = file.path(saveLoc, analysisName)
  )
  
  # if(!saveModel){
  #   PatientLevelPrediction::savePlpModel(lrResults$model, dirPath = file.path(saveLoc, analysisName, 'model'))
  # }
}
