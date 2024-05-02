#' @export
executeBenchmark <- function(benchmarkDesign,
                             analysisSettings,
                             databaseDetails,
                             cohortDefinitions = NULL, 
                             jsonFileLocation = NULL,
                             saveDirectory, 
                             analysisName,
                             createCohorts = FALSE, 
                             extractData = FALSE,
                             prepareData = FALSE, 
                             runPlp = FALSE
    ){
  
  # log 
  logSettings <- PatientLevelPrediction::createLogSettings(logName = "benchmarkLog")
  logPath <- file.path(saveDirectory, analysisName)
  logSettings$saveDirectory <- logPath
  logSettings$logFileName <- 'benchmarkLog'
  logger <- do.call(PatientLevelPrediction:::createLog,logSettings)
  ParallelLogger::registerLogger(logger)
  on.exit(PatientLevelPrediction:::closeLog(logger))
  
  ParallelLogger::logInfo(paste("Starting benchmark analysis for ", analysisName, "..."))
  
  if (!dir.exists(saveDirectory)){
    dir.create(saveDirectory)
  }
  
  if (!dir.exists(file.path(saveDirectory, "rawData"))){
    dir.create(file.path(saveDirectory, "rawData"))
  }
  
  # if (class(benchmarkDesign) == "analysisDesignList") {
  #   benchmarkDesign <- list(analysisDesignList)
  # }
  
  # if (is.null(benchmarkSettings)){
  # benchmarkSettings <- createBenchmarkAnalysisSettings(analysisDesignList = analysisDesignList, cohortDefinitions = cohortDefinitions)
  # }
  
  if (is.null(jsonFileLocation)){
  jsonFiles <- .getJsonFileLocation(analysisSettings$dataSettings)
  } else {
    jsonFiles <- NULL
  }
  
  # databaseDetails <- benchmarkDesign$modelDesign$databaseDetails
  restrictPlpDataSettings <- benchmarkDesign$modelDesign$restrictPlpDataSettings
  covariateSettings <- benchmarkDesign$modelDesign$covariateSettings
  populationSettings <- benchmarkDesign$modelDesign$populationSettings
  requiredTrainPositiveEvents <- benchmarkDesign$requiredTrainPositiveEvents
  splitSettings <- benchmarkDesign$modelDesign$splitSettings
  
  #1. create cohorts
  if(createCohorts){
    ParallelLogger::logInfo(paste("Creating cohorts..."))
    createCohorts(jsonfileLocation = jsonFileLocation, 
                  cohortsToCreate = jsonFiles,
                  connectionDetails = databaseDetails$connectionDetails, 
                  cdmDatabaseSchema = databaseDetails$cdmDatabaseSchema, 
                  cohortDatabaseSchema = databaseDetails$cohortDatabaseSchema, 
                  cohortTable = databaseDetails$cohortTable, 
                  saveDirectory = saveDirectory)
  }
  
  gc()
  
  #2. extract covariates
  if (extractData){
    runSettings <- analysisSettings$dataSettings %>% dplyr::filter(targetId == benchmarkDesign$modelDesign$targetId & outcomeIds == benchmarkDesign$modelDesign$outcomeId)
    databaseDetails$targetId <- benchmarkDesign$modelDesign$targetId
    databaseDetails$outcomeIds <- benchmarkDesign$modelDesign$outcomeId
    
    extractRawData(databaseDetails = databaseDetails, 
                   restrictPlpDataSettings = restrictPlpDataSettings, 
                   populationSettings = populationSettings, 
                   covariateSettings = covariateSettings,
                   requiredTrainPositiveEvents = requiredTrainPositiveEvents, 
                   covariateComparisson = covariateComparisson,
                   dataSettings = runSettings,
                   testSplitFraction = splitSettings$test, 
                   analysisName = analysisName)
  }
  
  #3. downsampling original data and creating covariate sets to make comparisons
  if(prepareData){
    runSettings <- analysisSettings$analysisSettings %>% 
      dplyr::filter(analysisId == analysisName)
    ParallelLogger::logInfo(paste("Preparing data..."))
    prepareData(databaseDetails = databaseDetails, 
                restrictPlpDataSettings = restrictPlpDataSettings,
                populationSettings = populationSettings, 
                covariateSettings = covariateSettings, 
                requiredTrainPositiveEvents = requiredTrainPositiveEvents, 
                testSplitFraction = splitSettings$test, 
                covariateComparisson = benchmarkDesign$covariateComparisson,
                dataSettings = runSettings, 
                saveDirectory = saveDirectory, 
                analysisName = analysisName)
  }
  
  ParallelLogger::logInfo(paste("Data prepared."))
  gc()
  
  #4. run models
  if(runPlp){
    ParallelLogger::logInfo(paste("Running prediction models..."))
    runSettings <- analysisSettings$analysisSettings %>% 
      dplyr::filter(analysisId == analysisName)
    
    runModel(dataSettings = runSettings, 
             outcomeId = databaseDetails$outcomeId, 
             analysisName = analysisName,
             populationSettings = benchmarkDesign$modelDesign$populationSettings, 
             splitSettings = benchmarkDesign$modelDesign$splitSettings, 
             sampleSettings = benchmarkDesign$modelDesign$sampleSettings, 
             featureEngineeringSettings = benchmarkDesign$modelDesign$featureEngineeringSettings, 
             preprocessSettings = PatientLevelPrediction::createPreprocessSettings(),
             modelSettings = benchmarkDesign$modelDesign$modelSettings, 
             logSettings = PatientLevelPrediction::createLogSettings(),
             executeSettings = benchmarkDesign$modelDesign$executeSettings, 
             saveDirectory = saveDirectory)
  }
  ParallelLogger::logInfo(paste("runPlp finished."))
  gc()
  ParallelLogger::logInfo(paste("Benchmark run finished."))
  return(invisible())
  
}