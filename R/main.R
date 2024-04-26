#' @export
executeBenchmark <- function(jsonFileLocation, 
                             databaseDetails, 
                             restrictPlpDataSettings, 
                             populationSettings, 
                             requiredTrainPositiveEvents, 
                             covariateSettings, 
                             modelSettings, 
                             featureEngineeringSettings, 
                             sampleSettings, 
                             splitSettings, 
                             executeSettings, 
                             analysisName, 
                             saveDirectory, 
                             createCohorts = FALSE, 
                             prepareData = FALSE, 
                             runPlp = FALSE){
  
  # log 
  logSettings <- PatientLevelPrediction::createLogSettings(logName = "benchmarkLog")
  logPath <- file.path(saveDirectory, analysisName)
  logSettings$saveDirectory <- logPath
  logSettings$logFileName <- 'benchmarkLog'
  logger <- do.call(PatientLevelPrediction:::createLog,logSettings)
  ParallelLogger::registerLogger(logger)
  on.exit(PatientLevelPrediction:::closeLog(logger))
  
  ParallelLogger::logInfo(paste("Starting benchmark analysis for ", analysisName, "..."))
  
  #1. create cohorts
  if(createCohorts){
    ParallelLogger::logInfo(paste("Creating cohorts..."))
    createCohorts(jsonfileLocation = jsonFileLocation, 
                  connectionDetails = databaseDetails$connectionDetails, 
                  cdmDatabaseSchema = databaseDetails$cdmDatabaseSchema, 
                  cohortDatabaseSchema = databaseDetails$cohortDatabaseSchema, 
                  cohortTable = databaseDetails$cohortTable)
  }
  ParallelLogger::logInfo(paste("Cohorts created."))
  gc()
  
  #2. downsampling original data and creating covariate sets to make comparisons
  if(prepareData){
    ParallelLogger::logInfo(paste("Preparing data..."))
    prepareData(databaseDetails = databaseDetails, 
                restrictPlpDataSettings = restrictPlpDataSettings,
                populationSettings = populationSettings, 
                covariateSettings = covariateSettings, 
                requiredTrainPositiveEvents = requiredTrainPositiveEvents, 
                testSplitFraction = splitSettings$test, 
                saveDirectory = saveDirectory, 
                analysisName = analysisName)
  }
  
  ParallelLogger::logInfo(paste("Data prepared."))
  gc()
  
  #3. run models
  if(runPlp){
    ParallelLogger::logInfo(paste("Running prediction models..."))
    plpDataList <- list(
      demo_only = PatientLevelPrediction::loadPlpData(file.path(saveDirectory, analysisName, "processedData", paste(analysisName, "plpData_demo_only", sep = "_"))), 
      demo_conds = PatientLevelPrediction::loadPlpData(file.path(saveDirectory, analysisName, "processedData", paste(analysisName, "plpData_demo_conds", sep = "_"))), 
      demo_drugs = PatientLevelPrediction::loadPlpData(file.path(saveDirectory, analysisName, "processedData", paste(analysisName, "plpData_demo_drugs", sep = "_"))),
      demo_prcdrs = PatientLevelPrediction::loadPlpData(file.path(saveDirectory, analysisName, "processedData", paste(analysisName, "plpData_demo_prcdrs", sep = "_"))),
      demo_conds_drugs = PatientLevelPrediction::loadPlpData(file.path(saveDirectory, analysisName, "processedData", paste(analysisName, "plpData_demo_conds_drugs", sep = "_"))), 
      demo_conds_prcdrs = PatientLevelPrediction::loadPlpData(file.path(saveDirectory, analysisName, "processedData", paste(analysisName, "plpData_demo_conds_prcdrs", sep = "_"))),
      demo_drugs_prcdrs = PatientLevelPrediction::loadPlpData(file.path(saveDirectory, analysisName, "processedData", paste(analysisName, "plpData_demo_drugs_prcdrs", sep = "_"))),
      demo_conds_drugs_prcdrs = PatientLevelPrediction::loadPlpData(file.path(saveDirectory, analysisName, "processedData", paste(analysisName, "plpData_demo_conds_drugs_prcdrs", sep = "_")))
    )
    plpDataList <- setNames(plpDataList, paste(analysisName, names(plpDataList), sep = "_"))
    population <- readRDS(file.path(saveDirectory, analysisName, "processedData", paste(analysisName, "studyPopulation.Rds", sep = "_")))
    plpDataList <- lapply(plpDataList, function(x) c(x, population = list(population)))
    plpDataList <- lapply(plpDataList, `class<-`, value = 'plpData')
    
    runModel(plpDataList = plpDataList, 
             outcomeId = databaseDetails$outcomeId, 
             analysisId = .y, 
             analysisName = analysisName,
             populationSettings = populationSettings, 
             splitSettings = splitSettings, 
             sampleSettings = sampleSettings, 
             featureEngineeringSettings = featureEngineeringSettings, 
             preprocessSettings = PatientLevelPrediction::createPreprocessSettings(),
             modelSettings = modelSettings, 
             logSettings = PatientLevelPrediction::createLogSettings(),
             executeSettings = executeSettings, 
             saveDirectory = saveDirectory)
  }
  ParallelLogger::logInfo(paste("runPlp finished."))
  gc()
  ParallelLogger::logInfo(paste("Benchmark run finished."))
  return(invisible())
  
}