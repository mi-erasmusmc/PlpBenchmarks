library(PatientLevelPrediction)
library(Eunomia)
library(dplyr)
library(PLPBenchmarks)

###########################################
#User specified input
jsonfileLocation = "cohort definition json files location"
saveDirectory = "Where to save the results" # Note that adequate space is required to run the whole study
numberOfThreads = #Change this to enable parallelization
requiredTrainPositiveEvents = 3500 #The number of positive event counts to ensure are included in the study population for each problem
seed = 42
cdmDatabaseSchema = ""
cdmDatabaseName = ""
cdmDatabaseId = ""
cohortDatabaseSchema = ""
outcomeDatabaseSchema = ""
cohortTable = ""

dbms = "" # Prefferable to use the keyring package so that the parallelization works.
user = ""
pw = ""
server = ""
port = ""
###########################################

#Do not edit 
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                user = user,
                                                                password = pw,
                                                                server = server,
                                                                port = port
                                                                )

## Filling in PatientLevelPredicton settings
lassoModel <- PatientLevelPrediction::setLassoLogisticRegression(seed=seed)

### Covariate settings
### We only need to define the biggest set of candidate covariates: currently only supporting , demo, conds and drugs.
covariateSettings_demo_conds_drugs <- FeatureExtraction::createCovariateSettings(useDemographicsGender = T, 
                                                                                 useDemographicsAge = T, 
                                                                                 useConditionOccurrenceLongTerm = T,
                                                                                 useDrugGroupEraLongTerm = T, 
                                                                                 longTermStartDays = -365,
                                                                                 endDays = -1)

### List of problems to run
problemList <- read.csv("extras/ProblemSpecification.csv") 

### Popoulation settings
studyPopulationSettingsList <- vector("list", length(nrow(problemList)))
for (i in seq_along(1:nrow(problemList))) {
  if(problemList$cohortId_Target[i] %in% c(8, 20)){
    studyPopulationSettingsList[[i]]<- PatientLevelPrediction::createStudyPopulationSettings(binary = T,
                                                                                             includeAllOutcomes = F,
                                                                                             firstExposureOnly = F, 
                                                                                             washoutPeriod = 0, 
                                                                                             removeSubjectsWithPriorOutcome = T, 
                                                                                             priorOutcomeLookback = 99999,
                                                                                             requireTimeAtRisk = T, 
                                                                                             minTimeAtRisk = 1, 
                                                                                             riskWindowStart = problemList$TAR_start_day[i], 
                                                                                             riskWindowEnd = problemList$TAR_end_day[i])
  } else if (problemList$TAR_start_day[i] == 0){
    studyPopulationSettingsList[[i]]<- PatientLevelPrediction::createStudyPopulationSettings(binary = T,
                                                                                             includeAllOutcomes = F,
                                                                                             firstExposureOnly = T, 
                                                                                             washoutPeriod = 0, 
                                                                                             removeSubjectsWithPriorOutcome = T, 
                                                                                             priorOutcomeLookback = 99999,
                                                                                             requireTimeAtRisk = T, 
                                                                                             minTimeAtRisk = 0, 
                                                                                             riskWindowStart = problemList$TAR_start_day[i], 
                                                                                             riskWindowEnd = problemList$TAR_end_day[i])
  } else {
  studyPopulationSettingsList[[i]]<- PatientLevelPrediction::createStudyPopulationSettings(binary = T,
                                                                                       includeAllOutcomes = F,
                                                                                       firstExposureOnly = T, 
                                                                                       washoutPeriod = 0, 
                                                                                       removeSubjectsWithPriorOutcome = T, 
                                                                                       priorOutcomeLookback = 99999,
                                                                                       requireTimeAtRisk = T, 
                                                                                       minTimeAtRisk = 1, 
                                                                                       riskWindowStart = problemList$TAR_start_day[i], 
                                                                                       riskWindowEnd = problemList$TAR_end_day[i])
  }
}

names(studyPopulationSettingsList) <- paste0(problemList$Problem_Name, "_studyPopSettings")

### Database details
databaseDetailsList <- vector("list", length(nrow(problemList)))
for (i in seq_along(1:nrow(problemList))) {
  databaseDetailsList[[i]]<- PatientLevelPrediction::createDatabaseDetails(connectionDetails = connectionDetails, 
                                                                           cdmDatabaseSchema = cdmDatabaseSchema,
                                                                           cdmDatabaseName = cdmDatabaseName,
                                                                           cdmDatabaseId = cdmDatabaseId, 
                                                                           cohortDatabaseSchema = cohortDatabaseSchema,
                                                                           cohortTable = cohortTable,
                                                                           outcomeDatabaseSchema = outcomeDatabaseSchema,
                                                                           outcomeTable = cohortTable, 
                                                                           targetId = problemList$cohortId_Target[i], 
                                                                           outcomeIds = problemList$cohortId_Outcome[i])
}
names(databaseDetailsList) <- paste0(problemList$Problem_Name, "_DbDetails")

### Restrict plp settings

restrictPlpDataSettingsList <- vector("list", length(nrow(problemList)))
for (i in seq_along(1:nrow(problemList))) {
  if (problemList$cohortId_Target[i] == 8){
    restrictPlpDataSettingsList[[i]]<- PatientLevelPrediction::createRestrictPlpDataSettings(studyStartDate = "20190101",
                                                                                             studyEndDate = "20191231")
  } else {
  restrictPlpDataSettingsList[[i]]<- PatientLevelPrediction::createRestrictPlpDataSettings()
  }
}
names(restrictPlpDataSettingsList) <- paste0(problemList$Problem_Name, "_restrictSettinngs")

## Creating cohorts

PLPBenchmarks::createCohorts(jsonfileLocation = jsonfileLocation, 
                             connectionDetails = connectionDetails,
                             cdmDatabaseSchema = cdmDatabaseSchema,
                             cohortDatabaseSchema = cohortDatabaseSchema, 
                             cohortTable = cohortTable)

cohortCounts <- CohortGenerator::getCohortCounts(connectionDetails = connectionDetails,
                                                 cohortDatabaseSchema = cohortDatabaseSchema,
                                                 cohortTable = cohortTable)


## Developing models

wrappedExecute <- function(analysisList, seed = 42, dbms = dbms){
  
  restrictPlpDataSettings = analysisList$restrictPlpDataSettings
  requiredTrainPositiveEvents = analysisList$requiredTrainPositiveEvents  
  populationSettings = analysisList$populationSettings
  analysisName = analysisList$analysisName
  covariateSettings = analysisList$covariateSettings
  modelSettings = analysisList$modelSettings
  saveDirectory = analysisList$saveDirectory
  
  PLPBenchmarks::executeBenchmark(databaseDetails = databaseDetails, 
                                  restrictPlpDataSettings = restrictPlpDataSettings,
                                  populationSettings = populationSettings, 
                                  requiredTrainPositiveEvents = requiredTrainPositiveEvents, 
                                  covariateSettings = covariateSettings,
                                  modelSettings = modelSettings, 
                                  featureEngineeringSettings = PatientLevelPrediction::createFeatureEngineeringSettings(),
                                  sampleSettings = PatientLevelPrediction::createSampleSettings(), 
                                  splitSettings = PatientLevelPrediction::createDefaultSplitSetting(splitSeed = seed), 
                                  executeSettings = PatientLevelPrediction::createExecuteSettings(runSplitData = T,
                                                                                                  runSampleData = F, 
                                                                                                  runfeatureEngineering = F,
                                                                                                  runPreprocessData = T,
                                                                                                  runModelDevelopment = T, 
                                                                                                  runCovariateSummary = T), 
                                  analysisName = analysisName,
                                  saveDirectory = saveDirectory, 
                                  createCohorts = F,
                                  prepareData = T, 
                                  runPlp = T )
  
  return(invisible())
}

executeBenchmarkParallel <- function(largeVector) {
  t1 <- Sys.time()
  
  cluster <- ParallelLogger::makeCluster(numberOfThreads = numberOfThreads)
  ParallelLogger::clusterApply(cluster, largeVector, wrappedExecute)
  ParallelLogger::stopCluster(cluster)
  
  tt <- Sys.time() - t1
  print(tt)
  return(tt)
}

benchTime <-executeBenchmarkParallel(analysisList)
