library(PatientLevelPrediction)
library(dplyr)
library(ParallelLogger)
library(PLPBenchmarks)

###########################################
#User specified input
saveDirectory = "Results"
requiredTrainPositiveEvents = 3500
seed = 42
cdmDatabaseSchema = ""
cdmDatabaseName = ""
cdmDatabaseId = ""
cohortDatabaseSchema = ""
outcomeDatabaseSchema = ""
cohortTable = ""

dbms = ""
user = ""
pw = ""
connecionString = ""
server = ""
port = ""
extraSettings = ""

###########################################

#Do not edit 
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                # connectionString = connectionString,
                                                                user = user,
                                                                password = pw,
                                                                server = server,
                                                                port = port, 
                                                                extraSettings = extraSettings)

# databaseDetails <- createDatabaseDetails(connectionDetails = connectionDetails, 
#                                          cdmDatabaseSchema = "main",
#                                          cdmDatabaseName = "Eunomia",cdmDatabaseId = "Eunomia", 
#                                          cohortDatabaseSchema = "main",
#                                          cohortTable = "cohort",
#                                          outcomeDatabaseSchema = "main",
#                                          outcomeTable = "cohort", 
#                                          targetId = 1, 
#                                          outcomeIds = 3, cohortId = 1)
# restrictPlpDataSettings <- PatientLevelPrediction::createRestrictPlpDataSettings()
# populationSettings <- PatientLevelPrediction::createStudyPopulationSettings(binary = T,
#                                                                             includeAllOutcomes = F,
#                                                                             firstExposureOnly = T, 
#                                                                             washoutPeriod = 0, 
#                                                                             removeSubjectsWithPriorOutcome = T, 
#                                                                             priorOutcomeLookback = 99999,
#                                                                             requireTimeAtRisk = T, 
#                                                                             minTimeAtRisk = 1, 
#                                                                             riskWindowStart = 1, 
#                                                                             riskWindowEnd = 365
# )
lassoModel <- PatientLevelPrediction::setLassoLogisticRegression(seed=seed)
# covariateSettings_demo <- FeatureExtraction::createCovariateSettings(useDemographicsGender = T, useDemographicsAge = T)
# covariateSettings_demo_conds <- FeatureExtraction::createCovariateSettings(useDemographicsGender = T, 
#                                                                            useDemographicsAge = T, 
#                                                                            useConditionOccurrenceLongTerm = T, 
#                                                                            longTermStartDays = -365,
#                                                                            endDays = -1)
covariateSettings_demo_conds_drugs <- FeatureExtraction::createCovariateSettings(useDemographicsGender = T, 
                                                                                 useDemographicsAge = T, 
                                                                                 useConditionOccurrenceLongTerm = T,
                                                                                 useDrugGroupEraLongTerm = T, 
                                                                                 longTermStartDays = -365,
                                                                                 endDays = -1)

problemList <- read.csv("extras/ProblemSpecification.csv") 

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

analysisList <- vector("list", length(nrow(multipleAnalyses)))
for (i in seq_along(1:nrow(multipleAnalyses))) {
  analysisList[[i]] <- list(
    databaseDetails = databaseDetailsList[[i]], 
    restrictPlpDataSettings = restrictPlpDataSettingsList[[i]],
    requiredTrainPositiveEvents = requiredTrainPositiveEvents, 
    populationSettings = studyPopulationSettingsList[[i]], 
    analysisName = problemList$Problem_Name[i], 
    covariateSettings = covariateSettings_demo_conds_drugs,
    saveDirectory = saveDirectory,
    modelSettings = lassoModel
  )
}

wrappedExecute <- function(analysisList, seed = 42){
  databaseDetails = analysisList$databaseDetails
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
  cluster <- ParallelLogger::makeCluster(numberOfThreads = 3)
  ParallelLogger::clusterApply(cluster, largeVector, wrappedExecute)
  ParallelLogger::stopCluster(cluster)
}

executeBenchmarkParallel(analysisList)