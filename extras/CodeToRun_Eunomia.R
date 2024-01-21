library(PatientLevelPrediction)
library(Eunomia)
library(dplyr)
library(PLPBenchmarks)

connectionDetails <- getEunomiaConnectionDetails()
Eunomia::createCohorts(connectionDetails = connectionDetails)

analysisName = "GIBinCLXB"
saveDirectory = "eunomiaResults"
requiredTrainPositiveEvents = 50
seed = 42
databaseDetails <- createDatabaseDetails(connectionDetails = connectionDetails, 
                                         cdmDatabaseSchema = "main",
                                         cdmDatabaseName = "Eunomia",cdmDatabaseId = "Eunomia", 
                                         cohortDatabaseSchema = "main",
                                         cohortTable = "cohort",
                                         outcomeDatabaseSchema = "main",
                                         outcomeTable = "cohort", 
                                         targetId = 1, 
                                         outcomeIds = 3, cohortId = 1)
restrictPlpDataSettings <- PatientLevelPrediction::createRestrictPlpDataSettings()
populationSettings <- PatientLevelPrediction::createStudyPopulationSettings(binary = T,
                                                                            includeAllOutcomes = F,
                                                                            firstExposureOnly = T, 
                                                                            washoutPeriod = 0, 
                                                                            removeSubjectsWithPriorOutcome = T, 
                                                                            priorOutcomeLookback = 99999,
                                                                            requireTimeAtRisk = T, 
                                                                            minTimeAtRisk = 1, 
                                                                            riskWindowStart = 1, 
                                                                            riskWindowEnd = 365
)
lassoModel <- PatientLevelPrediction::setLassoLogisticRegression(seed=seed)
covariateSettings_demo <- FeatureExtraction::createCovariateSettings(useDemographicsGender = T, useDemographicsAge = T)
covariateSettings_demo_conds <- FeatureExtraction::createCovariateSettings(useDemographicsGender = T, 
                                                                           useDemographicsAge = T, 
                                                                           useConditionOccurrenceLongTerm = T, 
                                                                           longTermStartDays = -365,
                                                                           endDays = -1)
covariateSettings_demo_conds_drugs <- FeatureExtraction::createCovariateSettings(useDemographicsGender = T, 
                                                                                 useDemographicsAge = T, 
                                                                                 useConditionOccurrenceLongTerm = T,
                                                                                 useDrugGroupEraLongTerm = T, 
                                                                                 longTermStartDays = -365,
                                                                                 endDays = -1)

executeBenchmark(databaseDetails = databaseDetails, 
                 restrictPlpDataSettings = restrictPlpDataSettings,
                 populationSettings = populationSettings, 
                 requiredTrainPositiveEvents = requiredTrainPositiveEvents, 
                 covariateSettings = covariateSettings_demo_conds_drugs,
                 modelSettings = lassoModel, 
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
                 runPlp = T 
                 )
