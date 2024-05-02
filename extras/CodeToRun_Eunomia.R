library(PatientLevelPrediction)
library(Eunomia)
library(dplyr)
library(PLPBenchmarks)

connectionDetails <- getEunomiaConnectionDetails()
Eunomia::createCohorts(connectionDetails = connectionDetails)

analysisName = "GIBinCLXB"
saveDirectory = "eunomiaResultsSingle6"
requiredTrainPositiveEvents = 450
seed = 42
covariateComparisson = "demographics, conditions, drugs and procedures"
cohortDefinitions <- tibble( targetId = c(1, 2, 4, 1), 
                             outcomeId = c(3, 3, 3, 3),
                             riskStart = c(0,0,0, 0), 
                             riskEnd = c(365, 365, 365, 90), 
                             analysisName = c("GIBinCLXB", "GIBinDCLFNC", "GIBinNSAIDs", "GIBinCLXB_90"),
                             plpDataName = c("GIBinCLXB", "GIBinDCLFNC", "GIBinNSAIDs", "GIBinCLXB"), 
                             targetJsonLocation = rep("Eunomia", 4), 
                             outcomeJsonLocation = rep("Eunomia", 4))
cohortDefinitions <- cohortDefinitions[1,]

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
# covariateSettings_demo_conds <- FeatureExtraction::createCovariateSettings(useDemographicsGender = T, 
#                                                                            useDemographicsAge = T, 
#                                                                            useConditionOccurrenceLongTerm = T, 
#                                                                            longTermStartDays = -365,
#                                                                            endDays = -1)
# covariateSettings_demo_conds_drugs <- FeatureExtraction::createCovariateSettings(useDemographicsGender = T, 
#                                                                                  useDemographicsAge = T, 
#                                                                                  useConditionOccurrenceLongTerm = T,
#                                                                                  useDrugGroupEraLongTerm = T, 
#                                                                                  longTermStartDays = -365,
#                                                                                  endDays = -1)
covariateSettings_demo_conds_drugs_prcdrs <- FeatureExtraction::createCovariateSettings(useDemographicsGender = T, 
                                                                                 useDemographicsAge = T, 
                                                                                 useConditionOccurrenceLongTerm = T,
                                                                                 useDrugGroupEraLongTerm = T,
                                                                                 useProcedureOccurrenceLongTerm = T,
                                                                                 longTermStartDays = -365,
                                                                                 endDays = -1)

modelDesign <- createModelDesign(targetId = databaseDetails$targetId,
                                 outcomeId = databaseDetails$outcomeIds,
                                 restrictPlpDataSettings = restrictPlpDataSettings,
                                 populationSettings = populationSettings, 
                                 covariateSettings = covariateSettings_demo_conds_drugs_prcdrs,
                                 featureEngineeringSettings = createFeatureEngineeringSettings(), 
                                 sampleSettings = createSampleSettings(),
                                 preprocessSettings = createPreprocessSettings(), 
                                 modelSettings = lassoModel, 
                                 splitSettings = createDefaultSplitSetting(), 
                                runCovariateSummary = TRUE)

analysisDesign <- createBenchmarkDesign(modelDesign = modelDesign,
                                       databaseDetails = databaseDetails,
                                       requiredTrainPositiveEvents = requiredTrainPositiveEvents,
                                       covariateComparisson = covariateComparisson,
                                       analysisNames = analysisName,
                                       saveDirectory = saveDirectory)

benchmarkSettings <- createBenchmarkAnalysisSettings(analysisDesignList = list(analysisDesign), cohortDefinitions = cohortDefinitions)

executeBenchmark(benchmarkDesign = analysisDesign, 
                 analysisSettings = benchmarkSettings,
                 cohortDefinitions = cohortDefinitions, 
                 databaseDetails = databaseDetails, 
                 jsonFileLocation = NULL,
                 analysisName = analysisDesign$analysisName,
                 saveDirectory = saveDirectory, 
                 createCohorts = F,
                 extractData = T,
                 prepareData = T, 
                 runPlp = T 
                 )
