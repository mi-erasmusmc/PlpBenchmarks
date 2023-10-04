hfint2dm_populationSettings <- 
  PatientLevelPrediction::createStudyPopulationSettings(
    washoutPeriod = 0, 
    firstExposureOnly = FALSE,
    removeSubjectsWithPriorOutcome = FALSE,
    priorOutcomeLookback = 99999,
    riskWindowStart = 1,
    riskWindowEnd = 365,
    startAnchor =  'cohort start',
    endAnchor =  'cohort start',
    minTimeAtRisk = 364,
    requireTimeAtRisk = FALSE,
    includeAllOutcomes = TRUE)

coverF_populationSettings <- 
  PatientLevelPrediction::createStudyPopulationSettings(
    washoutPeriod = 365, 
    firstExposureOnly = FALSE,
    removeSubjectsWithPriorOutcome = FALSE,
    priorOutcomeLookback = 99999,
    riskWindowStart = 1,
    riskWindowEnd = 30,
    startAnchor =  'cohort start',
    endAnchor =  'cohort start',
    minTimeAtRisk = 364,
    requireTimeAtRisk = FALSE,
    includeAllOutcomes = TRUE)

coverI_populationSettings <- 
  PatientLevelPrediction::createStudyPopulationSettings(
    washoutPeriod = 365, 
    firstExposureOnly = FALSE,
    removeSubjectsWithPriorOutcome = FALSE,
    priorOutcomeLookback = 99999,
    riskWindowStart = 1,
    riskWindowEnd = 30,
    startAnchor =  'cohort start',
    endAnchor =  'cohort start',
    minTimeAtRisk = 364,
    requireTimeAtRisk = FALSE,
    includeAllOutcomes = TRUE)

coverH_populationSettings <- 
  PatientLevelPrediction::createStudyPopulationSettings(
    washoutPeriod = 365, 
    firstExposureOnly = FALSE,
    removeSubjectsWithPriorOutcome = FALSE,
    priorOutcomeLookback = 99999,
    riskWindowStart = 1,
    riskWindowEnd = 30,
    startAnchor =  'cohort start',
    endAnchor =  'cohort start',
    minTimeAtRisk = 364,
    requireTimeAtRisk = FALSE,
    includeAllOutcomes = TRUE)

MNCSwithCreatinine_populationSettings <- 
  PatientLevelPrediction::createStudyPopulationSettings(
    washoutPeriod = 0, 
    firstExposureOnly = FALSE,
    removeSubjectsWithPriorOutcome = FALSE,
    priorOutcomeLookback = 99999,
    riskWindowStart = 1,
    riskWindowEnd = 30,
    startAnchor =  'cohort start',
    endAnchor =  'cohort start',
    minTimeAtRisk = 30,
    requireTimeAtRisk = FALSE,
    includeAllOutcomes = TRUE)

MNCSwithoutCreatinine_populationSettings <- 
  PatientLevelPrediction::createStudyPopulationSettings(
    washoutPeriod = 0, 
    firstExposureOnly = FALSE,
    removeSubjectsWithPriorOutcome = FALSE,
    priorOutcomeLookback = 99999,
    riskWindowStart = 1,
    riskWindowEnd = 30,
    startAnchor =  'cohort start',
    endAnchor =  'cohort start',
    minTimeAtRisk = 30,
    requireTimeAtRisk = FALSE,
    includeAllOutcomes = TRUE)