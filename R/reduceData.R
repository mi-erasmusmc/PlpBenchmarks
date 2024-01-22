#' @export
reduceData <- function(studyPopulation,
                       plpData, 
                       requiredTrainPositiveEvents, 
                       testSplitFraction, 
                       seed){
  
  summaryStats <- summariseStudyPopulation(studyPopulation = studyPopulation, 
                                           requiredTrainPositiveEvents = requiredTrainPositiveEvents, 
                                           testSplitFraction = testSplitFraction)
  
  if (requiredTrainPositiveEvents >= sum(studyPopulation$outcomeCount)){
    warning(paste("Number of outcomes is less than the required number of positive events. Returning the same data..."))
    finalPopulation <- studyPopulation
    finalPlpData <- plpData
  } else {
  set.seed(seed)
  selectedOutcomes <- studyPopulation %>%
    dplyr::filter(outcomeCount == 1) %>%
    dplyr::slice_sample(n = summaryStats$requiredOverallOutcomes) %>%
    dplyr::pull(rowId)
  
  set.seed(seed)
  selectedNonOutcomes <- studyPopulation %>%
    dplyr::filter(outcomeCount == 0) %>%
    dplyr::slice_sample(n = summaryStats$nonOutcomePopulation) %>%
    dplyr::pull(rowId)
  
  popIds <- c(selectedNonOutcomes, selectedOutcomes)
  
  finalPopulation <- studyPopulation %>%
    dplyr::filter(rowId %in% popIds)
  
  finalPlpData <- list()
  class(finalPlpData) <- 'plpData'
  finalPlpData$covariateData <- Andromeda::andromeda()
  finalPlpData$covariateData$covariates <- plpData$covariateData$covariates %>%
    dplyr::filter(rowId %in% popIds)
  finalPlpData$covariateData$covariateRef <- plpData$covariateData$covariateRef
  
  finalPlpData$cohorts <- plpData$cohorts %>%
    dplyr::filter(rowId %in% popIds)
  finalPlpData$outcomes <- plpData$outcomes %>%
    dplyr::filter(rowId %in% popIds)
  finalPlpData$timeRef <- plpData$timeRef
  finalPlpData$population <- finalPopulation
  finalPlpData$metaData <- plpData$metaData
  
  metaData <- attr(plpData$covariateData, 'metaData')
  metaData$populationSize = length(popIds)
  attr(finalPlpData$covariateData, 'metaData') <- metaData
  
  class(finalPlpData$covariateData) <- 'CovariateData'
  }
  
  result = list(studyPopulation = finalPopulation, 
                plpData = finalPlpData, 
                summaryStats = summaryStats)
  
  return(result)
}

#' @export
summariseStudyPopulation <- function(studyPopulation, 
                                     requiredTrainPositiveEvents, 
                                     testSplitFraction){
  
  studyPopSummary <- studyPopulation %>%
    dplyr::summarize(eventCount = sum(outcomeCount), 
                     eventRate = eventCount/length(unique(.data$subjectId)),
                     requiredTrainOutcomes = requiredTrainPositiveEvents,
                     requiredOverallOutcomes = ceiling((1+testSplitFraction)*requiredTrainOutcomes),
                     neededPopulation = ceiling((requiredOverallOutcomes)/eventRate),
                     nonOutcomePopulation = ceiling(neededPopulation - requiredOverallOutcomes),
                     totalPeople = length(unique(.data$subjectId)), 
                     totalRecords = length(.data$rowId))
  
  if (studyPopSummary$neededPopulation > studyPopSummary$totalPeople){
    warning(paste("The required number of people is greater than the number of people in the overall population. Something went wrong..."))
  }
  
  return(studyPopSummary)
  
}
