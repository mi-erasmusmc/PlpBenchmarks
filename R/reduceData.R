#' @export
reduceData <- function(studyPopulation,
                       plpData, 
                       requiredTrainPositiveEvents, 
                       testSplitFraction, 
                       seed){
  
  summaryStats <- studyPopulation %>%
    dplyr::summarize(positiveOutcomeRate = sum(outcomeCount)/nrow(.), 
                     requiredTrainOutcomes = requiredTrainPositiveEvents, 
                     requiredOverallOutcomes = ceiling((1+testSplitFraction)*requiredTrainOutcomes), 
                     neededPopulation = ceiling((requiredOverallOutcomes)/positiveOutcomeRate), 
                     nonOutcomePopulation = ceiling(neededPopulation - requiredOverallOutcomes),
                     totalPop = nrow(.))
  
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
  
  result = list(studyPopulation = finalPopulation, 
                plpData = finalPlpData, 
                summaryStats = summaryStats)
  
  return(result)
}
