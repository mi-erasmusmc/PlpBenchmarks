#' @title Create sample settings to sample the train set.
#'
#' @details
#' Create a sampleSettings object containing a number of required outcomes to reduce the train set sample, maintaining the original outcome to non-outcome distribution. 
#' 
#' @description
#' Given a train set and a number of required outcomes, the train set outcomes are reduced to the number of required train outcomes, trying to maintain the original outcome to non-outcome distribution. 
#'
#' @param numberTrainSetOutcomes The absolute number of desired number of outcomes to retain in the train set.
#' @param sampleSeed A seed to use when sampling. 
#' 
#' @export
createSampleTrainSetSettings <- function(numberTrainSetOutcomes = NULL, 
                                         sampleSeed = sample(10000,1) 
                                         ){
  
  sampleSets <- checkmate::makeAssertCollection()
  
  checkmate::assert(
  checkmate::checkIntegerish(sampleSeed, len = 1, null.ok = FALSE),
  checkmate::checkIntegerish(sampleSeed),combine = "and", 
  add = sampleSets
  )
  
  checkmate::reportAssertions(collection = sampleSets)
  
  sampleSettings <- list(
    numberTrainSetOutcomes = numberTrainSetOutcomes,
    sampleSeed = sampleSeed
  )
  
  # ellipsisArgs <- list(...)
  # 
  # sampleSettings <- listAppend(a = sampleSettings, b = ellipsisArgs)
  
    attr(sampleSettings, "fun") <- "PLPBenchmarks:::reduceTrainSetOutcomes" 
  
  class(sampleSettings) <- "sampleSettings"
  return(sampleSettings)
  
}

#' @title Sample the train set.
#'
#' @details
#' Provide a number of required outcomes, to reduce the number the train set sample, maintaining the original outcome to non-outcome distribution. 
#' 
#' @description
#' Given a train set and a number of required outcomes, the train set outcomes are reduced to the number of required train outcomes, trying to maintain the original outcome to non-outcome distribution. 
#' @param trainData An object of class `trainData`.
#' @param sampleSettings An object of class `sampleSettings`.
#' 
reduceTrainSetOutcomes <- function(trainData, sampleSettings){
  
  checkmate::checkIntegerish(sampleSettings$sampleSeed)
  checkmate::checkIntegerish(sampleSettings$numberTrainSetOutcomes)
  
  ParallelLogger::logInfo(paste0('sampleSeed: ', sampleSettings$sampleSeed))
  
  ParallelLogger::logInfo(paste0('numberTrainSetOutcomes:', sampleSettings$numberTrainSetOutcomes))
  
  set.seed(sampleSettings$sampleSeed)
  ParallelLogger::logInfo(paste0('Starting sampling with seed ', sampleSettings$sampleSeed))
  
  population <- trainData$labels %>% dplyr::collect()
  folds <- trainData$folds %>% dplyr::collect()
  
  population <- merge(population, folds, by = 'rowId')
  numberOfFolds <- max(unique(trainData$folds$index))
  
  trainOutcomePrevalence <- sum(population$outcomeCount > 0) / nrow(population)
  sampleSize <- ceiling(sampleSettings$numberTrainSetOutcomes/trainOutcomePrevalence)
  numberOfOutcomeInEachFold <- ceiling(sampleSettings$numberTrainSetOutcomes / numberOfFolds)
  
  if(sampleSize > nrow(population)){
    ParallelLogger::logWarn('Population outcome count less than required sample size. Returning same population.')
    sampleSize <- nrow(population)
    
    return(trainData)
  } else {
    
    ParallelLogger::logInfo(paste0('Initial train data has ',sum(population$outcomeCount > 0),' outcomes to ',
                                   sum(population$outcomeCount == 0), ' non-outcomes'))
    
    pplOfInterest <- c()
    for(i in unique(folds$index)){
      outcomeIds <- population$rowId[population$outcomeCount > 0 & population$index == i]
      nonoutcomeIds <- population$rowId[population$outcomeCount == 0 & population$index == i]
      
      # trueOutcomePrevalence <- length(outcomeIds)/length(nonoutcomeIds)
      # # desiredTrainPopulation <- ceiling(sampleSettings$numberTrainSetOutcomes/trueOutcomePrevalence)
      # sampleSize <- ceiling(numberOfOutcomeInEachFold/trainOutcomePrevalence)
      
      # if(sampleSize > length(outcomeIds) + length(nonoutcomeIds)){
      #   ParallelLogger::logWarn('Population count less that require sample size. Returning same population.')
      #   sampleSize <- length(nonoutcomeIds) + length(outcomeIds)
      # }
      
      # randomly pick people
      sampleOutcomeIds <- sample(outcomeIds, size = numberOfOutcomeInEachFold)
      sampleNonOutcomeIds <- sample(nonoutcomeIds, size = ((sampleSize/numberOfFolds) - numberOfOutcomeInEachFold))
      newPopulationIds <- c(sampleOutcomeIds, sampleNonOutcomeIds)
      
      pplOfInterest <- c(pplOfInterest, newPopulationIds)
    }
    
    
    # filter to these patients 
    sampleTrainData <- list()
    class(sampleTrainData) <- 'plpData'
    sampleTrainData$labels <- trainData$labels %>% dplyr::filter(.data$rowId %in% pplOfInterest)
    sampleTrainData$folds <- trainData$folds %>% dplyr::filter(.data$rowId %in% pplOfInterest)
    
    sampleTrainData$covariateData <- Andromeda::andromeda()
    sampleTrainData$covariateData$covariateRef <- trainData$covariateData$covariateRef
    Andromeda::createIndex(trainData$covariateData$covariates, 
                           columnNames = c("rowId"),
                           indexName = "covariates_rowId"
    )
    sampleTrainData$covariateData$covariates <- trainData$covariateData$covariates %>% 
      dplyr::filter(.data$rowId %in% pplOfInterest)
    Andromeda::removeIndex(trainData$covariateData$covariates, 
                           columnNames = c("rowId"),
                           indexName = "covariates_rowId")
    
    #update metaData$populationSize = nrow(trainData$labels)
    metaData <- attr(trainData$covariateData, 'metaData')
    metaData$populationSize = nrow(sampleTrainData$labels)
    attr(sampleTrainData$covariateData, 'metaData') <- metaData
    
    class(sampleTrainData$covariateData) <- 'CovariateData'
    
    return(sampleTrainData)
  }
}