#' @export
createComparissonCovariates <- function(plpData,
                                        studyPopulation,
                                        covariates = c("demographics only",
                                                       "demographics and conditions", 
                                                       "demographics and drugs", 
                                                       "demographics and procedures", 
                                                       "demographics, conditions and drugs",
                                                       "demographics, conditions and procedures", 
                                                       "demographics, drugs and procedures")){
  
  newPlpData <- list()
  class(newPlpData) <- 'plpData'
  newPlpData$covariateData <- Andromeda::copyAndromeda(plpData$covariateData)
  class(newPlpData$covariateData) <- 'CovariateData'
  
  newPlpData$cohorts <- plpData$cohorts
  newPlpData$outcomes <- plpData$outcomes
  newPlpData$timeRef <- ifelse(is.null(plpData$timeRef), "NULL", plpData$timeRef)
  newPlpData$metaData <- plpData$metaData
  
  # Andromeda::createIndex(newPlpData$covariateData$covariates, 
  #                        columnNames = 'covariateId', 
  #                        indexName = 'covariates_covariateIds')
  
  if(covariates == "demographics only"){
    
    includedCovariateIds <- plpData$covariateData$covariateRef %>% 
      dplyr::filter(analysisId %in% c(1:12)) %>%
      dplyr::pull(covariateId)
    
    newPlpData$covariateData$covariateRef <- newPlpData$covariateData$covariateRef %>%
      dplyr::filter(covariateId %in% includedCovariateIds)
    newPlpData$covariateData$covariates <- newPlpData$covariateData$covariates %>%
      dplyr::filter(covariateId %in% includedCovariateIds)
  }
  
  if(covariates == "demographics and conditions"){
    
    includedCovariateIds <- plpData$covariateData$covariateRef %>% 
      dplyr::filter(analysisId %in% c(1:12, 100:216)) %>%
      dplyr::pull(covariateId)
    
    newPlpData$covariateData$covariateRef <- newPlpData$covariateData$covariateRef %>%
      dplyr::filter(covariateId %in% includedCovariateIds)
    newPlpData$covariateData$covariates <- newPlpData$covariateData$covariates %>%
      dplyr::filter(covariateId %in% includedCovariateIds)
  }
  
  if(covariates == "demographics and drugs"){
    
    includedCovariateIds <- plpData$covariateData$covariateRef %>% 
      dplyr::filter(analysisId %in% c(1:12, 300:416)) %>%
      dplyr::pull(covariateId)
    
    newPlpData$covariateData$covariateRef <- newPlpData$covariateData$covariateRef %>%
      dplyr::filter(covariateId %in% includedCovariateIds)
    newPlpData$covariateData$covariates <- newPlpData$covariateData$covariates %>%
      dplyr::filter(covariateId %in% includedCovariateIds)
  }
  
  if(covariates == "demographics and procedures"){
    
    includedCovariateIds <- plpData$covariateData$covariateRef %>% 
      dplyr::filter(analysisId %in% c(1:12, 500:599)) %>%
      dplyr::pull(covariateId)
    
    newPlpData$covariateData$covariateRef <- newPlpData$covariateData$covariateRef %>%
      dplyr::filter(covariateId %in% includedCovariateIds)
    newPlpData$covariateData$covariates <- newPlpData$covariateData$covariates %>%
      dplyr::filter(covariateId %in% includedCovariateIds)
  }
  
  if(covariates == "demographics, conditions and drugs"){
    
    includedCovariateIds <- plpData$covariateData$covariateRef %>% 
      dplyr::filter(analysisId %in% c(1:12, 100:216, 300:416)) %>%
      dplyr::pull(covariateId)
    
    newPlpData$covariateData$covariateRef <- newPlpData$covariateData$covariateRef %>%
      dplyr::filter(covariateId %in% includedCovariateIds)
    newPlpData$covariateData$covariates <- newPlpData$covariateData$covariates %>%
      dplyr::filter(covariateId %in% includedCovariateIds)
  }
  
  if(covariates == "demographics, conditions and procedures"){
    
    includedCovariateIds <- plpData$covariateData$covariateRef %>% 
      dplyr::filter(analysisId %in% c(1:12, 100:216, 500:599)) %>%
      dplyr::pull(covariateId)
    
    newPlpData$covariateData$covariateRef <- newPlpData$covariateData$covariateRef %>%
      dplyr::filter(covariateId %in% includedCovariateIds)
    newPlpData$covariateData$covariates <- newPlpData$covariateData$covariates %>%
      dplyr::filter(covariateId %in% includedCovariateIds)
  }
  
  if(covariates == "demographics, drugs and procedures"){
    
    includedCovariateIds <- plpData$covariateData$covariateRef %>% 
      dplyr::filter(analysisId %in% c(1:12, 300:416, 500:599)) %>%
      dplyr::pull(covariateId)
    
    newPlpData$covariateData$covariateRef <- newPlpData$covariateData$covariateRef %>%
      dplyr::filter(covariateId %in% includedCovariateIds)
    newPlpData$covariateData$covariates <- newPlpData$covariateData$covariates %>%
      dplyr::filter(covariateId %in% includedCovariateIds)
  }
  # attaching the study population
  newPlpData$population <- studyPopulation
  
  # Andromeda::removeIndex(tbl = newPlpData$covariateData$covariates, 
  #                        indexName = 'covariates_covariateIds')
  
  metaData <- attr(plpData$covariateData, 'metaData')
  attr(newPlpData$covariateData, 'metaData') <- metaData
  
  return(newPlpData)
}
