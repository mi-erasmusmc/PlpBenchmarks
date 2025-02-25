#' @title View settings of a benchmark design
#' 
#' @description
#' Provides the settings as a dataframe for side-to-side comparissons. 
#' 
#' @param benchmarkDesign A list of class \code{modelDesign} created using 
#' \code{PLPBenchmarks::createBenchmarkDesign()}
#'
#' @export
viewBenchmarkSettings <- function(benchmarkDesign){
  
  checkmate::check_class(benchmarkDesign, "benchmarkDesign")
  
  popSets <- vector("list", length(benchmarkDesign))
  popSets <- lapply(benchmarkDesign, '[[', 'populationSettings')
  popSetsDf <- as.data.frame(t(do.call(rbind, popSets))) %>%
    dplyr::mutate(settings = "populationSettings", option = rownames(.)) %>%
    dplyr::select(settings, option, dplyr::everything())
  
  # covSets <- lapply(benchmarkDesign, '[[', 'covariateSettings')
  # covSets <- lapply(covSets, function(x) {
  #   x <- as.data.frame(t(do.call(rbind, x)))
  #   return(x)
  # })
  # covSetsDf <- as.data.frame(t(do.call(dplyr::bind_rows, covSets)))
  # names(covSetsDf) <- names(benchmarkDesign)
  # covSetsDf <- covSetsDf %>%
  #   dplyr::mutate(settings = "covariateSettings", option = rownames(.)) %>%
  #   dplyr::select(settings, option, dplyr::everything())
  covSets <- lapply(benchmarkDesign, '[[', 'covariateSettings')
  covSets2 <- lapply(covSets, function(x)
    if (length(x) == 2) {
      x
    } else {
      lapply(x, as.character)
    })
  covSets3 <- lapply(covSets2, function(x) tibble::enframe(unlist(x), name = "option",value = "value" ))
  covSetsDf <- do.call(rbind, unname(Map(cbind, id = names(covSets3), covSets3))) %>%
    tidyr::pivot_wider(id_cols = "option", names_from = "id", values_from = "value", values_fn = function(x) ifelse(length(unique(x)), x[1], paste(x, collapse = ","))) %>%
    dplyr::mutate(settings = "covariateSettings") %>%
    dplyr::select(settings, option, dplyr::everything())
  
  modelSets <- lapply(benchmarkDesign, '[[', "modelSettings")
  modelSets <- lapply(modelSets, function(x) attributes(x$param)$settings[c("name", "seed")])
  modelSetsDf <- as.data.frame(t(do.call(rbind, modelSets))) %>%
    dplyr::mutate(settings = "modelSettings", option = rownames(.)) %>%
    dplyr::select(settings, option, dplyr::everything())
  
  sampleSets <- lapply(benchmarkDesign, '[[', 'sampleSettings')
  sampleSets <- lapply(sampleSets, '[[', 1)
  sampleFun <- lapply(sampleSets, function(x) data.frame("fun" = attr(x, "fun")))
  sampleSetsDf <- as.data.frame(t(do.call(rbind, sampleFun))) %>%
    rbind(as.data.frame(t(do.call(rbind, sampleSets)))) %>%
    dplyr::mutate(settings = "sampleSettings", option = rownames(.)) %>%
    dplyr::select(settings, option, dplyr::everything())
  
  featEngSets <- lapply(benchmarkDesign, '[[', "featureEngineeringSettings")
  featEngSets <- lapply(featEngSets, '[[', 1)
  featEngSetsDf <- as.data.frame(t(do.call(rbind, featEngSets))) %>%
    dplyr::mutate(settings = "featureEngineeringSettings", option = rownames(.)) %>%
    dplyr::select(settings, option, dplyr::everything())
  
  splitSets <- lapply(benchmarkDesign, '[[', "splitSettings")
  splitSetsDf <- as.data.frame(t(do.call(rbind, splitSets))) %>%
    dplyr::mutate(settings = "splitSettings", option = rownames(.)) %>%
    dplyr::select(settings, option, dplyr::everything())
  
  preprocessSets <- lapply(benchmarkDesign, '[[', "preprocessSettings")
  preprocessSetsDf <- as.data.frame(t(do.call(rbind, preprocessSets))) %>%
    dplyr::mutate(settings = "preprocessSettings", option = rownames(.)) %>%
    dplyr::select(settings, option, dplyr::everything())
  
  executeSets <- lapply(benchmarkDesign, '[[', "executeSettings")
  executeSetsDf <- as.data.frame(t(do.call(rbind, executeSets))) %>%
    dplyr::mutate(settings = "executeSettings", option = rownames(.)) %>%
    dplyr::select(settings, option, dplyr::everything())
  
  uniquePopSets <- attributes(benchmarkDesign)$uniquePopulation %>%
    dplyr::select("problemId", "targetId", "outcomeId", "plpDataName",  "populationLocation")
  
  uniquePlpDataSets <- attributes(benchmarkDesign)$uniquePlpData %>%
    dplyr::select("problemId", "dataLocation")
  
  samePlpDataSets <- attributes(benchmarkDesign)$benchmarkSettings %>%
    dplyr::select("sameTargetAsProblemId")
  
  otherSets <- dplyr::right_join(uniquePopSets, uniquePlpDataSets, by = "problemId") %>%
    dplyr::left_join(., samePlpDataSets, by = dplyr::join_by("problemId"=="sameTargetAsProblemId"))
  otherSetsDf <- as.data.frame(t(otherSets)) 
  names(otherSetsDf) <- names(benchmarkDesign)
  otherSetsDf <- otherSetsDf %>%
    dplyr::mutate(settings = "benchmarkSettings", option = rownames(.)) %>%
    dplyr::select(settings, option, dplyr::everything())
     
  result <- rbind(otherSetsDf, popSetsDf) %>%
    rbind(., covSetsDf) %>%
    rbind(., modelSetsDf) %>%
    rbind(., splitSetsDf ) %>%
    rbind(., preprocessSetsDf) %>%
    rbind(., sampleSetsDf) %>%
    rbind(., featEngSetsDf) %>%
    rbind(., executeSetsDf)
  rownames(result) <- NULL
  
  return(result)
  
}
