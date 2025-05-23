#' @title View settings of a benchmark design
#'
#' @description
#' Provides the settings as a data frame for side-to-side comparisons.
#'
#' @param benchmarkDesign A list of class \code{modelDesign} created using
#' \code{PLPBenchmarks::createBenchmarkDesign()}
#'
#' @export
viewBenchmarkSettings <- function(benchmarkDesign) {
  
  benchmarkSettings <- checkmate::makeAssertCollection()
  
  checkmate::assert(
    checkmate::checkList(benchmarkDesign, types = "modelDesign", null.ok = FALSE, names = "named"),
    checkmate::checkClass(benchmarkDesign, classes = "benchmarkDesign", null.ok = FALSE),
    combine = "and",
    add = benchmarkSettings
    )
  
  checkmate::reportAssertions(benchmarkSettings)

  popSets <- vector("list", length(benchmarkDesign))
  popSets <- lapply(benchmarkDesign, "[[", "populationSettings")
  popSetsDf <- as.data.frame(t(do.call(rbind, popSets))) %>%
    dplyr::mutate(settings = "populationSettings", option = rownames(.)) %>%
    dplyr::select(settings, option, dplyr::everything())

  covSets <- lapply(benchmarkDesign, "[[", "covariateSettings")
  covSets2 <- lapply(covSets, function(x) {
    if (inherits(x, "list")) {
      x
    } else {
      lapply(x, as.character)
    }
  })
  covSets3 <- lapply(covSets2, function(x) tibble::enframe(unlist(x), name = "option", value = "value"))
  covSetsDf <- do.call(rbind, unname(Map(cbind, id = names(covSets3), covSets3))) %>%
    tidyr::pivot_wider(id_cols = "option", names_from = "id", values_from = "value", values_fn = function(x) ifelse(length(unique(x)), x[1], paste(x, collapse = ","))) %>%
    dplyr::mutate(settings = "covariateSettings") %>%
    dplyr::select(settings, option, dplyr::everything())

  modelSets <- lapply(benchmarkDesign, "[[", "modelSettings")
  modelSets <- lapply(modelSets, function(x) attributes(x$param)$settings[c("name", "seed")])
  modelSetsDf <- as.data.frame(t(do.call(rbind, modelSets))) %>%
    dplyr::mutate(settings = "modelSettings", option = rownames(.)) %>%
    dplyr::select(settings, option, dplyr::everything())

  sampleSets <- lapply(benchmarkDesign, "[[", "sampleSettings")
  sampleSets <- lapply(sampleSets, "[[", 1)
  sampleFun <- lapply(sampleSets, function(x) data.frame("fun" = attr(x, "fun")))
  sampleSetsDf <- as.data.frame(t(do.call(rbind, sampleFun))) %>%
    rbind(as.data.frame(t(do.call(rbind, sampleSets)))) %>%
    dplyr::mutate(settings = "sampleSettings", option = rownames(.)) %>%
    dplyr::select(settings, option, dplyr::everything())

  featEngSets <- lapply(benchmarkDesign, "[[", "featureEngineeringSettings")
  featEngSets <- lapply(featEngSets, "[[", 1)
  featEngSetsDf <- as.data.frame(t(do.call(rbind, featEngSets))) %>%
    dplyr::mutate(settings = "featureEngineeringSettings", option = rownames(.)) %>%
    dplyr::select(settings, option, dplyr::everything())

  splitSets <- lapply(benchmarkDesign, "[[", "splitSettings")
  splitSetsDf <- as.data.frame(t(do.call(rbind, splitSets))) %>%
    dplyr::mutate(settings = "splitSettings", option = rownames(.)) %>%
    dplyr::select(settings, option, dplyr::everything())

  preprocessSets <- lapply(benchmarkDesign, "[[", "preprocessSettings")
  preprocessSetsDf <- as.data.frame(t(do.call(rbind, preprocessSets))) %>%
    dplyr::mutate(settings = "preprocessSettings", option = rownames(.)) %>%
    dplyr::select(settings, option, dplyr::everything())

  executeSets <- lapply(benchmarkDesign, "[[", "executeSettings")
  executeSetsDf <- as.data.frame(t(do.call(rbind, executeSets))) %>%
    dplyr::mutate(settings = "executeSettings", option = rownames(.)) %>%
    dplyr::select(settings, option, dplyr::everything())

  benchmarkSettings <- attributes(benchmarkDesign)$benchmarkSettings %>%
    dplyr::select(analysisId, problemId, dplyr::everything())

  otherSetsDf <- as.data.frame(t(benchmarkSettings))
  names(otherSetsDf) <- benchmarkSettings$analysisId
  otherSetsDf <- otherSetsDf %>%
    dplyr::mutate(settings = "benchmarkSettings", option = rownames(.)) %>%
    dplyr::select(settings, option, dplyr::everything())

  result <- rbind(otherSetsDf, popSetsDf) %>%
    rbind(., covSetsDf) %>%
    rbind(., modelSetsDf) %>%
    rbind(., splitSetsDf) %>%
    rbind(., preprocessSetsDf) %>%
    rbind(., sampleSetsDf) %>%
    rbind(., featEngSetsDf) %>%
    rbind(., executeSetsDf)
  rownames(result) <- NULL

  return(result)
}
