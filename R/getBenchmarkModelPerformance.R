#' @title Get the mode performance.
#'
#' @description
#' Collect the performance of each model specified in the benchmark design.
#'
#' @param benchmarkDesign An object of class \code{benchmarkDesign}.
#'
#' @export
getBenchmarkModelPerformance <- function(benchmarkDesign) {
 
  benchmarkPerf <- checkmate::makeAssertCollection()
  
  checkmate::assert(
    checkmate::checkList(benchmarkDesign, types = "modelDesign", null.ok = FALSE, names = "named"),
    checkmate::checkClass(benchmarkDesign, classes = "benchmarkDesign", null.ok = FALSE),
    combine = "and",
    add = benchmarkPerf
  )
  
  checkmate::reportAssertions(benchmarkPerf)

  performanceList <- vector("list", length(benchmarkDesign))

  ParallelLogger::logInfo("Collecting model performance...")
  progressBar <- utils::txtProgressBar(style = 3)
  for (i in seq_along(benchmarkDesign)) {
    analysisName <- benchmarkDesign[[i]]$analysisName
    saveDirectory <- benchmarkDesign[[i]]$saveDirectory
    
    if(!file.exists(file.path(saveDirectory, "plpResult", "runPlp.rds"))) {
      
      plpPerformance <- data.frame("analysisName" = analysisName, 
                                   "metric" = NA,
                                   "Test" = NA,
                                   "Train" = NA,
                                   "CV" = NA 
                                   )
    } else {
      
      plpResult <- readRDS(file.path(saveDirectory, "plpResult", "runPlp.rds"))

    plpPerformance <- as.data.frame(sapply(plpResult$performanceEvaluation$evaluationStatistics, unlist)) %>%
      tidyr::pivot_wider(id_cols = metric, names_from = evaluation, values_from = value) %>%
      dplyr::mutate(analysisName = analysisName) %>%
      dplyr::select(analysisName, dplyr::everything()) 
    
    }

    performanceList[[i]] <- plpPerformance
    
    utils::setTxtProgressBar(progressBar, i)
  }

  close(progressBar)
  performanceDf <- do.call(rbind.data.frame, performanceList)
  return(performanceDf)
}
