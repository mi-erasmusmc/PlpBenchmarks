#' @title Get the mode performance.
#' 
#' @description
#' Collect the performance of each model specified in the benchmark design.
#' 
#' @param benchmarkDesign An object of class \code{benchmarkDesign}.
#' 
#' @export
getBenchmarkModelPerformance <- function(benchmarkDesign){
  
  checkmate::check_null(benchmarkDesign)
  checkmate::check_class(benchmarkDesign, "benchmarkDesign")
  
  performanceList <- vector("list", length(benchmarkDesign))
  
  for (i in seq_along(benchmarkDesign)) {
    
    analysisName = benchmarkDesign[[i]]$analysisName
    saveDirectory = benchmarkDesign[[i]]$saveDirectory
    
    plpResult <- readRDS(file.path(saveDirectory, "plpResult", "runPlp.rds")) 
    
    plpPerformance <- as.data.frame(sapply(plpResult$performanceEvaluation$evaluationStatistics, unlist)) %>%
      tidyr::pivot_wider(id_cols = metric, names_from = evaluation, values_from = value) %>%
      dplyr::mutate(analysisName = analysisName) %>%
      dplyr::select(analysisName, everything())
    
    performanceList[[i]] <- plpPerformance
  }
  
  performanceDf <- do.call(rbind.data.frame, performanceList)
  return(performanceDf)
}