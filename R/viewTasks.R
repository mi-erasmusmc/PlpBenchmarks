#' @export
viewBenchmarkTasks <- function(){
  
  problemList <- read.csv("extras/ProblemSpecification.csv")  %>%
    dplyr::rename(plpDataName = cohortName) %>%
    dplyr::as_tibble()
  
  return(problemList)
}

#' @export
loadBenchmarkDesignList <- function(){
  
  designList <- readRDS(system.file(package = "PLPBenchmarks", "data", "BenchmarkModelDesignList.Rds"))
  
  return(designList)
}

