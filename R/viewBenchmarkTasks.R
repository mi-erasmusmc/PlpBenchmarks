#' @title View the benchmark tasks 
#' 
#' @description
#' Loads the benchmark tasks to be run on an OMOP-CDM.
#' 
#' @export
viewBenchmarkTasks <- function(){
  
  problemList <- read.csv(system.file(package = "PLPBenchmarks", "extdata", "ProblemSpecification.csv"))  %>%
    dplyr::rename(plpDataName = cohortName) %>%
    dplyr::as_tibble()
  
  return(problemList)
}


#' @title Loads the benchmark design list 
#' 
#' @description
#' Loads the benchmark design list. 
#' 
#' @export
loadBenchmarkDesignList <- function(){
  
  designList <- readRDS(system.file(package = "PLPBenchmarks", "extdata", "BenchmarkModelDesignList.Rds"))
  
  return(designList)
}

