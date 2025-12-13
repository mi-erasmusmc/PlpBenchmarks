library(checkmate)

test_that("sampling works", {

  skip_if_not_installed("Eunomia")
  Eunomia::createCohorts(connectionDetails = connectionDetails)
  extractBenchmarkData(benchmarkDesign = eunomiaBenchmarkDesign_toSample, createStudyPopulation = T)
  suppressWarnings({
    runBenchmarkDesign(benchmarkDesign = eunomiaBenchmarkDesign_toSample)
    performance <- getBenchmarkModelPerformance(benchmarkDesign = eunomiaBenchmarkDesign_toSample)
  })
  
  expect_equal(as.double(performance$performanceMetrics %>% dplyr::filter(metric == "outcomeCount") %>% dplyr::pull(Train)), as.vector(sapply(sapply(eunomiaBenchmarkDesign_toSample, "[[", "sampleSettings"), "[[", "numberTrainSetOutcomes")))
  expect_equal(as.double(performance$performanceMetrics %>% dplyr::filter(metric == "outcomeCount") %>% dplyr::pull(CV)), as.vector(sapply(sapply(eunomiaBenchmarkDesign_toSample, "[[", "sampleSettings"), "[[", "numberTrainSetOutcomes")))
})


test_that("sampling works 2", {
  
  skip_if_not_installed("Eunomia")
  Eunomia::createCohorts(connectionDetails = connectionDetails)
  extractBenchmarkData(benchmarkDesign = eunomiaBenchmarkDesign_toSample2, createStudyPopulation = T)
  suppressWarnings({
    runBenchmarkDesign(benchmarkDesign = eunomiaBenchmarkDesign_toSample2)
    performance <- getBenchmarkModelPerformance(benchmarkDesign = eunomiaBenchmarkDesign_toSample2)
  })
  
  expect_equal(as.double(performance$performanceMetrics %>% dplyr::filter(metric == "outcomeCount") %>% dplyr::pull(Train)), as.vector(sapply(sapply(eunomiaBenchmarkDesign_toSample2, "[[", "sampleSettings"), "[[", "numberTrainSetOutcomes")))
  expect_equal(as.double(performance$performanceMetrics %>% dplyr::filter(metric == "outcomeCount") %>% dplyr::pull(CV)), as.vector(sapply(sapply(eunomiaBenchmarkDesign_toSample2, "[[", "sampleSettings"), "[[", "numberTrainSetOutcomes")))
  expect_equal(as.double(performance$performanceMetrics %>% dplyr::filter(metric == "outcomeCount") %>% dplyr::pull(CV)), c(90, 90))
})
