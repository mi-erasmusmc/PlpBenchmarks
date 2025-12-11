Eunomia::createCohorts(connectionDetails = connectionDetails)
extractBenchmarkData(benchmarkDesign = eunomiaBenchmarkDesign_toSample, createStudyPopulation = T)
runBenchmarkDesign(benchmarkDesign = eunomiaBenchmarkDesign_toSample)
performance <- getBenchmarkModelPerformance(benchmarkDesign = eunomiaBenchmarkDesign_toSample)
test_that("sampling works", {
  expect_equal(as.double(performance$performanceMetrics %>% dplyr::filter(metric == "outcomeCount") %>% dplyr::pull(Train)), as.vector(sapply(sapply(eunomiaBenchmarkDesign_toSample, "[[", "sampleSettings"), "[[", "numberTrainSetOutcomes")))
  expect_equal(as.double(performance$performanceMetrics %>% dplyr::filter(metric == "outcomeCount") %>% dplyr::pull(CV)), as.vector(sapply(sapply(eunomiaBenchmarkDesign_toSample, "[[", "sampleSettings"), "[[", "numberTrainSetOutcomes")))
})

Eunomia::createCohorts(connectionDetails = connectionDetails)
extractBenchmarkData(benchmarkDesign = eunomiaBenchmarkDesign_toSample2, createStudyPopulation = T)
runBenchmarkDesign(benchmarkDesign = eunomiaBenchmarkDesign_toSample2)
performance <- getBenchmarkModelPerformance(benchmarkDesign = eunomiaBenchmarkDesign_toSample2)
test_that("sampling works 2", {
  expect_equal(as.double(performance$performanceMetrics %>% dplyr::filter(metric == "outcomeCount") %>% dplyr::pull(Train)), as.vector(sapply(sapply(eunomiaBenchmarkDesign_toSample2, "[[", "sampleSettings"), "[[", "numberTrainSetOutcomes")))
  expect_equal(as.double(performance$performanceMetrics %>% dplyr::filter(metric == "outcomeCount") %>% dplyr::pull(CV)), as.vector(sapply(sapply(eunomiaBenchmarkDesign_toSample2, "[[", "sampleSettings"), "[[", "numberTrainSetOutcomes")))
  expect_equal(as.double(performance$performanceMetrics %>% dplyr::filter(metric == "outcomeCount") %>% dplyr::pull(CV)), c(48, 48))
})
