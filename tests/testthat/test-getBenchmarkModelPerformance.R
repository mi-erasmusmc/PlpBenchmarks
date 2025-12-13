library(checkmate)

runBenchmarkDesign(benchmarkDesign = eunomiaBenchmarkDesign_2)
res <- getBenchmarkModelPerformance(eunomiaBenchmarkDesign_2)

test_that("function works properly", {
  expect_list(res)
  expect_subset(names(res), choices = c("performanceMetrics", "executionTimes"))
  expect_error(getBenchmarkModelPerformance(eunomiaDesigns))
  expect_error(getBenchmarkModelPerformance(benchmarkDesign[[1]]))
})

test_that("plpPerformance collected properly", {
  expect_data_frame(res$performanceMetrics)
  expect_tibble(res$performanceMetrics)
  expect_subset(names(res$performanceMetrics), choices = c("analysisName", "metric", "Test", "Train", "CV"))
})

test_that("executionTimes collected properly", {
  expect_data_frame(res$executionTimes)
  expect_tibble(res$executionTimes)
  expect_subset(names(res$executionTimes), choices = c("analysisName", "TotalExecutionElapsedTime"))
})
