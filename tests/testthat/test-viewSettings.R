test_that("view works", {
  expect_no_error(viewBenchmarkSettings(benchmarkDesign = eunomiaBenchmarkDesign_1))
  expect_data_frame(viewBenchmarkSettings(benchmarkDesign = eunomiaBenchmarkDesign_1))
  expect_equal(ncol(viewBenchmarkSettings(benchmarkDesign = eunomiaBenchmarkDesign_1)), length(eunomiaBenchmarkDesign_1)+2)
})
