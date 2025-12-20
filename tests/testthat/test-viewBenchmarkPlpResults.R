library(checkmate)

test_that("viewBenchmarkResults works", {
  
  suppressWarnings({
    extractBenchmarkData(benchmarkDesign = eunomiaBenchmarkDesign_1)
    runBenchmarkDesign(benchmarkDesign = eunomiaBenchmarkDesign_1)
  })
  
 expect_no_error(viewBenchmarkResults(benchmarkDesign = eunomiaBenchmarkDesign_1, databaseList = list("Eunomia"), databaseDirectory = file.path(saveDirectory, "eunomia_design_1"), viewShiny = F))
  expect_directory(file.path(saveDirectory, "eunomia_design_1", "sqlite"))
  expect_file(file.path(saveDirectory, "eunomia_design_1", "sqlite", "databaseFile.sqlite"))
  expect_error(viewBenchmarkResults(eunomiaDesigns[1], databaseDirectory = file.path(saveDirectory, "eunomia_design_1")))
  expect_error(viewBenchmarkResults(benchmarkDesign))
})
