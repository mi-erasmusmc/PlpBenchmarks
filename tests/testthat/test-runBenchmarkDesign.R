library(checkmate)

test_that("runs without population creation", {
  
  suppressWarnings({
    Eunomia::createCohorts(connectionDetails = connectionDetails)
    extractBenchmarkData(benchmarkDesign = eunomiaBenchmarkDesign_1, createStudyPopulation = F)
  })
  
  designNames <- names(eunomiaBenchmarkDesign_1)
  expect_no_error(runBenchmarkDesign(benchmarkDesign = eunomiaBenchmarkDesign_1)) 
  expect_equal(as.vector(sapply(eunomiaBenchmarkDesign_1, "[[", "saveDirectory")), file.path(saveDirectory, "eunomia_design_1", designNames))
  expect_directory(x = file.path(saveDirectory, "eunomia_design_1", designNames, "plpResult"))
  expect_file(x = file.path(saveDirectory, "eunomia_design_1", designNames, "plpResult", "runPlp.Rds"))
})

