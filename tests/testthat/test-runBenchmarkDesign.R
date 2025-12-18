library(checkmate)

test_that("runs without population creation", {
  
  suppressWarnings({
    Eunomia::createCohorts(connectionDetails = connectionDetails)
    extractBenchmarkData(benchmarkDesign = eunomiaBenchmarkDesign_3, createStudyPopulation = F)
  })
  
  designNames <- names(eunomiaBenchmarkDesign_3)
  expect_no_error(runBenchmarkDesign(benchmarkDesign = eunomiaBenchmarkDesign_3)) 
  
  suppressWarnings({
    runBenchmarkDesign(benchmarkDesign = eunomiaBenchmarkDesign_3)
  })
  
  expect_equal(as.vector(sapply(eunomiaBenchmarkDesign_3, "[[", "saveDirectory")), file.path(saveDirectory, "eunomia_design_3", designNames))
  expect_directory(x = file.path(saveDirectory, "eunomia_design_3", designNames, "plpResult"))
  expect_file_exists(x = file.path(saveDirectory, "eunomia_design_3", designNames, "plpResult", "runPlp.Rds"))
})

