extractBenchmarkData(benchmarkDesign = eunomiaBenchmarkDesign_1, createStudyPopulation = F)
test_that("runs without population creation", {
  designNames <- names(eunomiaBenchmarkDesign_1)
  expect_no_error(runBenchmarkDesign(benchmarkDesign = eunomiaBenchmarkDesign_1)) 
  expect_equal(as.vector(sapply(eunomiaBenchmarkDesign_1, "[[", "saveDirectory")), file.path(saveDirectory, "eunomia_design_1", designNames))
  expect_directory(x = file.path(saveDirectory, "eunomia_design_1", designNames, "plpResult"))
  expect_file(x = file.path(saveDirectory, "eunomia_design_1", designNames, "plpResult", "runPlp.Rds"))
})

