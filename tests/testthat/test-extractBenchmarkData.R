# Eunomia::createCohorts(connectionDetails = connectionDetails)
# extractBenchmarkData(benchmarkDesign = eunomiaBenchmarkDesign, createStudyPopulation = FALSE)
test_that("unique plp data objects are created", {
  
  skip_if_not_installed("Eunomia")
  
  suppressWarnings({
    Eunomia::createCohorts(connectionDetails = connectionDetails)
    extractBenchmarkData(benchmarkDesign = eunomiaBenchmarkDesign, createStudyPopulation = FALSE)
  })
  expect_equal(length(attributes(eunomiaBenchmarkDesign)$uniquePlpData$dataLocation), length(list.files(file.path(saveDirectory, "eunomia_designs", "rawData"))))
})

extractBenchmarkData(benchmarkDesign = eunomiaBenchmarkDesign4, createStudyPopulation = TRUE)
test_that("unique populations are created", {
  
  skip_if_not_installed("Eunomia")
  
  suppressWarnings({
    Eunomia::createCohorts(connectionDetails = connectionDetails)
    extractBenchmarkData(benchmarkDesign = eunomiaBenchmarkDesign4, createStudyPopulation = TRUE)
  })
  
  designNames <- names(eunomiaBenchmarkDesign4)
  expect_equal(attributes(eunomiaBenchmarkDesign4)$uniquePopulation$populationLocation, file.path(saveDirectory, "eunomia_designs_4", "rawData", designNames, "studyPopulation") )
})

