test_that("benchmarkDesign is of class 'benchmarkDesign'", {
  expect_class(benchmarkDesign, "benchmarkDesign")
})

test_that("benchmarkDesign is a nested list of lists with class 'modelDesign'", {
  expect_list(benchmarkDesign, "modelDesign", names = "named")
  expect_named(benchmarkDesign)
})

test_that("benchmarkDesign has the same names a modelDesigns", {
  expect_equal(names(benchmarkDesign), names(modelDesigns))
  expect_equal(length(benchmarkDesign), length(modelDesigns))
})

test_that("benchmarkDesign, tasks and analysis designs have the same targetId and outcomeId", {
  expect_equal(sapply(benchmarkDesign, "[[", "targetId"), sapply(modelDesigns, "[[", "targetId"))
  expect_equal(sapply(benchmarkDesign, "[[", "outcomeId"), sapply(modelDesigns, "[[", "outcomeId"))
  expect_equal(as.vector(sapply(benchmarkDesign, "[[", "targetId")), tasks$targetId)
  expect_equal(as.vector(sapply(benchmarkDesign, "[[", "outcomeId")), tasks$outcomeId)
  
  expect_equal(attributes(benchmarkDesign)$benchmarkSettings$targetId, as.vector(sapply(modelDesigns, "[[", "targetId")))
  expect_equal(attributes(benchmarkDesign)$benchmarkSettings$outcomeId, as.vector(sapply(modelDesigns, "[[", "outcomeId")))
  expect_equal(attributes(benchmarkDesign)$benchmarkSettings$targetId, tasks$targetId)
  expect_equal(attributes(benchmarkDesign)$benchmarkSettings$outcomeId, tasks$outcomeId)
  
  expect_equal(attributes(benchmarkDesign)$benchmarkSettings$targetId, as.vector(sapply(benchmarkDesign, "[[", "targetId")))
  expect_equal(attributes(benchmarkDesign)$benchmarkSettings$outcomeId, as.vector(sapply(benchmarkDesign, "[[", "outcomeId")))
  
  expect_length(benchmarkDesign, length(modelDesigns))
  expect_equal(length(modelDesigns), nrow(attributes(benchmarkDesign)$benchmarkSettings))
  expect_equal(length(benchmarkDesign), nrow(attributes(benchmarkDesign)$benchmarkSettings))
})

test_that("directories are specified correctly", {
  distinctProblemIds <- unique(attributes(benchmarkDesign)$benchmarkSettings$sameTargetAsProblemId)
  expect_equal(unique(attributes(benchmarkDesign)$benchmarkSettings$dataLocation), file.path(saveDirectory, "rwd_designs", "rawData", names(benchmarkDesign[c(distinctProblemIds)]), "plpData"))
  expect_equal(attributes(benchmarkDesign)$benchmarkSettings$populationLocation, file.path(saveDirectory, "rwd_designs", "rawData", names(benchmarkDesign), "studyPopulation"))
})

test_that("directories are specified correctly (2)", {
  distinctProblemIds <- unique(attributes(benchmarkDesign)$benchmarkSettings$sameTargetAsProblemId)
  expect_equal(attributes(createBenchmarkDesign(modelDesign = modelDesigns, databaseDetails = databaseDetails, rawDataFolder = "~/some/other/Dir", saveDirectory = "test"))$uniquePlpData$dataLocation, file.path("~/some/other/Dir", names(benchmarkDesign[c(distinctProblemIds)]), "plpData"))
  expect_equal(attributes(createBenchmarkDesign(modelDesign = modelDesigns, databaseDetails = databaseDetails, rawDataFolder = "~/some/other/Dir", saveDirectory = "test"))$uniquePopulation$populationLocation, file.path("~/some/other/Dir", names(benchmarkDesign), "studyPopulation"))
})

modelDesignObject <- modelDesigns[[1]]
modelDesignList <- list(modelDesignObject)

test_that("it does not accept an unnamed list", {
  expect_error(createBenchmarkDesign(modelDesignObject, databaseDetails = databaseDetails, saveDirectory = "test"))
  expect_error(createBenchmarkDesign(modelDesignList, databaseDetails = databaseDetails, saveDirectory = "test"))
})

names(modelDesignList) <- "name1"
test_that("it does accept a named list", {
  expect_no_error(createBenchmarkDesign(modelDesignList,  databaseDetails = databaseDetails, saveDirectory = "test"))
})

test_that("it does accept a named list", {
  expect_class(createBenchmarkDesign(modelDesignList,  databaseDetails = databaseDetails, saveDirectory = "test"), "benchmarkDesign")
  expect_type(createBenchmarkDesign(modelDesignList,  databaseDetails = databaseDetails, saveDirectory = "test"), "list")
})

