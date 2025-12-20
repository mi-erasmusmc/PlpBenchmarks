# Create a benchmark analysis design

Create a benchmark analysis design by providing a list of objects each
of class `modelDesign`. The result is a list of class `benchmarkDesign`
that holds the necessary information to run each problem. Note that, for
problems that share the same target and outcome cohorts, and/or same
population settings the design specifies which of them are unique to
avoid duplicating tasks. The directories where each plpData or study
population object is to be saved are found in the attributes of the
returned object.

## Usage

``` r
createBenchmarkDesign(
  modelDesign = NULL,
  databaseDetails = NULL,
  rawDataFolder = "rawData",
  saveDirectory = getwd()
)
```

## Arguments

- modelDesign:

  A named list of model designs. Each object of the list should be
  another list of class `modelDesign` created using
  [`PatientLevelPrediction::createModelDesign()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createModelDesign.html)

- databaseDetails:

  An object of class databaseDetails created using
  [`PatientLevelPrediction::createDatabaseDetails()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createDatabaseDetails.html).

- rawDataFolder:

  A folder to save the rawData objects i.e. plpData, population,
  cohortCounts, records of cohorts create. If a folder is provided, the
  saved object will be create in
  `file.path(saveDirectory, rawDataFolder)`.

- saveDirectory:

  The directory to save the benchmark models.
