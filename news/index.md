# Changelog

## PLPBenchmarks 2.2.0

- Github Actions workflows added for package checks and test coverage.
- Updates to the whole test suit to speed up testing.
- Prediction problems are now offered as datasets e.g. call
  `data("tasks")` to load a data frame with the problems.
- Package `DatabaseConnector` now moved to Imports.
- New vignettes added.
- Github Actions workflow added for building the website. The package
  now has a dedicated website.
- Bug fix in argument checks in
  [`createBenchmarkCohorts()`](https://mi-erasmusmc.github.io/PlpBenchmarks/reference/createBenchmarkCohorts.md).
- Fixed how arguments are passed into dplyr functions to limit “Notes”
  in package checks (R CMD check)
- Updated README.md

## PLPBenchmarks 2.1.1

- ExecutionTimes are now exported as part of performance evaluation when
  calling
  [`getBenchmarkModelPerformance()`](https://mi-erasmusmc.github.io/PlpBenchmarks/reference/getBenchmarkModelPerformance.md).
  The times are derived from
  [`PatientLevelPrediction::runPlp()`](https://ohdsi.github.io/PatientLevelPrediction/reference/runPlp.html)
  and refer to the TotalExecutionTime estimated in the `runPlp`
  function.  
- `Andromeda` R package updated to version 1.0.0. This is a major change
  for `Andromeda` as it switches the backend to `duckdb`. Also, packages
  `FeatureExtraction` updated to their latest compatible versions.
- Updates also to the `PatientLevelPrediction` R package to its latest
  version.

## PLPBenchmarks 2.1.0

- Added tests.
- Updated renv files so that now all profiles use
  <PatientLevelPrediction@v6.4.0> from CRAN.
- Solved a bug when incremental folder is defined outside saveDirectory.
- Vignette updates

## PLPBenchmarks 2.0.1

- All function arguments are now being checked for validity.
- New function to create a database and a shiny app to view results.
- Default profile now loads <PatientLevelPrediction@v6.4.0> from CRAN.
- Function
  [`createBenchmarkCohorts()`](https://mi-erasmusmc.github.io/PlpBenchmarks/reference/createBenchmarkCohorts.md)
  now uses entirely the R package `CohortGenerator` to create the
  cohorts.
- Function `loadBenchmarkDesigns()` has been changed to
  [`loadModelDesigns()`](https://mi-erasmusmc.github.io/PlpBenchmarks/reference/loadModelDesigns.md).
- Function
  [`viewBenchmarkSettings()`](https://mi-erasmusmc.github.io/PlpBenchmarks/reference/viewBenchmarkSettings.md)
  can now accept list/lists of `covariateSettings` object/s.
- Cohort definitions in json and sql formats are now linked with their
  corresponding cohortId.
- Updates of problem specification and analysis designs.

## PLPBenchmarks 2.0.0

- Initial release.
