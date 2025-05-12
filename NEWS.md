# PLPBenchmarks 2.1.1
  
- ExecutionTimes are now exported as part of performance evaluation when calling `getBenchmarkModelPerformance()`. The times are 
derived from `PatientLevelPrediction::runPlp()` and refer to the TotalExecutionTime estimated in the `runPlp` function.  
- `Andromeda` R package updated to version 1.0.0. This is a major change for `Andromeda` as it switches the backend to `duckdb`. Also, 
packages `FeatureExtraction` updated to their latest compatible versions. 
- Updates also to the `PatientLevelPrediction` R package to its latest version.  

# PLPBenchmarks 2.1.0
  
- Added tests.
- Updated renv files so that now all profiles use PatientLevelPrediction@v6.4.0 from CRAN.
- Solved a bug when incremental folder is defined outside saveDirectory.
- Vignette updates
  
# PLPBenchmarks 2.0.1

- All function arguments are now being checked for validity.
- New function to create a database and a shiny app to view results.
- Default profile now loads PatientLevelPrediction@v6.4.0 from CRAN.
- Function `createBenchmarkCohorts()` now uses entirely the R package `CohortGenerator` to create the cohorts. 
- Function `loadBenchmarkDesigns()` has been changed to `loadModelDesigns()`.
- Function `viewBenchmarkSettings()` can now accept list/lists of `covariateSettings` object/s. 
- Cohort definitions in json and sql formats are now linked with their corresponding cohortId.
- Updates of problem specification and analysis designs. 

# PLPBenchmarks 2.0.0

* Initial release.
