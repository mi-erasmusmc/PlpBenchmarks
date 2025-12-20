# Case Study: Benchmarking Algorithms using Eunomia

For this example, we are going to run a small experiment that compares
the performance of the XGBoost algorithm to the performance of
L1-penalised logistic regression. We are going to make use of the
Eunomia dataset.

## Setup

We start by loading the packages that we will use.

``` r
library(Eunomia)
library(PLPBenchmarks)
#> Loading required package: PatientLevelPrediction
library(xgboost)
```

Define the connectionDetails object for Eunomia:

``` r
connectionDetails <- getEunomiaConnectionDetails()
```

Some other variables we need to define apriori:

``` r
saveDirectory = "comparisonsVignette"
seed = 42
cdmDatabaseSchema = "main"
cdmDatabaseName = "Eunomia"
cdmDatabaseId = "Eunomia"
cohortDatabaseSchema = "main"
outcomeDatabaseSchema = "main"
cohortTable = "cohort"
```

We can have an overview of the pre-specified problems for Eunomia. We
are going to compare the two algorithms on the following problem:

``` r
data("eunomiaTasks")
eunomiaTasks$problemSpecification[1]
```

Let’s load the benchmark design for the Eunomia prediction problems.

``` r
data("eunomiaDesigns")
```

Let’s continue by creating the cohorts we will work with.

``` r
Eunomia::createCohorts(connectionDetails = connectionDetails)
```

Define our database details.

``` r
databaseDetails <- PatientLevelPrediction::createDatabaseDetails(connectionDetails = connectionDetails, 
                                                                 cdmDatabaseSchema = cdmDatabaseSchema,
                                                                 cdmDatabaseName = cdmDatabaseName,
                                                                 cdmDatabaseId = cdmDatabaseId, 
                                                                 cohortDatabaseSchema = cohortDatabaseSchema,
                                                                 cohortTable = cohortTable,
                                                                 outcomeDatabaseSchema = outcomeDatabaseSchema,
                                                                 outcomeTable = cohortTable, 
                                                                 )
```

## Specifying our benchmark

We are going to set up our algorithm settings,

``` r
lassoSettings <- PatientLevelPrediction::setLassoLogisticRegression(seed = seed)

xgbSettings <- PatientLevelPrediction::setGradientBoostingMachine(seed = seed)
```

and pass them to our model designs:

``` r
selectedDesignList <- eunomiaDesigns[c(1, 1)]
names(selectedDesignList) <- c("GIBinCLXB_lasso", "GIBinCLXB_xgb")
names(selectedDesignList)
#> [1] "GIBinCLXB_lasso" "GIBinCLXB_xgb"
```

The `GIBinCLXB_lasso` model design already is using LASSO, to check:

``` r
attr(selectedDesignList$GIBinCLXB_lasso$modelSettings$param, "settings")$name
#> [1] "Lasso Logistic Regression"
```

We need to change the `modelSettings` for the second design that will
execute the XGBoost algorithm.

``` r
selectedDesignList$GIBinCLXB_xgb$modelSettings <- xgbSettings
# Just to verify the algorithm has indeed changed:
attr(selectedDesignList$GIBinCLXB_xgb$modelSettings$param, "settings")$modelName
#> [1] "Gradient Boosting Machine"
```

Let’s create our benchmark design:

``` r
comparisonBenchmark <- createBenchmarkDesign(modelDesign = selectedDesignList, 
                                                 databaseDetails = databaseDetails, 
                                                 saveDirectory = saveDirectory)
```

We can now have a view at our settings for the benchmark

``` r
viewBenchmarkSettings(benchmarkDesign = comparisonBenchmark) %>%
  knitr::kable() %>%
  kableExtra::kable_paper(lightable_options = "striped") %>%
  kableExtra::scroll_box(width = "100%", height = "200px")
```

| settings           | option                         | GIBinCLXB_lasso                                             | GIBinCLXB_xgb                                               |
|:-------------------|:-------------------------------|:------------------------------------------------------------|:------------------------------------------------------------|
| benchmarkSettings  | analysisId                     | GIBinCLXB_lasso                                             | GIBinCLXB_xgb                                               |
| benchmarkSettings  | problemId                      | 1                                                           | 2                                                           |
| benchmarkSettings  | targetId                       | 1                                                           | 1                                                           |
| benchmarkSettings  | outcomeId                      | 3                                                           | 3                                                           |
| benchmarkSettings  | sameTargetAsProblemId          | 1                                                           | 1                                                           |
| benchmarkSettings  | plpDataName                    | GIBinCLXB_lasso                                             | GIBinCLXB_lasso                                             |
| benchmarkSettings  | populationLocation             | comparisonsVignette/rawData/GIBinCLXB_lasso/studyPopulation | comparisonsVignette/rawData/GIBinCLXB_lasso/studyPopulation |
| benchmarkSettings  | dataLocation                   | comparisonsVignette/rawData/GIBinCLXB_lasso/plpData         | comparisonsVignette/rawData/GIBinCLXB_lasso/plpData         |
| populationSettings | binary                         | TRUE                                                        | TRUE                                                        |
| populationSettings | includeAllOutcomes             | FALSE                                                       | FALSE                                                       |
| populationSettings | firstExposureOnly              | TRUE                                                        | TRUE                                                        |
| populationSettings | washoutPeriod                  | 0                                                           | 0                                                           |
| populationSettings | removeSubjectsWithPriorOutcome | TRUE                                                        | TRUE                                                        |
| populationSettings | priorOutcomeLookback           | 99999                                                       | 99999                                                       |
| populationSettings | requireTimeAtRisk              | TRUE                                                        | TRUE                                                        |
| populationSettings | minTimeAtRisk                  | 1                                                           | 1                                                           |
| populationSettings | riskWindowStart                | 1                                                           | 1                                                           |
| populationSettings | startAnchor                    | cohort start                                                | cohort start                                                |
| populationSettings | riskWindowEnd                  | 365                                                         | 365                                                         |
| populationSettings | endAnchor                      | cohort start                                                | cohort start                                                |
| populationSettings | restrictTarToCohortEnd         | FALSE                                                       | FALSE                                                       |
| covariateSettings  | temporal                       | FALSE                                                       | FALSE                                                       |
| covariateSettings  | temporalSequence               | FALSE                                                       | FALSE                                                       |
| covariateSettings  | DemographicsGender             | TRUE                                                        | TRUE                                                        |
| covariateSettings  | DemographicsAge                | TRUE                                                        | TRUE                                                        |
| covariateSettings  | ConditionOccurrenceLongTerm    | TRUE                                                        | TRUE                                                        |
| covariateSettings  | DrugGroupEraLongTerm           | TRUE                                                        | TRUE                                                        |
| covariateSettings  | longTermStartDays              | -365                                                        | -365                                                        |
| covariateSettings  | mediumTermStartDays            | -180                                                        | -180                                                        |
| covariateSettings  | shortTermStartDays             | -30                                                         | -30                                                         |
| covariateSettings  | endDays                        | -1                                                          | -1                                                          |
| covariateSettings  | addDescendantsToInclude        | FALSE                                                       | FALSE                                                       |
| covariateSettings  | addDescendantsToExclude        | FALSE                                                       | FALSE                                                       |
| modelSettings      | modelName                      | Lasso Logistic Regression                                   | Gradient Boosting Machine                                   |
| splitSettings      | test                           | 0.25                                                        | 0.25                                                        |
| splitSettings      | train                          | 0.75                                                        | 0.75                                                        |
| splitSettings      | seed                           | 123                                                         | 123                                                         |
| splitSettings      | nfold                          | 3                                                           | 3                                                           |
| preprocessSettings | minFraction                    | 0.001                                                       | 0.001                                                       |
| preprocessSettings | normalize                      | TRUE                                                        | TRUE                                                        |
| preprocessSettings | removeRedundancy               | TRUE                                                        | TRUE                                                        |
| sampleSettings     | fun                            | sameData                                                    | sameData                                                    |
| sampleSettings     | numberOutcomestoNonOutcomes    | 1                                                           | 1                                                           |
| sampleSettings     | sampleSeed                     | 1                                                           | 1                                                           |
| executeSettings    | runSplitData                   | TRUE                                                        | TRUE                                                        |
| executeSettings    | runSampleData                  | FALSE                                                       | FALSE                                                       |
| executeSettings    | runFeatureEngineering          | FALSE                                                       | FALSE                                                       |
| executeSettings    | runPreprocessData              | TRUE                                                        | TRUE                                                        |
| executeSettings    | runModelDevelopment            | TRUE                                                        | TRUE                                                        |
| executeSettings    | runCovariateSummary            | TRUE                                                        | TRUE                                                        |

As we see, all settings between the two designs are equal, except the
algorithm to be used, as we wanted.

## Extracting the data and running the benchmark design

Now let’s extract the data.

``` r
extractBenchmarkData(benchmarkDesign = comparisonBenchmark)
```

Finally, let’s run our benchmark.

``` r
runBenchmarkDesign(benchmarkDesign = comparisonBenchmark)
```

## Inspecting results

And a let’s have a look at some of the results.

``` r
results <- PLPBenchmarks::getBenchmarkModelPerformance(benchmarkDesign = comparisonBenchmark)
results$performanceMetrics %>%
  dplyr::filter(metric %in% c("AUROC", "AUPRC", "calibrationInLarge mean prediction"))
```
