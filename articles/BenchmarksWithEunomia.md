# Using the PLPBenchmark package with Eunomia

The following vignette explains how to use the `PLPBenchmarks` package
using the Eunomia dataset. We have created hypothetical benchmark
problems using the available cohorts in Eunomia and these exist as saved
datasets in the package. A predefined set of model design for the
Eunomia benchmark problems also exists as datasets. See the exampl below
of how to call these datasets.

## Running the Eunomia benchmark problems

Let’s start by loading the required packages:

``` r
library(Eunomia)
library(PLPBenchmarks)
#> Loading required package: PatientLevelPrediction
```

Next we need to connect to a database. Since we are using Eunomia, we
just need to define the following in order to define our
connectionDetails object:

``` r
connectionDetails <- getEunomiaConnectionDetails()
```

Some other variables we need to define apriori:

``` r
saveDirectory = "exampleVignette"
seed = 42
cdmDatabaseSchema = "main"
cdmDatabaseName = "Eunomia"
cdmDatabaseId = "Eunomia"
cohortDatabaseSchema = "main"
outcomeDatabaseSchema = "main"
cohortTable = "cohort"
```

Let’s load the predefined Eunomia prediction problems that we will use
as benchmarks.

``` r
data("eunomiaTasks")
class(eunomiaTasks)
```

We can have an overview of the pre-specified problems for Eunomia.

``` r
eunomiaTasks$problemSpecification
```

Let’s load the benchmark design for the Eunomia prediction problems.

``` r
data("eunomiaDesigns")
class(eunomiaDesigns)
sapply(eunomiaDesigns, class)
```

As we can see, the `eunomiaDesign` object is just a list which contains
objects of class `modelDesigns` created by
[`PatientLevelPrediction::createModelDesign()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createModelDesign.html).

Let’s continue by creating the cohorts we will work with.

``` r
Eunomia::createCohorts(connectionDetails = connectionDetails)
```

We need to define our database details in true PatientLevelPrediction
fashion to be able to develop the models.

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

Now that we have all the necessary components in place, let us create a
benchmark design. A benchmark design, is an object of class
`benchmarkDesign`, created using
[`createBenchmarkDesign()`](https://mi-erasmusmc.github.io/PlpBenchmarks/reference/createBenchmarkDesign.md).
It holds the same information as a `modelDesign` object and adds some
necessary components to run each problem: targetId, outcomeId,
saveDirectory, analysisName, and the databaseDetails object.

``` r
benchmarkDesign <- createBenchmarkDesign(modelDesign = eunomiaDesigns, 
                                         databaseDetails = databaseDetails,
                                         saveDirectory = saveDirectory)
class(benchmarkDesign)
```

Having set up our benchmark design, we can use that to extract the data.
We can do that by simply calling,

``` r
PLPBenchmarks::extractBenchmarkData(benchmarkDesign = benchmarkDesign, createStudyPopulation = TRUE)
```

The `extractBenhcmarkData()` function calls
[`PatientLevelPrediction::getPlpData()`](https://ohdsi.github.io/PatientLevelPrediction/reference/getPlpData.html)
and has an additional option to build the study population as well. We
have introduced this feature as sometimes, one wants to pre-build the
study population prior to running
[`PatientLevelPrediction::runPlp()`](https://ohdsi.github.io/PatientLevelPrediction/reference/runPlp.html).

Finally we can run our model using

``` r
PLPBenchmarks::runBenchmarkDesign(benchmarkDesign = benchmarkDesign)
```

We can collect the results in a dataframe,

``` r
results <- PLPBenchmarks::getBenchmarkModelPerformance(benchmarkDesign = benchmarkDesign)
```

``` r
head(results$performanceMetrics)
# or for the execution times
results$executionTimes
```

or we can view them in a shiny app:

``` r
PLPBenchmarks::viewBenchmarkResults(benchmarkDesign = benchmarkDesign, databaseList = list("Eunomia"), viewShiny = FALSE, databaseDirectory = saveDirectory)
```

Tip: If we want to view the Shiny app again, we can make use of
[`PatientLevelPrediction::viewDatabaseResultPlp`](https://ohdsi.github.io/PatientLevelPrediction/reference/viewDatabaseResultPlp.html)
as follows:

``` r
PatientLevelPrediction::viewDatabaseResultPlp(
      mySchema = "main",
      myServer = file.path(saveDirectory, "sqlite", "databaseFile.sqlite"),
      myUser = NULL,
      myPassword = NULL,
      myDbms = "sqlite",
      myPort = NULL,
      myTableAppend = ""
    )
```

## Example: Running a benchmark to evaluate performance of under sampling

For running a selection of problems, select which ones from the list
model designs from the design list. Suppose for this example we want to
run the first problem of the Eunomia benchmarks which is specified as
Predicting gastrointestinal bleeding in new users of celecoxib within
one year of initiating tretment. . In addition, for the sake of the
example, suppose we would like to test the impact of under sampling on
the predictive performance of the model generated to predict the outcome
of interest.  
In the same fashion as before, we build a list of `modelDesign` objects.
For our example, we copy the design for the first problem, we just edit
the settings we require to change and the names of the analysis.

``` r
selectedDesignList <- eunomiaDesigns[c(1, 1)]
names(selectedDesignList) <- c("GIBinCLXB", "GIBinCLXBSampled")
names(selectedDesignList)

selectedDesignList$GIBinCLXBSampled$sampleSettings
selectedDesignList$GIBinCLXBSampled$sampleSettings <- PatientLevelPrediction::createSampleSettings(type = "underSample", numberOutcomestoNonOutcomes = 1, sampleSeed = 1012L)
selectedDesignList$GIBinCLXBSampled$executeSettings <- PatientLevelPrediction::createExecuteSettings(runSampleData = TRUE, runModelDevelopment = T, runCovariateSummary = T, runSplitData = T, runFeatureEngineering = F, runPreprocessData = T)
```

Let’s create our benchmark design:

``` r
selectedSampleBenchmark <- createBenchmarkDesign(modelDesign = selectedDesignList, 
                                                 databaseDetails = databaseDetails, 
                                                 saveDirectory = "testSelected")
```

Extract the data.

``` r
extractBenchmarkData(benchmarkDesign = selectedSampleBenchmark)
```

Finally, let’s run our models.

``` r
runBenchmarkDesign(benchmarkDesign = selectedSampleBenchmark)
```
