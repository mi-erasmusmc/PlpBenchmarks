# eunomiaTasks

Task Specification for the Eunomia dataset

## Usage

``` r
eunomiaTasks
```

## Format

A data frame with 7 variables:

- `pId`:

  Problem Id

- `analysisName`:

  Name given for this analysis

- `problemSpecification`:

  Problem specification

- `TAR_start_day`:

  Starting day for time-at-risk

- `TAR_end_day`:

  Ending day for time-at-risk

- `targetId`:

  The id of the target cohort as found in
  \`inst/settings/cohortsToCreate.csv\`

- `outcomeId`:

  The id of the outcome cohort as found in
  \`inst/settings/cohortsToCreate.csv\`

To create the cohorts for the Eunomia dataset we can call
[createCohorts](https://ohdsi.github.io/Eunomia/reference/createCohorts.html)
