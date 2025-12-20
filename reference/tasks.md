# tasks

Task Specification for Real-World Data

## Usage

``` r
tasks
```

## Format

A data frame with 13 variables:

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

- `targetCohortName`:

  The name of the target cohort as found in
  \`inst/settings/cohortsToCreate.csv\`

- `outcomeCohortName`:

  The name of the outcome cohort as found in
  \`inst/settings/cohortsToCreate.csv\`

- `targetCohortJson`:

  The name of the json file containing the target cohort definition

- `outcomeCohortJson`:

  The name of the json file containing the outcome cohort definition
