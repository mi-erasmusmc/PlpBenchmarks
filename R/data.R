#' @importFrom tibble tibble
NULL

#' tasks
#'
#' Task Specification for Real-World Data
#'
#' @format A data frame with 13 variables:
#' \describe{
#' \item{\code{pId}}{Problem Id}
#' \item{\code{analysisName}}{Name given for this analysis}
#' \item{\code{problemSpecification}}{Problem specification}
#' \item{\code{TAR_start_day}}{Starting day for time-at-risk}
#' \item{\code{TAR_end_day}}{Ending day for time-at-risk}
#' \item{\code{targetId}}{The id of the target cohort as found in `inst/settings/cohortsToCreate.csv`}
#' \item{\code{outcomeId}}{The id of the outcome cohort as found in `inst/settings/cohortsToCreate.csv`}
#' \item{\code{targetCohortName}}{The name of the target cohort as found in `inst/settings/cohortsToCreate.csv`}
#' \item{\code{outcomeCohortName}}{The name of the outcome cohort as found in `inst/settings/cohortsToCreate.csv`}
#' \item{\code{targetCohortJson}}{The name of the json file containing the target cohort definition}
#' \item{\code{outcomeCohortJson}}{The name of the json file containing the outcome cohort definition}
#' }
#'
"tasks"

#' modelDesigns
#' 
#' Patient-Level Prediction Model Designs for Real-World Data 
#' 
#' A prespecified list containing model designs with class `modelDesign` as generetated by `PatientLevelPrediction::createModelDesign`
#' See \link[PatientLevelPrediction]{createModelDesign} for more details and \url{https://ohdsi.github.io/PatientLevelPrediction/articles/BuildingMultiplePredictiveModels.html#creating-a-model-design} on examples how to create a model design.
"modelDesigns"

#' eunomiaTasks
#'
#' Task Specification for the Eunomia dataset
#'
#' @format A data frame with 7 variables:
#' \describe{
#' \item{\code{pId}}{Problem Id}
#' \item{\code{analysisName}}{Name given for this analysis}
#' \item{\code{problemSpecification}}{Problem specification}
#' \item{\code{TAR_start_day}}{Starting day for time-at-risk}
#' \item{\code{TAR_end_day}}{Ending day for time-at-risk}
#' \item{\code{targetId}}{The id of the target cohort as found in `inst/settings/cohortsToCreate.csv`}
#' \item{\code{outcomeId}}{The id of the outcome cohort as found in `inst/settings/cohortsToCreate.csv`}
#' }
#'
#' To create the cohorts for the Eunomia dataset we can call \link[Eunomia]{createCohorts} 
#' 
"eunomiaTasks"

#' eunomiaDesigns
#' 
#' Patient-Level Prediction Model Designs for the Eunomia dataset 
#' 
#' A prespecified list containing model designs with class `modelDesign` as generetated by `PatientLevelPrediction::createModelDesign`
#' See \link[PatientLevelPrediction]{createModelDesign} for more details and \url{https://ohdsi.github.io/PatientLevelPrediction/articles/BuildingMultiplePredictiveModels.html#creating-a-model-design} on examples how to create a model design.
"eunomiaDesigns"