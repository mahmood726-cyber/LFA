#' Example Meta-Analysis Dataset
#'
#' A dataset containing effect sizes from 10 hypothetical studies examining
#' the effect of an intervention on a continuous outcome.
#'
#' @format A data frame with 10 rows and 7 variables:
#' \describe{
#'   \item{study}{Study identifier (author and year)}
#'   \item{effect}{Effect size estimate (standardized mean difference)}
#'   \item{se}{Standard error of the effect size}
#'   \item{year}{Publication year}
#'   \item{sample_size}{Total sample size}
#'   \item{quality}{Study quality score (1-5 scale)}
#'   \item{region}{Geographic region (North America, Europe, Asia)}
#' }
#'
#' @examples
#' data(example_meta)
#' result <- cbamm_fast(example_meta)
#' print(result)
"example_meta"

#' Example Dataset for Transport Weights
#'
#' A dataset containing effect sizes and population characteristics from
#' 8 studies, useful for demonstrating transport weight calculations.
#'
#' @format A data frame with 8 rows and 7 variables:
#' \describe{
#'   \item{study}{Study identifier}
#'   \item{effect}{Effect size estimate}
#'   \item{se}{Standard error}
#'   \item{age_mean}{Mean age of participants}
#'   \item{female_pct}{Percentage of female participants (0-1)}
#'   \item{bmi_mean}{Mean body mass index}
#'   \item{diabetes_pct}{Percentage with diabetes (0-1)}
#' }
#'
#' @examples
#' data(example_transport)
#' target_pop <- list(age_mean = 60, female_pct = 0.50, bmi_mean = 28)
#' weights <- compute_transport_weights(example_transport, target_pop)
"example_transport"

#' Example Diagnostic Test Accuracy Dataset
#'
#' A dataset containing diagnostic test accuracy data from 6 studies,
#' including 2x2 contingency table data and calculated accuracy measures.
#'
#' @format A data frame with 6 rows and 11 variables:
#' \describe{
#'   \item{study}{Study identifier}
#'   \item{tp}{True positives}
#'   \item{fp}{False positives}
#'   \item{fn}{False negatives}
#'   \item{tn}{True negatives}
#'   \item{sensitivity}{Sensitivity (true positive rate)}
#'   \item{specificity}{Specificity (true negative rate)}
#'   \item{setting}{Study setting (Hospital or Clinic)}
#'   \item{threshold}{Diagnostic threshold used}
#'   \item{dor}{Diagnostic odds ratio}
#'   \item{log_dor}{Natural log of diagnostic odds ratio}
#'   \item{se_log_dor}{Standard error of log(DOR)}
#' }
#'
#' @examples
#' data(example_dta)
#' # Prepare for meta-analysis of log diagnostic odds ratios
#' dta_ma <- data.frame(
#'   study = example_dta$study,
#'   effect = example_dta$log_dor,
#'   se = example_dta$se_log_dor
#' )
#' result <- cbamm_fast(dta_ma)
"example_dta"
