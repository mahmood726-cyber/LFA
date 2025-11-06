#' Data Validation Functions
#'
#' @name validation
NULL

#' Validate Meta-Analysis Data
#'
#' Validates that data has required columns and proper format for meta-analysis
#'
#' @param data Data frame to validate
#' @param required_cols Character vector of required column names
#' @param check_numeric Logical indicating whether to check numeric columns
#'
#' @return List with validation results:
#'   \itemize{
#'     \item valid: Logical indicating if data is valid
#'     \item errors: Character vector of error messages
#'     \item warnings: Character vector of warning messages
#'   }
#'
#' @export
validate_cbamm_data <- function(data,
                                  required_cols = c("study", "effect", "se"),
                                  check_numeric = TRUE) {
  errors <- character(0)
  warnings <- character(0)

  # Check if data is a data frame
  if (!is.data.frame(data)) {
    errors <- c(errors, "Input must be a data frame")
    return(list(valid = FALSE, errors = errors, warnings = warnings))
  }

  # Check for required columns
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    errors <- c(errors, sprintf("Missing required columns: %s",
                                paste(missing_cols, collapse = ", ")))
  }

  # Check for empty data
  if (nrow(data) == 0) {
    errors <- c(errors, "Data frame is empty")
  }

  # Check for minimum number of studies
  if (nrow(data) < 2) {
    warnings <- c(warnings, "Less than 2 studies; meta-analysis may not be meaningful")
  }

  # Check numeric columns if requested
  if (check_numeric) {
    numeric_cols <- c("effect", "se", "var")
    existing_numeric_cols <- intersect(numeric_cols, names(data))

    for (col in existing_numeric_cols) {
      if (!is.numeric(data[[col]])) {
        errors <- c(errors, sprintf("Column '%s' must be numeric", col))
      } else {
        # Check for NA values
        if (any(is.na(data[[col]]))) {
          warnings <- c(warnings, sprintf("Column '%s' contains NA values", col))
        }

        # Check for negative standard errors or variances
        if (col %in% c("se", "var")) {
          if (any(data[[col]] <= 0, na.rm = TRUE)) {
            errors <- c(errors, sprintf("Column '%s' contains non-positive values", col))
          }
        }

        # Check for infinite values
        if (any(is.infinite(data[[col]]))) {
          errors <- c(errors, sprintf("Column '%s' contains infinite values", col))
        }
      }
    }
  }

  # Check for duplicate study names
  if ("study" %in% names(data)) {
    if (any(duplicated(data$study))) {
      warnings <- c(warnings, "Duplicate study names detected")
    }
  }

  valid <- length(errors) == 0

  return(list(
    valid = valid,
    errors = errors,
    warnings = warnings
  ))
}


#' Standardize Meta-Analysis Data
#'
#' Standardizes data by ensuring proper column names and computing missing values
#'
#' @param data Data frame to standardize
#' @param effect_col Name of effect size column (default: "effect")
#' @param se_col Name of standard error column (default: "se")
#' @param study_col Name of study identifier column (default: "study")
#'
#' @return Standardized data frame with columns: study, effect, se, var
#' @export
standardize_cbamm_data <- function(data,
                                     effect_col = "effect",
                                     se_col = "se",
                                     study_col = "study") {
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  # Create a copy
  std_data <- data

  # Rename columns if necessary
  col_mapping <- c(study = study_col, effect = effect_col, se = se_col)

  for (new_name in names(col_mapping)) {
    old_name <- col_mapping[new_name]
    if (old_name != new_name && old_name %in% names(std_data)) {
      names(std_data)[names(std_data) == old_name] <- new_name
    }
  }

  # Add study names if missing
  if (!"study" %in% names(std_data)) {
    std_data$study <- paste0("Study", seq_len(nrow(std_data)))
  }

  # Compute variance if missing
  if (!"var" %in% names(std_data) && "se" %in% names(std_data)) {
    std_data$var <- std_data$se^2
  }

  # Compute SE if missing but variance present
  if (!"se" %in% names(std_data) && "var" %in% names(std_data)) {
    std_data$se <- sqrt(std_data$var)
  }

  return(std_data)
}
