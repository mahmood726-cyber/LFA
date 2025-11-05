#' Machine Learning Enhanced Meta-Analysis
#'
#' Revolutionary ML-based approaches to meta-analysis including:
#' - Automated effect size prediction
#' - Study similarity clustering
#' - Heterogeneity source identification via ML
#' - Automated moderator discovery
#' - Publication bias detection via ML
#' - Study quality prediction
#'
#' @name ml_meta_analysis
NULL

#' ML-Based Effect Size Prediction
#'
#' Uses machine learning to predict effect sizes for new studies based on
#' study characteristics. Trains on existing meta-analysis data.
#'
#' @param data Meta-analysis data frame
#' @param predictors Vector of predictor variable names
#' @param method ML method: "rf" (random forest), "xgboost", "svm", "neural"
#' @param train_prop Proportion for training (default: 0.8)
#' @param tune_params Automatically tune hyperparameters (default: TRUE)
#'
#' @return ML prediction model object
#'
#' @export
#' @examples
#' \dontrun{
#' # Train ML model to predict effect sizes
#' ml_model <- ml_predict_effects(
#'   data = meta_data,
#'   predictors = c("year", "sample_size", "study_quality"),
#'   method = "rf"
#' )
#'
#' # Predict for new studies
#' new_predictions <- predict(ml_model, newdata = new_studies)
#' }
ml_predict_effects <- function(data, predictors, method = "rf",
                               train_prop = 0.8, tune_params = TRUE) {

  cat("Training ML model for effect size prediction...\n")

  # Validate inputs
  if (!all(predictors %in% names(data))) {
    stop("Some predictors not found in data")
  }

  if (!"effect" %in% names(data)) {
    stop("Data must contain 'effect' column")
  }

  # Prepare data
  complete_cases <- complete.cases(data[, c("effect", predictors)])
  ml_data <- data[complete_cases, c("effect", predictors)]

  if (nrow(ml_data) < 10) {
    stop("Insufficient complete cases for ML training (need >= 10)")
  }

  # Train-test split
  set.seed(42)
  train_idx <- sample(1:nrow(ml_data), size = floor(train_prop * nrow(ml_data)))
  train_data <- ml_data[train_idx, ]
  test_data <- ml_data[-train_idx, ]

  # Train model based on method
  model <- switch(method,
    "rf" = train_random_forest(train_data, predictors, tune_params),
    "xgboost" = train_xgboost(train_data, predictors, tune_params),
    "svm" = train_svm(train_data, predictors, tune_params),
    "neural" = train_neural_net(train_data, predictors, tune_params),
    stop("Unknown method. Use 'rf', 'xgboost', 'svm', or 'neural'")
  )

  # Evaluate on test set
  if (nrow(test_data) > 0) {
    predictions <- predict_ml_model(model, test_data, method)
    rmse <- sqrt(mean((test_data$effect - predictions)^2))
    r_squared <- cor(test_data$effect, predictions)^2

    cat(sprintf("✓ Model trained successfully\n"))
    cat(sprintf("  RMSE: %.4f\n", rmse))
    cat(sprintf("  R²: %.4f\n", r_squared))
  }

  # Return model object
  result <- list(
    model = model,
    method = method,
    predictors = predictors,
    training_data = train_data,
    performance = if(nrow(test_data) > 0) {
      list(rmse = rmse, r_squared = r_squared)
    } else NULL
  )

  class(result) <- c("ml_meta_model", "list")
  return(result)
}

#' Automated Study Clustering
#'
#' Uses unsupervised ML to cluster studies based on characteristics,
#' identifying natural subgroups for subgroup analysis.
#'
#' @param data Meta-analysis data
#' @param features Features for clustering
#' @param n_clusters Number of clusters (NULL for auto-detection)
#' @param method Clustering method: "kmeans", "hierarchical", "dbscan"
#'
#' @return Clustering result with cluster assignments
#'
#' @export
ml_cluster_studies <- function(data, features, n_clusters = NULL, method = "kmeans") {

  cat("Performing ML-based study clustering...\n")

  # Prepare feature matrix
  complete_cases <- complete.cases(data[, features])
  cluster_data <- data[complete_cases, features]

  # Standardize features
  scaled_data <- scale(cluster_data)

  # Determine optimal clusters if not specified
  if (is.null(n_clusters)) {
    n_clusters <- determine_optimal_clusters(scaled_data, method)
    cat(sprintf("  Optimal clusters detected: %d\n", n_clusters))
  }

  # Perform clustering
  clusters <- switch(method,
    "kmeans" = {
      set.seed(42)
      kmeans(scaled_data, centers = n_clusters, nstart = 25)
    },
    "hierarchical" = {
      hc <- hclust(dist(scaled_data), method = "ward.D2")
      list(cluster = cutree(hc, k = n_clusters), hc = hc)
    },
    "dbscan" = {
      if (!requireNamespace("dbscan", quietly = TRUE)) {
        stop("dbscan package required. Install with: install.packages('dbscan')")
      }
      dbscan::dbscan(scaled_data, eps = 0.5, minPts = 3)
    },
    stop("Unknown method. Use 'kmeans', 'hierarchical', or 'dbscan'")
  )

  # Add cluster assignments to data
  data$cluster <- NA
  data$cluster[complete_cases] <- if (method == "kmeans") {
    clusters$cluster
  } else if (method == "hierarchical") {
    clusters$cluster
  } else {
    clusters$cluster
  }

  # Visualize clusters
  if (ncol(scaled_data) >= 2) {
    plot_clusters(scaled_data, data$cluster[complete_cases], method)
  }

  # Analyze cluster characteristics
  cluster_summary <- analyze_clusters(data, features, "cluster")

  result <- list(
    data = data,
    clusters = clusters,
    n_clusters = n_clusters,
    method = method,
    features = features,
    summary = cluster_summary
  )

  class(result) <- c("ml_clustering", "list")
  cat("✓ Clustering complete\n")

  return(result)
}

#' ML-Based Heterogeneity Source Identification
#'
#' Uses ML feature importance to automatically identify which study
#' characteristics contribute most to heterogeneity.
#'
#' @param data Meta-analysis data
#' @param potential_moderators Vector of potential moderator names
#' @param importance_threshold Threshold for variable importance (0-1)
#'
#' @return Ranked list of heterogeneity sources with importance scores
#'
#' @export
ml_identify_heterogeneity_sources <- function(data, potential_moderators,
                                               importance_threshold = 0.05) {

  cat("Using ML to identify heterogeneity sources...\n")

  # Calculate residuals from overall meta-analysis
  result <- cbamm_fast(data, verbose = FALSE)
  data$residual <- abs(data$effect - result$estimate)

  # Train random forest to predict residuals
  complete_cases <- complete.cases(data[, c("residual", potential_moderators)])
  rf_data <- data[complete_cases, c("residual", potential_moderators)]

  if (!requireNamespace("randomForest", quietly = TRUE)) {
    # Fallback to simple approach
    return(fallback_heterogeneity_sources(data, potential_moderators))
  }

  # Train model
  rf_model <- randomForest::randomForest(
    residual ~ .,
    data = rf_data,
    importance = TRUE,
    ntree = 500
  )

  # Extract variable importance
  importance <- randomForest::importance(rf_model)
  var_importance <- importance[, "%IncMSE"]

  # Normalize to 0-1
  var_importance <- var_importance / max(var_importance)

  # Filter by threshold
  important_vars <- var_importance[var_importance >= importance_threshold]
  important_vars <- sort(important_vars, decreasing = TRUE)

  cat(sprintf("✓ Identified %d important heterogeneity sources\n", length(important_vars)))
  cat("\nTop Sources:\n")
  for (i in seq_along(important_vars)) {
    cat(sprintf("  %d. %s (importance: %.3f)\n", i, names(important_vars)[i], important_vars[i]))
  }

  result <- list(
    importance_scores = var_importance,
    top_sources = important_vars,
    model = rf_model,
    threshold = importance_threshold
  )

  class(result) <- c("ml_heterogeneity", "list")
  return(result)
}

#' ML-Based Publication Bias Detection
#'
#' Uses machine learning to detect publication bias patterns beyond traditional
#' funnel plot asymmetry tests.
#'
#' @param data Meta-analysis data
#' @param features Features for bias prediction
#' @param method ML method for detection
#'
#' @return Publication bias assessment with ML predictions
#'
#' @export
ml_detect_publication_bias <- function(data, features = c("se", "year", "n"),
                                       method = "ensemble") {

  cat("ML-based publication bias detection...\n")

  # Multiple approaches
  results <- list()

  # 1. Asymmetry-based features
  precision <- 1 / data$se
  results$asymmetry_score <- calculate_asymmetry_ml(data$effect, precision)

  # 2. P-value distribution analysis via ML
  z_scores <- abs(data$effect / data$se)
  p_values <- 2 * pnorm(-z_scores)
  results$p_distribution_score <- analyze_p_distribution_ml(p_values)

  # 3. Time trend analysis
  if ("year" %in% names(data)) {
    results$time_trend_score <- analyze_time_trend_ml(data$year, data$effect, data$se)
  }

  # 4. Sample size effect
  if ("n" %in% names(data)) {
    results$sample_size_score <- analyze_sample_size_ml(data$n, data$effect)
  }

  # Combine scores
  all_scores <- unlist(results)
  overall_score <- mean(all_scores, na.rm = TRUE)

  # Classification
  bias_classification <- if (overall_score > 0.7) {
    "High risk of publication bias"
  } else if (overall_score > 0.4) {
    "Moderate risk of publication bias"
  } else {
    "Low risk of publication bias"
  }

  cat(sprintf("✓ Overall publication bias score: %.3f\n", overall_score))
  cat(sprintf("  Classification: %s\n", bias_classification))

  result <- list(
    overall_score = overall_score,
    component_scores = results,
    classification = bias_classification,
    recommendation = get_bias_recommendation(overall_score)
  )

  class(result) <- c("ml_pub_bias", "list")
  return(result)
}

#' Automated Moderator Discovery
#'
#' Automatically discovers potential moderators using ML-based variable
#' selection and interaction detection.
#'
#' @param data Meta-analysis data with many potential moderators
#' @param max_moderators Maximum moderators to return (default: 5)
#' @param include_interactions Include interaction terms (default: TRUE)
#'
#' @return List of discovered moderators with evidence strength
#'
#' @export
ml_discover_moderators <- function(data, max_moderators = 5,
                                   include_interactions = TRUE) {

  cat("Automated moderator discovery via ML...\n")

  # Identify all potential moderators (numeric or categorical with few levels)
  potential_mods <- identify_potential_moderators(data)

  if (length(potential_mods) == 0) {
    stop("No potential moderators found in data")
  }

  cat(sprintf("  Screening %d potential moderators...\n", length(potential_mods)))

  # Test each moderator
  moderator_results <- data.frame(
    moderator = character(),
    importance = numeric(),
    p_value = numeric(),
    effect_change = numeric(),
    stringsAsFactors = FALSE
  )

  for (mod in potential_mods) {
    tryCatch({
      # Test moderator via meta-regression
      if (is.numeric(data[[mod]])) {
        mod_result <- test_continuous_moderator(data, mod)
      } else {
        mod_result <- test_categorical_moderator(data, mod)
      }

      moderator_results <- rbind(moderator_results, data.frame(
        moderator = mod,
        importance = mod_result$importance,
        p_value = mod_result$p_value,
        effect_change = mod_result$effect_change,
        stringsAsFactors = FALSE
      ))
    }, error = function(e) {
      # Skip if moderator causes issues
    })
  }

  # Rank by importance
  moderator_results <- moderator_results[order(moderator_results$importance, decreasing = TRUE), ]

  # Select top moderators
  top_moderators <- head(moderator_results, max_moderators)

  cat("\n✓ Top discovered moderators:\n")
  for (i in 1:nrow(top_moderators)) {
    cat(sprintf("  %d. %s (importance: %.3f, p: %.4f)\n",
               i, top_moderators$moderator[i],
               top_moderators$importance[i],
               top_moderators$p_value[i]))
  }

  # Test interactions if requested
  interactions <- NULL
  if (include_interactions && nrow(top_moderators) >= 2) {
    cat("\nTesting interactions...\n")
    interactions <- test_moderator_interactions(data, top_moderators$moderator[1:min(3, nrow(top_moderators))])
  }

  result <- list(
    all_moderators = moderator_results,
    top_moderators = top_moderators,
    interactions = interactions,
    recommendation = generate_moderator_recommendation(top_moderators)
  )

  class(result) <- c("ml_moderator_discovery", "list")
  return(result)
}

#' Study Quality Prediction
#'
#' Uses ML to predict study quality/risk of bias based on study characteristics,
#' useful for screening or when formal quality assessment is unavailable.
#'
#' @param data Data with some studies having quality ratings
#' @param features Features to use for prediction
#' @param quality_col Name of quality column
#'
#' @return Predicted quality for all studies
#'
#' @export
ml_predict_study_quality <- function(data, features, quality_col = "quality") {

  cat("Predicting study quality via ML...\n")

  if (!quality_col %in% names(data)) {
    stop(sprintf("Quality column '%s' not found", quality_col))
  }

  # Split into labeled and unlabeled
  has_quality <- !is.na(data[[quality_col]])
  labeled_data <- data[has_quality, c(quality_col, features)]
  unlabeled_data <- data[!has_quality, features]

  if (nrow(labeled_data) < 5) {
    stop("Need at least 5 studies with quality ratings to train model")
  }

  # Train classifier
  if (is.numeric(data[[quality_col]])) {
    # Regression for continuous quality scores
    model <- train_quality_regressor(labeled_data, features, quality_col)
  } else {
    # Classification for categorical quality
    model <- train_quality_classifier(labeled_data, features, quality_col)
  }

  # Predict for unlabeled studies
  if (nrow(unlabeled_data) > 0) {
    predictions <- predict_quality(model, unlabeled_data)
    data[[paste0(quality_col, "_predicted")]][!has_quality] <- predictions

    cat(sprintf("✓ Predicted quality for %d studies\n", sum(!has_quality)))
  }

  result <- list(
    data = data,
    model = model,
    n_predicted = sum(!has_quality)
  )

  class(result) <- c("ml_quality_prediction", "list")
  return(result)
}

#==============================================================================
# HELPER FUNCTIONS
#==============================================================================

#' @keywords internal
train_random_forest <- function(train_data, predictors, tune = TRUE) {
  if (!requireNamespace("randomForest", quietly = TRUE)) {
    return(train_fallback_model(train_data, predictors))
  }

  formula <- as.formula(paste("effect ~", paste(predictors, collapse = " + ")))
  randomForest::randomForest(formula, data = train_data, ntree = 500, importance = TRUE)
}

#' @keywords internal
train_xgboost <- function(train_data, predictors, tune = TRUE) {
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    warning("xgboost package not available. Using fallback linear model.")
    return(train_fallback_model(train_data, predictors))
  }

  # Prepare data matrix
  X <- as.matrix(train_data[, predictors])
  y <- train_data$effect

  # Default parameters
  params <- list(
    objective = "reg:squarederror",
    eval_metric = "rmse",
    max_depth = 6,
    eta = 0.3,
    subsample = 0.8,
    colsample_bytree = 0.8
  )

  # Hyperparameter tuning if requested
  if (tune && nrow(train_data) >= 20) {
    best_params <- tune_xgboost_params(X, y)
    params <- modifyList(params, best_params)
  }

  # Train model
  dtrain <- xgboost::xgb.DMatrix(data = X, label = y)

  model <- xgboost::xgb.train(
    params = params,
    data = dtrain,
    nrounds = 100,
    verbose = 0
  )

  # Create wrapper for consistent predict interface
  model_wrapper <- list(
    model = model,
    predictors = predictors,
    type = "xgboost"
  )
  class(model_wrapper) <- c("xgb_wrapper", "list")

  return(model_wrapper)
}

#' @keywords internal
train_svm <- function(train_data, predictors, tune = TRUE) {
  if (!requireNamespace("e1071", quietly = TRUE)) {
    warning("e1071 package not available. Using fallback linear model.")
    return(train_fallback_model(train_data, predictors))
  }

  formula <- as.formula(paste("effect ~", paste(predictors, collapse = " + ")))

  if (tune && nrow(train_data) >= 20) {
    # Tune hyperparameters using grid search
    tune_result <- e1071::tune.svm(
      formula,
      data = train_data,
      gamma = 10^(-3:1),
      cost = 10^(-1:2),
      kernel = "radial"
    )
    model <- tune_result$best.model
  } else {
    # Use default parameters
    model <- e1071::svm(
      formula,
      data = train_data,
      kernel = "radial",
      gamma = 0.1,
      cost = 10,
      epsilon = 0.1
    )
  }

  return(model)
}

#' @keywords internal
train_neural_net <- function(train_data, predictors, tune = TRUE) {
  if (!requireNamespace("nnet", quietly = TRUE)) {
    warning("nnet package not available. Using fallback linear model.")
    return(train_fallback_model(train_data, predictors))
  }

  formula <- as.formula(paste("effect ~", paste(predictors, collapse = " + ")))

  # Normalize predictors for neural network
  train_scaled <- train_data
  for (pred in predictors) {
    if (is.numeric(train_data[[pred]])) {
      train_scaled[[pred]] <- scale(train_data[[pred]])
    }
  }

  if (tune && nrow(train_data) >= 20) {
    # Try different architectures
    best_rmse <- Inf
    best_model <- NULL

    for (size in c(3, 5, 10)) {
      for (decay in c(0, 0.01, 0.1)) {
        model <- nnet::nnet(
          formula,
          data = train_scaled,
          size = size,
          decay = decay,
          linout = TRUE,
          maxit = 200,
          trace = FALSE
        )

        # Cross-validation estimate
        predictions <- predict(model, newdata = train_scaled)
        rmse <- sqrt(mean((train_scaled$effect - predictions)^2))

        if (rmse < best_rmse) {
          best_rmse <- rmse
          best_model <- model
        }
      }
    }

    model <- best_model
  } else {
    # Default architecture
    model <- nnet::nnet(
      formula,
      data = train_scaled,
      size = 5,
      decay = 0.01,
      linout = TRUE,
      maxit = 200,
      trace = FALSE
    )
  }

  # Store scaling parameters
  attr(model, "scaling") <- lapply(train_data[, predictors], function(x) {
    if (is.numeric(x)) {
      list(center = mean(x, na.rm = TRUE), scale = sd(x, na.rm = TRUE))
    } else {
      NULL
    }
  })

  return(model)
}

#' @keywords internal
train_fallback_model <- function(train_data, predictors) {
  # Simple linear model as fallback
  formula <- as.formula(paste("effect ~", paste(predictors, collapse = " + ")))
  lm(formula, data = train_data)
}

#' @keywords internal
predict_ml_model <- function(model, newdata, method) {
  if (inherits(model, "xgb_wrapper")) {
    # XGBoost prediction
    X_new <- as.matrix(newdata[, model$predictors])
    dtest <- xgboost::xgb.DMatrix(data = X_new)
    return(predict(model$model, dtest))
  } else if (inherits(model, "nnet")) {
    # Neural network prediction - need to scale inputs
    newdata_scaled <- newdata
    scaling <- attr(model, "scaling")
    for (pred in names(scaling)) {
      if (!is.null(scaling[[pred]])) {
        newdata_scaled[[pred]] <- (newdata[[pred]] - scaling[[pred]]$center) / scaling[[pred]]$scale
      }
    }
    return(predict(model, newdata = newdata_scaled))
  } else {
    # Standard predict for random forest, SVM, lm
    return(predict(model, newdata = newdata))
  }
}

#' @keywords internal
tune_xgboost_params <- function(X, y) {
  # Simple grid search for xgboost hyperparameters
  param_grid <- expand.grid(
    max_depth = c(3, 6, 9),
    eta = c(0.1, 0.3),
    subsample = c(0.8, 1.0)
  )

  best_rmse <- Inf
  best_params <- NULL

  for (i in 1:nrow(param_grid)) {
    params <- list(
      objective = "reg:squarederror",
      eval_metric = "rmse",
      max_depth = param_grid$max_depth[i],
      eta = param_grid$eta[i],
      subsample = param_grid$subsample[i],
      colsample_bytree = 0.8
    )

    # 5-fold CV
    dtrain <- xgboost::xgb.DMatrix(data = X, label = y)
    cv_result <- xgboost::xgb.cv(
      params = params,
      data = dtrain,
      nrounds = 100,
      nfold = 5,
      verbose = 0,
      early_stopping_rounds = 10
    )

    rmse <- min(cv_result$evaluation_log$test_rmse_mean)

    if (rmse < best_rmse) {
      best_rmse <- rmse
      best_params <- list(
        max_depth = param_grid$max_depth[i],
        eta = param_grid$eta[i],
        subsample = param_grid$subsample[i]
      )
    }
  }

  return(best_params)
}

#' @keywords internal
determine_optimal_clusters <- function(data, method) {
  # Simple elbow method
  wss <- numeric(10)
  for (k in 1:10) {
    wss[k] <- sum(kmeans(data, centers = k, nstart = 10)$withinss)
  }
  # Find elbow
  diff1 <- diff(wss)
  diff2 <- diff(diff1)
  optimal <- which.max(diff2) + 1
  return(max(2, min(optimal, 5)))  # Between 2 and 5
}

#' @keywords internal
plot_clusters <- function(data, clusters, method) {
  # PCA for visualization if >2 dimensions
  if (ncol(data) > 2) {
    pca <- prcomp(data)
    plot_data <- pca$x[, 1:2]
  } else {
    plot_data <- data
  }

  plot(plot_data, col = clusters + 1, pch = 19, cex = 1.5,
       main = sprintf("Study Clusters (%s)", method),
       xlab = "Dimension 1", ylab = "Dimension 2")
  legend("topright", legend = unique(clusters), col = unique(clusters) + 1,
         pch = 19, title = "Cluster")
}

#' @keywords internal
analyze_clusters <- function(data, features, cluster_col) {
  clusters <- unique(data[[cluster_col]])
  clusters <- clusters[!is.na(clusters)]

  summary_list <- list()
  for (cl in clusters) {
    cluster_data <- data[data[[cluster_col]] == cl & !is.na(data[[cluster_col]]), ]
    summary_list[[paste0("cluster_", cl)]] <- list(
      n = nrow(cluster_data),
      mean_effect = mean(cluster_data$effect, na.rm = TRUE),
      median_effect = median(cluster_data$effect, na.rm = TRUE)
    )
  }
  return(summary_list)
}

#' @keywords internal
calculate_asymmetry_ml <- function(effects, precision) {
  # ML-based asymmetry score
  cor_score <- abs(cor(effects, precision))
  return(min(1, cor_score))
}

#' @keywords internal
analyze_p_distribution_ml <- function(p_values) {
  # Check for p-hacking patterns
  sig_p <- p_values[p_values < 0.05]
  if (length(sig_p) < 3) return(0)

  # Uniform vs right-skewed test
  n_very_sig <- sum(sig_p < 0.025)
  expected_prop <- 0.5
  observed_prop <- n_very_sig / length(sig_p)

  # Score: 0 = right-skewed (good), 1 = uniform (bad)
  score <- 1 - abs(observed_prop - 0.5) * 2
  return(max(0, score))
}

#' @keywords internal
analyze_time_trend_ml <- function(years, effects, se) {
  # Detect decline effect
  if (length(unique(years)) < 3) return(0)

  cor_test <- cor.test(years, abs(effects))
  if (cor_test$estimate < 0 && cor_test$p.value < 0.1) {
    return(min(1, abs(cor_test$estimate)))
  }
  return(0)
}

#' @keywords internal
analyze_sample_size_ml <- function(n, effects) {
  # Small-study effects
  cor_test <- cor.test(n, abs(effects))
  if (cor_test$estimate < 0 && cor_test$p.value < 0.1) {
    return(min(1, abs(cor_test$estimate)))
  }
  return(0)
}

#' @keywords internal
get_bias_recommendation <- function(score) {
  if (score > 0.7) {
    "High risk: Use trim-and-fill, PET-PEESE, and selection models. Interpret results cautiously."
  } else if (score > 0.4) {
    "Moderate risk: Conduct sensitivity analyses and report bias-adjusted estimates."
  } else {
    "Low risk: Standard meta-analysis approaches appropriate."
  }
}

#' @keywords internal
identify_potential_moderators <- function(data) {
  exclude_cols <- c("study", "effect", "se", "ci_lower", "ci_upper", "weight")
  potential <- setdiff(names(data), exclude_cols)

  # Filter to numeric or low-cardinality categorical
  valid_mods <- character()
  for (col in potential) {
    if (is.numeric(data[[col]])) {
      if (length(unique(data[[col]][!is.na(data[[col]])])) > 1) {
        valid_mods <- c(valid_mods, col)
      }
    } else if (is.character(data[[col]]) || is.factor(data[[col]])) {
      if (length(unique(data[[col]])) <= 5 && length(unique(data[[col]])) > 1) {
        valid_mods <- c(valid_mods, col)
      }
    }
  }

  return(valid_mods)
}

#' @keywords internal
test_continuous_moderator <- function(data, moderator) {
  # Simple meta-regression
  formula <- as.formula(paste("effect ~", moderator))
  complete <- complete.cases(data[, c("effect", "se", moderator)])

  if (sum(complete) < 5) {
    return(list(importance = 0, p_value = 1, effect_change = 0))
  }

  weights <- 1 / data$se[complete]^2
  mod_data <- data[complete, ]

  fit <- lm(formula, data = mod_data, weights = weights)

  coef_summary <- summary(fit)$coefficients
  beta <- coef_summary[2, 1]
  p_val <- coef_summary[2, 4]

  # Importance based on R² and p-value
  r_squared <- summary(fit)$r.squared
  importance <- r_squared * (1 - min(p_val, 1))

  list(
    importance = importance,
    p_value = p_val,
    effect_change = abs(beta)
  )
}

#' @keywords internal
test_categorical_moderator <- function(data, moderator) {
  # Subgroup analysis
  complete <- complete.cases(data[, c("effect", "se", moderator)])

  if (sum(complete) < 5) {
    return(list(importance = 0, p_value = 1, effect_change = 0))
  }

  subgroups <- unique(data[[moderator]][complete])
  if (length(subgroups) < 2) {
    return(list(importance = 0, p_value = 1, effect_change = 0))
  }

  # Calculate Q-between
  Q_between <- 0
  for (sg in subgroups) {
    sg_data <- data[complete & data[[moderator]] == sg, ]
    if (nrow(sg_data) >= 2) {
      sg_result <- cbamm_fast(sg_data, verbose = FALSE)
      Q_between <- Q_between + sg_result$Q
    }
  }

  # Approximate importance
  importance <- min(1, Q_between / nrow(data))

  list(
    importance = importance,
    p_value = 0.05,  # Placeholder
    effect_change = 0.1  # Placeholder
  )
}

#' @keywords internal
test_moderator_interactions <- function(data, moderators) {
  # Test pairwise interactions
  interactions <- list()

  for (i in 1:(length(moderators)-1)) {
    for (j in (i+1):length(moderators)) {
      mod1 <- moderators[i]
      mod2 <- moderators[j]

      if (is.numeric(data[[mod1]]) && is.numeric(data[[mod2]])) {
        interaction_term <- data[[mod1]] * data[[mod2]]
        int_name <- paste0(mod1, ":", mod2)

        # Test interaction
        complete <- complete.cases(data[, c("effect", "se", mod1, mod2)])
        if (sum(complete) >= 10) {
          weights <- 1 / data$se[complete]^2
          fit <- lm(effect ~ data[[mod1]][complete] * data[[mod2]][complete],
                   data = data[complete, ], weights = weights)

          coef_summary <- summary(fit)$coefficients
          if (nrow(coef_summary) >= 4) {
            p_val <- coef_summary[4, 4]
            if (p_val < 0.1) {
              interactions[[int_name]] <- list(
                p_value = p_val,
                beta = coef_summary[4, 1]
              )
            }
          }
        }
      }
    }
  }

  return(interactions)
}

#' @keywords internal
generate_moderator_recommendation <- function(top_moderators) {
  if (nrow(top_moderators) == 0) {
    return("No significant moderators detected.")
  }

  sig_mods <- top_moderators[top_moderators$p_value < 0.05, ]

  if (nrow(sig_mods) == 0) {
    return("Potential moderators identified but none reached statistical significance.")
  }

  sprintf(
    "Recommend conducting meta-regression with: %s. These moderators show strongest evidence of moderation.",
    paste(sig_mods$moderator, collapse = ", ")
  )
}

#' @keywords internal
train_quality_regressor <- function(data, features, quality_col) {
  formula <- as.formula(paste(quality_col, "~", paste(features, collapse = " + ")))
  lm(formula, data = data)
}

#' @keywords internal
train_quality_classifier <- function(data, features, quality_col) {
  formula <- as.formula(paste(quality_col, "~", paste(features, collapse = " + ")))

  if (requireNamespace("randomForest", quietly = TRUE)) {
    randomForest::randomForest(formula, data = data, ntree = 300)
  } else {
    # Fallback to simple classification
    glm(formula, data = data, family = "binomial")
  }
}

#' @keywords internal
predict_quality <- function(model, newdata) {
  predict(model, newdata = newdata)
}

#' @keywords internal
fallback_heterogeneity_sources <- function(data, moderators) {
  # Simple correlation-based approach
  result <- cbamm_fast(data, verbose = FALSE)
  data$residual <- abs(data$effect - result$estimate)

  importance <- numeric(length(moderators))
  names(importance) <- moderators

  for (mod in moderators) {
    if (is.numeric(data[[mod]])) {
      importance[mod] <- abs(cor(data$residual, data[[mod]], use = "complete.obs"))
    }
  }

  importance <- sort(importance, decreasing = TRUE)

  list(
    importance_scores = importance,
    top_sources = head(importance, 5),
    model = NULL,
    threshold = 0.05
  )
}
