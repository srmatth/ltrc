
#' Get Clean LTRC Model Object
#'
#' @param mod a model fit using the `ltrc()` function
#'
#' @return an object of class `"ltrc_mod"` containing clean information from the model fit, which can
#'   then be passed to other methods
#' @export
get_clean_model <- function(mod) {
  clean_mod <- list(
    parameters = list(
      beta = mod$theta[1:mod$p],
      gamma = mod$theta[(mod$p+1):length(mod$theta)],
      beta_start = mod$theta0[1:mod$p],
      gamma_start = mod$theta0[(mod$p+1):length(mod$theta)],
      interior_knots = mod$knots,
      boundary_knots = mod$boundary_knots
    ),
    metrics = list(
      log_likelihood = mod$lnlklhd,
      log_likelihood_start = mod$lnlklhd0,
      num_iter = mod$nbriter,
      converge = mod$converge
    ),
    data = list(
      observed_response = mod$response,
      predictors = mod$predictors,
      num_predictors = mod$p,
      fitted_response = mod$fitted_values,
      residuals = mod$residuals,
      score = mod$score,
      information = mod$inform,
      information_efficient = mod$efficient_score_info
    )
  )

  if (!is.null(mod$all_res)) {
    clean_mod$all_starts <- mod$all_starts
    clean_mod$all_res <- mod$all_res
  }
  class(clean_mod) <- "ltrc_mod"
  clean_mod
}



#' Predict using an LTRC Model
#'
#' Note that we need to add on the intercept by taking the mean of the residuals.
#' This may make the predictions not theoretically sound.
#'
#' @param mod a model fit using the `ltrc()` function
#' @param newdata a `matrix` or `data.frame` with the columns matching the order
#'   of the predictors in the model
#'
#' @return a vector of predictions
#' @export
predict.ltrc_mod <- function(object, newdata, ...) {
  # Check if newdata is provided
  if (missing(newdata)) {
    stop("Please provide newdata for prediction.")
  }

  # Ensure newdata is a matrix or data frame
  if (!is.matrix(newdata) && !is.data.frame(newdata)) {
    stop("newdata must be a matrix or data frame.")
  }

  # Check if the number of predictors matches
  num_predictors <- object$data$num_predictors
  if (ncol(newdata) != num_predictors) {
    stop(paste(
      "newdata must have exactly", num_predictors, "columns,",
      "matching the number of predictors in the model."
    ))
  }

  # Extract beta coefficients
  beta <- object$parameters$beta

  # Ensure newdata is a matrix for matrix multiplication
  newdata <- as.matrix(newdata)

  # Compute predictions as a linear combination of predictors and beta
  predictions <- newdata %*% beta + mean(object$data$residuals)

  # Return the predictions
  return(as.vector(predictions)) # Convert to a vector for convenience
}

#' Summary of an LTRC Model
#'
#' Prints out a detailed summary of a fitted LTRC model.
#'
#' @param mod a model fit using the `ltrc()` function
#'
#' @return None
#' @export
summary.ltrc_mod <- function(object, ...) {
  cat("Summary of ltrc_mod object\n")
  cat("--------------------------\n")

  # Parameters
  cat("Model Parameters:\n")
  cat("  Beta Coefficients:\n")
  print(object$parameters$beta)
  cat("  Gamma Coefficients:\n")
  print(object$parameters$gamma)
  cat("\nInterior Knots:\n")
  print(object$parameters$interior_knots)
  cat("Boundary Knots:\n")
  print(object$parameters$boundary_knots)

  # Metrics
  cat("\nModel Metrics:\n")
  cat(sprintf("  Log-Likelihood: %.4f\n", object$metrics$log_likelihood))
  cat(sprintf("  Log-Likelihood (Start): %.4f\n", object$metrics$log_likelihood_start))
  cat(sprintf("  Number of Iterations: %d\n", object$metrics$num_iter))
  cat(sprintf("  Converged: %s\n", ifelse(object$metrics$converge, "Yes", "No")))

  # Data Summary
  cat("\nData Summary:\n")
  cat(sprintf("  Number of Predictors: %d\n", object$data$num_predictors))
  cat(sprintf("  Number of Observed Responses: %d\n", length(object$data$observed_response)))

  residuals <- object$data$residuals
  residual_summary <- c(min(residuals), quantile(residuals, 0.25), median(residuals), quantile(residuals, 0.75), max(residuals))
  residual_sd <- sd(residuals)
  residual_mean <- mean(residuals)

  cat("\nResiduals Summary:\n")
  cat(sprintf("  Min: %.4f, 1st Qu.: %.4f, Median: %.4f, Mean: %.4f, 3rd Qu.: %.4f, Max: %.4f\n",
              residual_summary[1], residual_summary[2], residual_summary[3], residual_mean,
              residual_summary[4], residual_summary[4]))
  cat(sprintf("  Standard Deviation: %.4f\n", residual_sd))
}

#' Print an LTRC Model
#'
#' Prints out a brief summary of a fitted LTRC model.
#'
#' @param object a model fit using the `ltrc()` function
#'
#' @return None
#' @export
print.ltrc_mod <- function(object, ...) {
  cat("ltrc_mod Object\n")
  cat("----------------\n")
  cat("Model Parameters:\n")
  cat("  Beta Coefficients:\n")
  print(object$parameters$beta)
  cat("\nMetrics:\n")
  cat(sprintf("  Log-Likelihood: %.4f\n", object$metrics$log_likelihood))
  cat(sprintf("  Converged: %s\n", ifelse(object$metrics$converge, "Yes", "No")))
}

#' Variance-Covariance Matrix for LTRC Model Parameters
#'
#' @param object a model fit using the `ltrc()` function
#' @param type character string specifying the method used to compute the
#'   variance, one of `c("efficient_score", "inverse_information")`
#' @param spline_params Logical indicating if the spline parameter variances
#'   should be included in the returned matrix. Only for `type = "inverse_information`.
#'   Defaults to `FALSE`.
#' @param ... unused additional arguments
#'
#' @return a variance-covariance matrix
#' @export
vcov.ltrc_mod <- function(object, type = c("efficient_score", "inverse_information"), spline_params = FALSE, ...) {
  type = type[1]
  if (type == "efficient_score") {
    return(solve(object$data$information_efficient))
  } else if (type == "inverse_information") {
    mat <- solve(object$data$information)
    if (!spline_params) mat <- mat[1:object$data$num_predictors, 1:object$data$num_predictors]
    return(mat)
  } else{
    stop("Argument `type` must be one of `c('efficient_score', 'inverse_information')`")
  }
}

