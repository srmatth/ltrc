#' ltrc regression
#'
#' Left-truncated right-censored regression model fit using a seive maximum likelihood approach.
#'
#' @param formula a `formula` object of the form `y ~ x`, where `y` must be a `Surv()` object
#'   from the `survival` package
#' @param trunc_time a vector indicating the truncation time for each subject
#' @param data The data where the variables in the formula can be found
#' @param n_start the number of random starts for the fitting algorithm
#' @param int_knots the number of interior knots for the spline basis
#' @param verbose bool indicating if intermediate steps should be printed,
#'   defaults to `FALSE`
#' @param n_folds the number of folds for cross-validation, if used
#' @param knot_range the number of knots to check for cross validation
#' @param knots if you want to fix the knots on the spline, pass the knots here
#' @param boundary_knots if you want to fix the knots on the spline, pass the boundary knots here
#' @param sd_multiplier controls the variance of the random noise for the parameter starting values
#'
#' @return a list with several elements
#' @export
ltrc <- function(formula, trunc_time, data = NULL, n_start = 10, int_knots = 5, verbose = FALSE,
                 n_folds = 5, knot_range = 1:10, knots = NULL, boundary_knots = NULL, sd_multiplier = 3,
                 diag_only = TRUE, iter_max = 100, return_all = FALSE) {
  dat_l <- extract_surv_components(formula, data)
  orig_int_knots <- int_knots

  y <- dat_l$survival_times
  X <- dat_l$covariate_matrix
  t <- trunc_time
  delta <- dat_l$event_indicator
  if (!is.matrix(X)) {
    X <- matrix(X, ncol = 1)
  }
  p <- ncol(X)

  naive_mod <- lm(y ~ X)
  beta <- naive_mod$coefficients[-1]
  sigma <- sqrt(diag(vcov(naive_mod)))[-1] * sd_multiplier
  starts <- matrix(beta + rnorm(p * n_start, mean = 0, sd = sigma), nrow = p)
  best_lnlklhd <- -Inf
  best_res <- NULL


  ## Do cross-validation here if the number of interior knots is not specified
  if (int_knots < 1) {
    knot_lnlklhds <- c()
    # Create indices for k-fold cross-validation
    folds <- cut(seq(1, nrow(X)), breaks = n_folds, labels = FALSE)
    ## Loop over the candidates for number of knots
    for (tmp_k in knot_range) {
      print(paste0("Working on cross validation with ", tmp_k, " knots"))
      gamma_start <- rnorm(tmp_k + 4, sd = 0.1)
      cv_lnlklhds <- numeric(n_folds)  # Vector to store the cross-validation errors
      ## Loop over each fold for cross-validation
      for (j in 1:n_folds) {
        # Indices for the training and validation sets
        test_indices <- which(folds == j)
        train_indices <- setdiff(1:nrow(X), test_indices)

        # Data for training and validation
        y_train <- y[train_indices]
        X_train <- X[train_indices,]
        t_train <- t[train_indices]
        delta_train <- delta[train_indices]

        y_test <- y[test_indices]
        X_test <- X[test_indices,]
        t_test <- t[test_indices]
        delta_test <- delta[test_indices]
        best_lnlklhd <- -Inf
        ## loop over the starting values to find the best model
        for (i in 1:ncol(starts)) {
          beta_start <- as.numeric(starts[,i])
          # Train the model on the training set
          tmp_tmp_res <- newtraph(y_train, X_train, t_train, delta_train, beta_start, gamma_start, lklhd = lnlklhd, K = tmp_k,
                                  verbose = verbose, diag_only = diag_only, iter_max = iter_max)
          if (best_lnlklhd < tmp_tmp_res$lnlklhd) {
            tmp_res <- tmp_tmp_res
          }
        }

        # Evaluate the model on the validation fold for this loop
        cv_lnlklhds[j] <- lnlklhd(
          tmp_res$theta[1:p],
          tmp_res$theta[(p+1):(p+tmp_k + 4)],
          X_test,
          y_test,
          t_test,
          delta_test,
          knots = tmp_res$knots,
          boundary_knots = tmp_res$boundary_knots
        )$lnlklhd
      }
      # Calculate average log-likelihood across all folds
      knot_lnlklhds <- c(knot_lnlklhds, mean(cv_lnlklhds))
    }

    # Use the average CV log_likelihood as the criterion for choosing the best model
    best_knots_indx <- which.max(knot_lnlklhds)
    # set int_knots to be the knots with the best log-likelihood
    int_knots <- knot_range[best_knots_indx]
  } else if (!is.null(knots) & !is.null(boundary_knots)) {
    int_knots <- length(knots)
  }

  if (return_all) {
    all_res <- list()
  }

  for (i in 1:ncol(starts)) {
    gamma_start <- rnorm(int_knots + 4)
    beta_start <- as.numeric(starts[,i])
    tmp_res <- newtraph(y, X, t, delta, beta_start, gamma_start, lklhd = lnlklhd, K = int_knots,
                        verbose = verbose, knots = knots, boundary_knots = boundary_knots,
                        diag_only = diag_only, iter_max = iter_max)
    if (return_all) {
      all_res[[i]] <- tmp_res
    }

    # print("getting variance")
    # print(diag(solve(tmp_res$inform)))
    beta_var_positive <- all(diag(solve(tmp_res$inform))[1:p] > 0)

    if (best_lnlklhd < tmp_res$lnlklhd) {
      best_lnlklhd <- tmp_res$lnlklhd
      best_res <- tmp_res
    }
  }

  # if (is.null(best_res)) {
  #   print("Algorithm did not converge!")
  #   return(NULL)
  # }

  # print("trying to return all")
  if (return_all) {
    add_on <- list(
      all_starts = starts,
      all_res = all_res
    )
    best_res <- append(best_res, add_on)
  }

  if (orig_int_knots < 1) {
    best_res <- append(best_res, list(knot_lnlklhds = knot_lnlklhds))
  }

  best_res$int_knots <- int_knots
  best_res

}
