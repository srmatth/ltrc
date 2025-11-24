#' Simulation Iteration Updated
#'
#' @param i the number of iteration you are on
#' @param FINAL_KNOTS the number of knots used in the spline
#' @param true_beta the true beta vector, of size 3
#' @param n the number of observations
#' @param error_dist a function for the error distribution
#' @param sd_multiplier the multiplier for the beta random starts
#' @param n_start the number of random starts in each iteration
#' @param type_data a character string, one of `c("ltrc", "lt", "rc", "none")`
#'   signifying the type of data we are fitting the model to: left-truncated
#'   right-censored, left-truncated only, right-censored only, or a normal linear model
#' @param C_UB a numeric value representing the censoring distribution upper bound. For
#'   50% censoring = 11, for 30% censoring = 17, for 15% censoring = 30
#' @param sample_size_mult multiplier for the sample size (based on truncation prob)
#'
#' @return a list with the simulation results
sim_iteration_updated <- function(i, FINAL_KNOTS, true_beta = c(0.5, 1), n, error_dist, sd_multiplier = 3,
                                  n_start = 20, type_data = "ltrc", verbose = FALSE, diag_only = TRUE, return_all = FALSE,
                                  C_UB = 44, sample_size_mult = 10) {

  cat("Working on iteration ", i, "\n")
  # initialize covariates
  x <- matrix(NA, ncol = length(true_beta), nrow = sample_size_mult*n)
  # create binary covariate
  x[,1] <- rbinom(sample_size_mult*n, 1, 0.5)
  x[,2] <- runif(sample_size_mult*n, min = 0, max = 1)

  eps <- error_dist(sample_size_mult*n)
  y <- exp(x %*% true_beta + eps)

  ## Censoring
  c <- runif(sample_size_mult*n, 0, C_UB)
  delta <- as.numeric(y < c)
  y_cens <- ifelse(y < c, y, c)
  ## Induce left-truncation into our data
  t <- runif(sample_size_mult*n, 0, C_UB)
  obs_indx <- which(y_cens > t)
  sampled_indx <- sample(obs_indx, size = n)
  ## Getting observed Data
  y_obs <- y_cens[sampled_indx]
  x_obs <- x[sampled_indx,]
  t_obs <- t[sampled_indx]
  delta_obs <- delta[sampled_indx]
  c_obs <- c[sampled_indx]
  true_y_obs <- y[sampled_indx]
  y_obs_transformed <- log(y_obs)
  t_obs_transformed <- log(t_obs)
  # print(mean(delta_obs))
  # print(length(obs_indx) / length(t))

  ## Get initial values
  lin_mod <- lm(y_obs_transformed ~ x_obs)

  dat <- data.frame(
    y_obs_transformed, delta_obs, x_obs
  )

  tictoc::tic()
  # res <- ltrc::ltrc(formula = survival::Surv(y_obs_transformed, delta_obs) ~ x_obs, trunc_time = t_obs_transformed, data = dat, n_start = n_start, int_knots = FINAL_KNOTS,
  #                   sd_multiplier = sd_multiplier, verbose = verbose, diag_only = diag_only, return_all = return_all)
  res <- tryCatch({
    ltrc(
      formula = survival::Surv(y_obs_transformed, delta_obs) ~ x_obs,
      trunc_time = t_obs_transformed,
      data = dat,
      n_start = n_start,
      int_knots = FINAL_KNOTS,
      sd_multiplier = sd_multiplier,
      verbose = verbose,
      diag_only = diag_only,
      return_all = return_all
    )
  },
  error = function(e) {
    return(list(theta = NULL))
  })
  time <- tictoc::toc()

  if (is.null(res$theta)) {
    cat("Model fit failed! Skipping iteration...\n")
    return(NULL)
  }
  # rc_res <- ltrc(formula = survival::Surv(y_obs, delta_obs) ~ x_obs, rep(-10, length(delta_obs)), data = dat, n_start = n_start, int_knots = res$int_knots, sd_multiplier = sd_multiplier, diag_only = diag_only)

  # ## Evaluate the theoretical efficient score function and use it to compute the information matrix

  ## Using the estimated betas
  p <- length(true_beta)
  est_beta <- matrix(res$theta[1:p], nrow = p)
  epsilon <- y_obs_transformed - x_obs %*% est_beta
  tau <- t_obs_transformed - x_obs %*% est_beta

  conditional_means <- matrix(nrow = length(epsilon), ncol = p)
  for (i in 1:length(epsilon)) {
    conditional_means[i,] <- colMeans(matrix(x_obs[(epsilon[i] <= epsilon) & (epsilon[i] >= tau),], ncol = p))
  }
  log_haz <- predict_log_hazard(
    x = epsilon,
    gamma = res$theta[(p+1):length(res$theta)], knots = res$knots,
    boundary_knots = res$boundary_knots, deriv = 0
  )
  g_dot <- predict_log_hazard(
    x = epsilon,
    gamma = res$theta[(p+1):length(res$theta)], knots = res$knots,
    boundary_knots = res$boundary_knots, deriv = 1
  )

  # sort the data
  e_order <- order(epsilon)
  epsilon_sorted <- epsilon[e_order]
  tau_sorted <- tau[e_order]
  log_haz_sorted <- log_haz[e_order]
  g_dot_sorted <- g_dot[e_order]
  conditional_means_sorted <- conditional_means[e_order,]
  diffs_sorted <-c(diff(epsilon_sorted), 0)
  delta_sorted <- delta_obs[e_order]
  x_sorted <- x_obs[e_order,]

  ## Martingale Representation ----

  m_s <- function(s, d_, t_, e_) {
    res <- numeric(length(s))
    for (j in 1:length(s)) {
      u <- s[j]
      first_term <- d_ * as.numeric(e_ <= u)
      if (u <= t_) second_term <- 0
      else {
        upper_bound <- e_
        if (u <= e_) upper_bound <- u

        indicator_vec <- as.numeric((epsilon_sorted >= t_) & (epsilon_sorted <= upper_bound))
        second_term <- sum(indicator_vec * exp(log_haz_sorted) * diffs_sorted)
      }

      res[j] <- first_term - second_term
    }
    res
  }

  contribs_m <- matrix(nrow = length(epsilon), ncol = p)
  for (i in 1:length(epsilon)) {
    e_i <- epsilon[i]
    t_i <- tau[i]
    d_i <- delta_obs[i]
    x_i <- x_obs[i,]
    m_s(epsilon_sorted[i], d_i, t_i, e_i)
    mart_vals <- m_s(epsilon_sorted, d_i, t_i, e_i)
    dms <- c(diff(mart_vals), 0)
    uni_dimensional_part <- dms * (-g_dot_sorted)
    multi_dimensional_part <- matrix(rep(x_i, length(epsilon)), ncol = p, byrow = TRUE) - conditional_means_sorted
    contribs_m[i,] <- t(uni_dimensional_part) %*% multi_dimensional_part
  }

  efficient_score <- colSums(contribs_m)

  efficient_score_info <- matrix(0, ncol = p, nrow = p)
  for (i in 1:nrow(contribs_m)) {
    efficient_score_info <- efficient_score_info + (contribs_m[i,] %*% t(contribs_m[i,]))
  }

  tmp_dat <- list(
    n_obs = n,
    pct_truncated = 1 - length(obs_indx) / (2*n),
    pct_censored = 1 - mean(delta_obs),
    true_beta = true_beta,
    naive_beta = lin_mod$coefficients[2:(length(true_beta) + 1)],
    time = time$toc - time$tic,
    lnlklhd_start = res$lnlklhd0,
    beta_start = res$theta0[1:length(true_beta)],
    converge = res$converge,
    errored = res$err_occurred,
    n_iter = res$nbriter,
    lnlklhd = res$lnlklhd,
    beta = res$theta[1:length(true_beta)],
    gamma = res$theta[-(1:length(true_beta))],
    score = res$score,
    inform = res$inform,
    # vars = solve(res$inform) %>% diag(),
    basis_boundary_knots = res$boundary_knots,
    basis_internal_knots = res$knots,
    efficient_score = efficient_score,
    efficient_score_info = efficient_score_info,
    # efficient_score_vars = solve(efficient_score_info) %>% diag(),
    x = x_obs,
    y = y_obs,
    t = t_obs,
    delta = delta_obs
  )
  if (FINAL_KNOTS < 1) {
    tmp_dat <- append(tmp_dat, list(cv_results = data.frame(knots_tried = 1:10, avg_knot_lnlklhds = res$knot_lnlklhds)))
  }
  if (return_all) {
    add_on <- list(
      all_starts = res$all_starts,
      all_res = res$all_res
    )
    tmp_dat <- append(tmp_dat, add_on)
  }

  return(tmp_dat)

}
