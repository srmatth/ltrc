newtraph <- function(y, X, t, delta, beta, gamma, lklhd, verbose = FALSE, EPS = 1e-2, iter_max = 100,
                     K = 3, knots = NULL, boundary_knots = NULL, diag_only = FALSE) {
  ## Evaluate likelihood for starting values of theta
  # errs <- y - X * beta
  if (!is.null(knots) && !is.null(boundary_knots)) {
    init_res <- lklhd(beta, gamma, X, y, t, delta, knots = knots, boundary_knots = boundary_knots, diag_only = diag_only)
  } else {
    init_res <- lklhd(beta, gamma, X, y, t, delta, K = K, diag_only = diag_only)
  }

  ## initialize values
  converge <- TRUE
  delta_vec <- rep(1, length(c(beta, gamma)))
  iters <- 0
  theta <- c(beta, gamma)
  theta_new <- theta
  new_res <- init_res
  err_occurred <- FALSE
  step_halving_start <- 0
  should_end <- FALSE
  p <- length(beta)
  p_gamma <- length(gamma)

  while(sum(abs(delta_vec) > EPS) >= 1){
    iters <- iters + 1
    # print(iters)
    theta_old <- theta_new
    lnlklhd_old <- new_res$lnlklhd
    delta_vec <- tryCatch({
      solve(new_res$inform) %*% new_res$score
    },
    error = function(e) {
      print(e)
      err_occurred <<- TRUE
      rep(0, length(c(beta, gamma)))
    })
    theta_new <- theta_old + delta_vec
    # errs <- y - X * theta_new[1:length(beta)]
    new_res <- tryCatch({
      if (!is.null(knots) && !is.null(boundary_knots)) {
        tmp <- lklhd(theta_new[1:p], theta_new[(p+1):(p+p_gamma)], X, y, t, delta, knots = knots, boundary_knots = boundary_knots, diag_only = diag_only)
      } else {
        tmp <- lklhd(theta_new[1:p], theta_new[(p+1):(p+p_gamma)], X, y, t, delta, K = K, diag_only = diag_only)
      }
      tmp
    },
    error = function(e) {
      err_occurred <<- TRUE
      print(e)
      new_res
    })
    lnlklhd_new <- new_res$lnlklhd
    step_size <- 1
    while (is.nan(lnlklhd_new) | lnlklhd_new - lnlklhd_old <= -EPS) {
      # print("Step Halving!")
      if (verbose) cat("Performing Step Halving (new log-likelihood", round(lnlklhd_new, 6), ")\n")
      step_size <- step_size / 2
      theta_new <- theta_old + delta_vec * step_size
      ## if (abs(step_halving_start - lnlklhd_new) <= EPS) break
      if (!is.null(knots) && !is.null(boundary_knots)) {
        new_res <- lklhd(theta_new[1:p], theta_new[(p+1):(p+p_gamma)], X, y, t, delta, knots = knots, boundary_knots = boundary_knots, diag_only = diag_only)
      } else {
        new_res <- lklhd(theta_new[1:p], theta_new[(p+1):(p+p_gamma)], X, y, t, delta, K = K, diag_only = diag_only)
      }
      lnlklhd_new <- new_res$lnlklhd
    }

    if (abs(step_halving_start - lnlklhd_new) <= EPS) {
      warning("Stopping after step halving failed!")
      converge <- FALSE
      break
    }

    step_halving_start <- lnlklhd_new

    if (verbose == TRUE) {
      print(paste0("Iteration: ", iters, "; Theta Vector: ", stringr::str_c(theta_new, collapse = ", ")))
      print("Log Likelihood:")
      print(new_res$lnlklhd)
      print("Score Vector: ")
      print(new_res$score)
      print("Fisher's Information Matrix:")
      print(new_res$inform)
      print("Delta Vector")
      print(delta_vec)
    }
    if (iters >= iter_max) {
      warning("Stopping after reaching the maximum number of iterations!")
      converge <- FALSE
      break
    }
  }
  if (err_occurred) converge <- FALSE

  if (!is.null(knots) && !is.null(boundary_knots)) {
    new_res <- lklhd(theta_new[1:p], theta_new[(p+1):(p+p_gamma)], X, y, t, delta, knots = knots, boundary_knots = boundary_knots, diag_only = FALSE)
  } else {
    new_res <- lklhd(theta_new[1:p], theta_new[(p+1):(p+p_gamma)], X, y, t, delta, K = K, diag_only = FALSE)
  }

  # final_inform <- as.matrix(Matrix::nearPD(new_res$inform)$mat)
  final_inform <- new_res$inform
  # solve(final_inform)[1:2, 1:2] %>% diag() %>% sqrt()
  return(
    list(
      theta0 = theta,
      lnlklhd0 = init_res$lnlklhd,
      converge = converge,
      nbriter = iters,
      theta = as.vector(theta_new),
      lnlklhd = new_res$lnlklhd,
      score = new_res$score,
      inform = final_inform,
      err_occurred = err_occurred,
      knots = new_res$basis$knots,
      boundary_knots = new_res$basis$boundary_knots
    )
  )
}
