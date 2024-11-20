## Running the updated simulation
# Load necessary libraries
# devtools::load_all()
library(extRemes)
library(furrr)
library(future)
library(ltrc)

## Set Variables
N <- 30
true_beta <- c(2, -1)
n <- 500
final_res <- list()
error_dist <- function(n) {
  # coin_flip <- runif(n)
  # ep1 <- rnorm(n)
  # ep2 <- rnorm(n, 0, 3)
  # err_vec <- ifelse(coin_flip > 0.5, ep1, ep2)
  # err_vec <- runif(n, -3, 3)
  err_vec <- rnorm(n)
  # err_vec <- revd(n)
  return(err_vec)
}
FINAL_KNOTS <- 4

# Refactor the loop body into a function
process_iteration <- function(i, FINAL_KNOTS, true_beta, n, sd_multiplier = 1) {
  cat("Working on iteration ", i, "\n")
  x <- matrix(runif(length(true_beta)*2*n, -3, 3), ncol = length(true_beta))
  eps <- error_dist(2*n)
  y <- x %*% true_beta + eps

  ## Induce left-truncation into our data
  t <- runif(2*n, -6, 0)
  obs_indx <- which(y > t)
  sampled_indx <- sample(obs_indx, size = n)
  ## Censoring
  c <- runif(2*n, 0, 6)
  delta <- as.numeric(y < c)
  y_cens <- ifelse(y < c, y, c)
  ## Getting observed Data
  y_obs <- y_cens[sampled_indx]
  x_obs <- x[sampled_indx,]
  t_obs <- t[sampled_indx]
  delta_obs <- delta[sampled_indx]

  ## Get initial values
  lin_mod <- lm(y_obs ~ x_obs)
  beta <- lin_mod$coefficients[-1]
  gamma <- rnorm(6)

  dat <- data.frame(
    y_obs, delta_obs, x_obs
  )

  tictoc::tic()
  res <- ltrc:::ltrc(formula = survival::Surv(y_obs, delta_obs) ~ x_obs, t_obs, data = dat, n_start = 20, int_knots = FINAL_KNOTS,
                     sd_multiplier = sd_multiplier)
  time <- tictoc::toc()

  rc_res <- ltrc:::ltrc(formula = survival::Surv(y_obs, delta_obs) ~ x_obs, rep(-10, length(delta_obs)), data = dat, n_start = 20, int_knots = FINAL_KNOTS, sd_multiplier = sd_multiplier)

  tmp_dat <- list(
    n_obs_start = n,
    n_obs = length(obs_indx),
    pct_truncated = 1 - length(obs_indx) / (2*n),
    pct_censored = 1 - mean(delta_obs),
    true_beta = true_beta,
    naive_beta = lin_mod$coefficients[2:(length(true_beta) + 1)],
    rc_beta = rc_res$theta[1:length(true_beta)],
    rc_score = rc_res$score,
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
    basis_boundary_knots = res$boundary_knots,
    basis_internal_knots = res$knots
  )

  tmp_dat
}

# Set up parallel backend
plan(multisession, workers = 6)
# Use future_map to run iterations in parallel
results <- future_map(1:N, process_iteration, FINAL_KNOTS = 4, true_beta = true_beta, n = 500, sd_multiplier = 3, .progress = TRUE)

readr::write_rds(results, "inst/sim_res/2024-04-11_beta_2_neg1_norm.rds")



error_dist <- function(n) {
  # coin_flip <- runif(n)
  # ep1 <- rnorm(n)
  # ep2 <- rnorm(n, 0, 3)
  # err_vec <- ifelse(coin_flip > 0.5, ep1, ep2)
  # err_vec <- runif(n, -3, 3)
  # err_vec <- rnorm(n)
  err_vec <- revd(n)
  return(err_vec)
}

plan(multisession, workers = 8)
# Use future_map to run iterations in parallel
results <- future_map(1:N, process_iteration, FINAL_KNOTS = 4, true_beta = 1, n = 500, sd_multiplier = 3, .progress = TRUE)

readr::write_rds(results, "inst/sim_res/2024-04-10_beta_1_ev.rds")

error_dist <- function(n) {
  # coin_flip <- runif(n)
  # ep1 <- rnorm(n)
  # ep2 <- rnorm(n, 0, 3)
  # err_vec <- ifelse(coin_flip > 0.5, ep1, ep2)
  err_vec <- runif(n, -3, 3)
  # err_vec <- rnorm(n)
  # err_vec <- revd(n)
  return(err_vec)
}

plan(multisession, workers = 8)
# Use future_map to run iterations in parallel
results <- future_map(1:N, process_iteration, FINAL_KNOTS = 4, true_beta = 1, n = 500, sd_multiplier = 3, .progress = TRUE)

readr::write_rds(results, "inst/sim_res/2024-04-10_beta_1_unif.rds")

error_dist <- function(n) {
  coin_flip <- runif(n)
  ep1 <- rnorm(n)
  ep2 <- rnorm(n, 0, 3)
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2)
  # err_vec <- runif(n, -3, 3)
  # err_vec <- rnorm(n)
  # err_vec <- revd(n)
  return(err_vec)
}

plan(multisession, workers = 8)
# Use future_map to run iterations in parallel
results <- future_map(1:N, process_iteration, FINAL_KNOTS = 4, true_beta = 1, n = 500, sd_multiplier = 3, .progress = TRUE)

readr::write_rds(results, "inst/sim_res/2024-04-10_beta_1_mixed_norm.rds")


results <- readRDS("inst/sim_res/2024-04-10_beta_1_unif.rds")




res_df <- sim_res_to_df(results)
mean(res_df$norm_score)
median(res_df$norm_score)
mean(res_df$norm_score < 10)
mean(res_df$norm_score < 1)
mean(res_df$beta_var > 0)
mean(res_df$beta[res_df$beta_var > 0])
sd(res_df$beta[res_df$beta_var > 0])

# baseline_res <- readRDS("inst/sim_res/2024-04-01_beta_1.rds")
# baseline_df <- sim_res_to_df(baseline_res)
# mean(baseline_df$norm_score)
# median(baseline_df$norm_score)
# mean(baseline_df$norm_score < 10)
# mean(baseline_df$norm_score < 1)
# mean(baseline_df$beta_var > 0)
# mean(baseline_df$beta[res_df$beta_var > 0])
# sd(baseline_df$beta[res_df$beta_var > 0])

# knots = c(-0.8416212, -0.2533471,  0.2533471,  0.8416212), boundary_knots = c(-10, 4),
