## Load the libraries

library(extRemes)
library(furrr)
library(future)
library(ltrc)
library(readr)

N <- 10 # 5000
true_beta <- c(1, 1)
FINAL_KNOTS <- 4
n_workers <- 1 # 58
n_start <- 5

## Standard Normal Error ----
cat("Working on Standard Normal Error, 200 Observations\n")
plan(multisession, workers = n_workers)
res <- furrr::future_map(
  1:N,
  sim_iteration,
  FINAL_KNOTS = FINAL_KNOTS,
  true_beta = true_beta,
  n = 200,
  error_dist = rnorm,
  sd_multiplier = 3,
  n_start = n_start
)
write_rds(res, "ltrc_sim_res/standard_normal_200.rds")

cat("Working on Standard Normal Error, 400 Observations\n")
plan(multisession, workers = n_workers)
res <- furrr::future_map(
  1:N,
  sim_iteration,
  FINAL_KNOTS = FINAL_KNOTS,
  true_beta = true_beta,
  n = 400,
  error_dist = rnorm,
  sd_multiplier = 3,
  n_start = n_start
)
write_rds(res, "ltrc_sim_res/standard_normal_400.rds")

cat("Working on Standard Normal Error, 800 Observations\n")
plan(multisession, workers = n_workers)
res <- furrr::future_map(
  1:N,
  sim_iteration,
  FINAL_KNOTS = FINAL_KNOTS,
  true_beta = true_beta,
  n = 800,
  error_dist = rnorm,
  sd_multiplier = 3,
  n_start = n_start
)
write_rds(res, "ltrc_sim_res/standard_normal_800.rds")


## Standard extreme value error ----
cat("Working on Standard Extreme Value Error, 200 Observations\n")
plan(multisession, workers = n_workers)
res <- furrr::future_map(
  1:N,
  sim_iteration,
  FINAL_KNOTS = FINAL_KNOTS,
  true_beta = true_beta,
  n = 200,
  error_dist = revd,
  sd_multiplier = 3,
  n_start = n_start
)
write_rds(res, "ltrc_sim_res/standard_ev_200.rds")

cat("Working on Standard Extreme Value Error, 400 Observations\n")
plan(multisession, workers = n_workers)
res <- furrr::future_map(
  1:N,
  sim_iteration,
  FINAL_KNOTS = FINAL_KNOTS,
  true_beta = true_beta,
  n = 400,
  error_dist = revd,
  sd_multiplier = 3,
  n_start = n_start
)
write_rds(res, "ltrc_sim_res/standard_ev_400.rds")

cat("Working on Standard Extreme Value Error, 800 Observations\n")
plan(multisession, workers = n_workers)
res <- furrr::future_map(
  1:N,
  sim_iteration,
  FINAL_KNOTS = FINAL_KNOTS,
  true_beta = true_beta,
  n = 800,
  error_dist = revd,
  sd_multiplier = 3,
  n_start = n_start
)
write_rds(res, "ltrc_sim_res/standard_ev_800.rds")

## Mixed normal distribution error (zero-centered) ----
error_dist <- function(n) {
  coin_flip <- runif(n)
  ep1 <- rnorm(n)
  ep2 <- rnorm(n, 0, 3)
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2)
  return(err_vec)
}

cat("Working on Centered Mixed Normal Error, 200 Observations\n")
plan(multisession, workers = n_workers)
res <- furrr::future_map(
  1:N,
  sim_iteration,
  FINAL_KNOTS = FINAL_KNOTS,
  true_beta = true_beta,
  n = 200,
  error_dist = error_dist,
  sd_multiplier = 3,
  n_start = n_start
)
write_rds(res, "ltrc_sim_res/centered_mixed_normal_200.rds")

cat("Working on Centered Mixed Normal Error, 400 Observations\n")
plan(multisession, workers = n_workers)
res <- furrr::future_map(
  1:N,
  sim_iteration,
  FINAL_KNOTS = FINAL_KNOTS,
  true_beta = true_beta,
  n = 400,
  error_dist = error_dist,
  sd_multiplier = 3,
  n_start = n_start
)
write_rds(res, "ltrc_sim_res/centered_mixed_normal_400.rds")

cat("Working on Centered Mixed Normal Error, 800 Observations\n")
plan(multisession, workers = n_workers)
res <- furrr::future_map(
  1:N,
  sim_iteration,
  FINAL_KNOTS = FINAL_KNOTS,
  true_beta = true_beta,
  n = 800,
  error_dist = error_dist,
  sd_multiplier = 3,
  n_start = n_start
)
write_rds(res, "ltrc_sim_res/centered_mixed_normal_800.rds")

## Mixed normal distribution error (non-zero-centered) ----
error_dist <- function(n) {
  coin_flip <- runif(n)
  ep1 <- rnorm(n)
  ep2 <- rnorm(n, -1, 0.5)
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2)
  return(err_vec)
}

cat("Working on Non-Centered Mixed Normal Error, 200 Observations\n")
plan(multisession, workers = n_workers)
res <- furrr::future_map(
  1:N,
  sim_iteration,
  FINAL_KNOTS = FINAL_KNOTS,
  true_beta = true_beta,
  n = 200,
  error_dist = error_dist,
  sd_multiplier = 3,
  n_start = n_start
)
write_rds(res, "ltrc_sim_res/non_centered_mixed_normal_200.rds")

cat("Working on Non-Centered Mixed Normal Error, 400 Observations\n")
plan(multisession, workers = n_workers)
res <- furrr::future_map(
  1:N,
  sim_iteration,
  FINAL_KNOTS = FINAL_KNOTS,
  true_beta = true_beta,
  n = 400,
  error_dist = error_dist,
  sd_multiplier = 3,
  n_start = n_start
)
write_rds(res, "ltrc_sim_res/non_centered_mixed_normal_400.rds")

cat("Working on Non-Centered Mixed Normal Error, 800 Observations\n")
plan(multisession, workers = n_workers)
res <- furrr::future_map(
  1:N,
  sim_iteration,
  FINAL_KNOTS = FINAL_KNOTS,
  true_beta = true_beta,
  n = 800,
  error_dist = error_dist,
  sd_multiplier = 3,
  n_start = n_start
)
write_rds(res, "ltrc_sim_res/non_centered_mixed_normal_800.rds")


cat("All Done!\n")

q(save = "no")
