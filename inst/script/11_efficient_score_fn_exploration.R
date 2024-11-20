
library(ggplot2)


true_beta <- matrix(c(1, 1), ncol = 1)
error_dist <- rnorm
n <- 400
FINAL_KNOTS <- 1
sd_multiplier <- 3
verbose <- FALSE
diag_only <- TRUE
return_all <- FALSE
n_start <- 10

x <- matrix(runif(length(true_beta)*2*n, -3, 3), ncol = length(true_beta))
# # create binary covariate
x[,2] <- as.numeric(x[,2] > 0)

eps <- error_dist(2*n)
y <- x %*% true_beta + eps

## Induce left-truncation into our data
t <- runif(2*n, -6, 1)
obs_indx <- which(y > t)
sampled_indx <- sample(obs_indx, size = n)
## Censoring
c <- runif(2*n, 1, 7)
delta <- as.numeric(y < c)
y_cens <- ifelse(y < c, y, c)
## Getting observed Data
y_obs <- y_cens[sampled_indx]
x_obs <- matrix(x[sampled_indx,], ncol = length(true_beta))
t_obs <- t[sampled_indx]
delta_obs <- delta[sampled_indx]

dat <- data.frame(
  y_obs, delta_obs, x_obs
)

res <- ltrc(formula = survival::Surv(y_obs, delta_obs) ~ x_obs, t_obs, data = dat, n_start = n_start, int_knots = FINAL_KNOTS,
            sd_multiplier = sd_multiplier, verbose = verbose, diag_only = diag_only, return_all = return_all)

## We are going to compute it two different ways for the two different equations

p <- length(true_beta)
est_beta <- matrix(res$theta[1:p], nrow = p)
epsilon <- y_obs - x_obs %*% est_beta
tau <- t_obs - x_obs %*% est_beta

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
  m_s(epsilon_sorted[1], d_i, t_i, e_i)
  mart_vals <- m_s(epsilon_sorted, d_i, t_i, e_i)

  # ggplot() +
  #   aes(x = epsilon_sorted, y = mart_vals) +
  #   geom_step() +
  #   theme_bw() +
  #   xlab("s") +
  #   ylab("M(s)")
  dms <- c(diff(mart_vals), 0)
  uni_dimensional_part <- dms * (-g_dot_sorted)
  multi_dimensional_part <- matrix(rep(x_i, length(epsilon)), ncol = p, byrow = TRUE) - conditional_means_sorted
  contribs_m[i,] <- t(uni_dimensional_part) %*% multi_dimensional_part
}

efficient_score <- colSums(contribs_m)

info <- matrix(0, ncol = p, nrow = p)
for (i in 1:nrow(contribs_m)) {
  info <- info + (contribs_m[i,] %*% t(contribs_m[i,]))
}
solve(info) %>% diag() %>% sqrt()

efficient_score_info <- (efficient_score %*% t(efficient_score))
inv <- (efficient_score_info / sum(efficient_score^2))
efficient_score_info

((efficient_score_info / sum(efficient_score^2)) / length(epsilon)) %>% diag() %>% sqrt()

norm_a_squared <- sum(efficient_score^2)
inverse_aaT <-  (1 / norm_a_squared^2) * efficient_score_info

efficient_score_info %*% inverse_aaT %*% efficient_score_info

(efficient_score_info / length(epsilon)) %>% diag() %>% sqrt()

(solve(Matrix::nearPD(efficient_score_info)$mat) / length(epsilon)) %>% diag() %>% sqrt()

## Martingale Representation 2 ----
# contribs_m2 <- matrix(nrow = length(epsilon), ncol = p)
# for (i in 1:length(epsilon)) {
#   e_i <- epsilon[i]
#   t_i <- tau[i]
#   d_i <- delta_obs[i]
#   x_i <- x_obs[i,]
#
#   uni_dimensional_part <- delta_sorted * (-g_dot_sorted)
#   multi_dimensional_part <- x_sorted - conditional_means_sorted
#   contribs_m2[i,] <- t(uni_dimensional_part) %*% multi_dimensional_part
# }
#
# efficient_score <- t(uni_dimensional_part) %*% multi_dimensional_part
# efficient_score_info <- t(efficient_score) %*% (efficient_score)



## Score Representation ----

contribs <- matrix(nrow = length(epsilon), ncol = p)
for (i in 1:length(epsilon)) {
  e_i <- epsilon[i]
  t_i <- tau[i]
  d_i <- delta_obs[i]
  x_i <- x_obs[i,]

  first_term <- d_i * (-g_dot[i]) * (x_i - conditional_means[i,])

  indicator_vec <- as.numeric((e_i >= epsilon_sorted) & (t_i <= epsilon_sorted))
  uni_dimensional_part <- indicator_vec * (-g_dot_sorted) * exp(log_haz_sorted) * diffs_sorted
  multi_dimensional_part <- matrix(rep(x_i, length(epsilon)), ncol = p, byrow = TRUE) - conditional_means_sorted

  second_term <- t(uni_dimensional_part) %*% multi_dimensional_part

  contribs[i,] <- first_term - second_term
}

efficient_score <- colSums(contribs)
efficient_score_info <- (efficient_score %*% t(efficient_score))

(efficient_score_info / length(epsilon)) %>% diag() %>% sqrt()

(solve(Matrix::nearPD(efficient_score_info)$mat) / length(epsilon)) %>% diag() %>% sqrt()


## Last try for the night
p <- length(true_beta)
est_beta <- matrix(res$theta[1:p], nrow = p)
epsilon <- y_obs - x_obs %*% est_beta
tau <- t_obs - x_obs %*% est_beta

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

for (i in 1:length(epsilon)) {
  e_i <- epsilon[i]
  t_i <- tau[i]
  d_i <- delta_obs[i]
  x_i <- x_obs[i,]

  l_b_1 <- d_i * g_dot[i]
  indx_l_b_2 <- which((epsilon >= t_i) & (epsilon <= e_i))
}



# p <- length(true_beta)
# est_beta <- matrix(res$theta[1:length(true_beta)], nrow = p)
# epsilon <- y_obs - x_obs %*% est_beta
# tau <- t_obs - x_obs %*% est_beta
# s_values <- sort(c(epsilon, tau))
# widths <- diff(s_values)
# log_haz <- predict_log_hazard(
#   x = s_values,
#   gamma = res$theta[(p+1):length(res$theta)], knots = res$knots,
#   boundary_knots = res$boundary_knots, deriv = 0
# )
# g_dot <- predict_log_hazard(
#   x = s_values,
#   gamma = res$theta[(p+1):length(res$theta)], knots = res$knots,
#   boundary_knots = res$boundary_knots, deriv = 1
# )
#
#
#
# efficient_score <- colSums(contribs)
# # (efficient_score^(-2) / length(epsilon)) %>% sqrt()
# efficient_score_info <- (efficient_score %*% t(efficient_score))
#
# (solve(Matrix::nearPD(efficient_score_info)$mat) / length(epsilon)) %>% diag() %>% sqrt()



# p <- length(true_beta)
# beta_est <- res$theta[1:p]
#
# epsilon_vec <- y_obs - x_obs %*% beta_est
# tau_vec <- t_obs - x_obs %*% beta_est
# g_hat <- function(s) {
#   predict_log_hazard(
#     x = s,
#     gamma = res$theta[(p+1):length(res$theta)], knots = res$knots,
#     boundary_knots = res$boundary_knots, deriv = 0
#   )
# }
# d_g_hat <- function(s) {
#   predict_log_hazard(
#     x = s,
#     gamma = res$theta[(p+1):length(res$theta)], knots = res$knots,
#     boundary_knots = res$boundary_knots, deriv = 1
#   )
# }
#
# # Ms_i <- function(s, e_, t_, d_, partition, g) {
# #   first <- d_ * as.numeric(e_ <= s)
# #   indicator <- (partition >= t_) & (partition <= s)
# #   widths <- c(diff(partition), 0)
# #   exp_g_vals <- exp(g(partition))
# #   second <- indicator * widths * as.numeric(partition <= e_) * exp_g_vals
# #   first - sum(second)
# # }
#
# s_values <- sort(c(tau_vec, epsilon_vec))
#
# conditional_means <- matrix(NA, nrow = length(s_values), ncol = p)
# for (i in 1:length(s_values)) {
#   s <- s_values[i]
#   conditional_means[i,] <- colMeans(matrix(x_obs[(s <= epsilon_vec) & (s >= tau_vec),], ncol = p))
# }
#
# widths <- diff(s_values)
# log_haz <- g_hat(s_values)
# ms <- function(s, t_, e, delta) {
#   res <- numeric(length(s))
#   for (j in 1:length(s)) {
#     k <- s[j]
#     first <- delta * as.numeric(e <= k)
#     upper_start <- e
#     if (t_ > k) {
#       res[j] <- 0
#       next
#     } else if (upper_start > k) upper_start <- k
#
#     indx <- as.numeric(s_values >= t_ & s_values <= upper_start)
#     second <- sum(widths * exp(log_haz)[-1] * indx[-1])
#     res[j] <- first - second
#   }
#   return(res)
# }
#
# second_part <- -d_g_hat(s_values)
# contribs <- matrix(nrow = length(epsilon_vec), ncol = p)
# for (i in 1:length(epsilon_vec)) {
#   # print(i)
#   e_i <- epsilon_vec[i]
#   t_i <- tau_vec[i]
#   x_i <- x_obs[i,]
#   d_i <- delta_obs[i]
#   x_i_mat <- matrix(rep(x_i, length(s_values)), ncol = p, byrow = TRUE)
#
#   first_part <- x_i_mat - conditional_means
#
#   y <- ms(s_values, t_i, e_i, d_i)
#   dms <- c(diff(y), 0)
#
#   contribs[i,] <- colSums(matrix(rep(dms * second_part, p), ncol = p) * first_part)
# }
#
# efficient_score <- colSums(contribs)
# info <- (efficient_score %*% t(efficient_score)) / length(epsilon_vec)
# solve(Matrix::nearPD(info)$mat)
#
#
#
#
#
#
#
