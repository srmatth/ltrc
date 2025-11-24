devtools::load_all()

# set.seed(4472)
true_beta <- c(1, 1)
N <- 500
p <- length(true_beta)
x <- matrix(runif(N*p, -3, 3), ncol = p)
eps <- rnorm(N)
y <- x %*% matrix(true_beta, ncol = 1) + eps

## Induce left-truncation into our data

t <- runif(N, -6, 0)
obs_indx <- which(y > t)

c <- runif(N, 1, 6)
delta <- as.numeric(y < c)
y_cens <- ifelse(y < c, y, c)

y_obs <- y_cens[obs_indx]
x_obs <- x[obs_indx,]
t_obs <- t[obs_indx] # comment this if only doing right-censored model
delta_obs <- delta[obs_indx]

lin_mod <- lm(y_obs ~ x_obs)
beta <- lin_mod$coefficients[-1]
gamma <- rep(0, 10)

dat <- data.frame(
  y_obs, delta_obs, x_obs
)

tictoc::tic()
res = ltrc(formula = survival::Surv(y_obs, delta_obs) ~ x_obs, t_obs, data = dat, n_start = 5, int_knots = 1, verbose = TRUE, return_all = TRUE)
#print(res)
tictoc::toc()

diag(solve(res$inform))

diag(solve(res$inform)) %>% sqrt()

all(eigen(res$inform)$values > 0)






res$all_starts[1,]
ends <- numeric()
ends_beta_2 <- numeric()
lklhds <- numeric()
pd <- numeric()
for (i in 1:length(res$all_starts[1,])) {
  print(i)
  ends[i] <- res$all_res[[i]]$theta[1]
  ends_beta_2 <- res$all_res[[i]]$theta[2]
  lklhds[i] <- res$all_res[[i]]$lnlklhd
  print(eigen(res$all_res[[i]]$inform)$values)
  pd[i] <- all(diag(solve(res$all_res[[i]]$inform))[1:2] > 0)
  print(diag(solve(res$all_res[[i]]$inform)))
}

converge_df <- data.frame(
  num = 1:length(ends),
  starts = res$all_starts[1,],
  ends = ends,
  starts_beta_2 = res$all_starts[2,],
  ends_beta_2 = ends_beta_2,
  lnlklhds = lklhds
)

converge_df %>%
  mutate(good_est = ifelse(round(ends) == 2, 1, 0)) %>%
  ggplot() +
  aes(color = as.factor(good_est)) +
  geom_vline(
    xintercept = 2,
    lty = "dashed",
    color = "blue"
  ) +
  geom_point(
    aes(x = starts, y = num),
    size = 3
  ) +
  geom_segment(
    aes(x = starts, y = num, xend = ends, yend = num),
    arrow = arrow(length = unit(0.1, "inches"), type = "closed"),
    size = 1
  ) +
  geom_label(
    aes(x = 20, y = num, label = paste("Likelihood:", round(lnlklhds))),
    hjust = 0.9
  ) +
  scale_color_manual(values = c("darkred", "darkgreen")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab("Beta 1 Value") +
  ylab("Start Number")

converge_df %>%
  mutate(good_est = ifelse(round(ends_beta_2) == -1, 1, 0)) %>%
  ggplot() +
  aes(color = as.factor(good_est)) +
  geom_vline(
    xintercept = -1,
    lty = "dashed",
    color = "blue"
  ) +
  geom_point(
    aes(x = starts_beta_2, y = num),
    size = 3
  ) +
  geom_segment(
    aes(x = starts_beta_2, y = num, xend = ends_beta_2, yend = num),
    arrow = arrow(length = unit(0.1, "inches"), type = "closed"),
    size = 1
  ) +
  scale_color_manual(values = c("darkgreen")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab("Beta 2 Value") +
  ylab("Start Number")

