## Sim results analysis

devtools::load_all()
library(dplyr)
library(ggplot2)

#### Simulation 1 ----

# Standard normal error, single beta
sim_1 <- readRDS("inst/sim_res/2024-04-01_beta_1.rds")

sim_1_df <- sim_res_to_df(sim_1)

sim_1_df_clean <- sim_1_df %>%
  filter(norm_score < 100)

mean(sim_1_df$norm_rc_score)
mean(sim_1_df$norm_score)
median(sim_1_df$norm_rc_score)
median(sim_1_df$norm_score)
mean(sim_1_df$beta_var > 0)

mean(sim_1_df$pct_truncated)
mean(sim_1_df$pct_censored)

mean(sim_1_df$naive_beta)
sd(sim_1_df$naive_beta)
mean(sim_1_df$rc_beta)
sd(sim_1_df$rc_beta)
mean(sim_1_df$beta)
sd(sim_1_df$beta)

mean(sim_1_df_clean$beta)
sd(sim_1_df_clean$beta)

mean(sim_1_df$pct_truncated)
mean(sim_1_df$pct_censored)

mean(sim_1_df$norm_score < 100)

mean((sim_1_df_clean$beta + 1.96 * sqrt(sim_1_df_clean$beta_var) > 1 ) & (sim_1_df_clean$beta - 1.96 * sqrt(sim_1_df_clean$beta_var) < 1 ), na.rm = TRUE)

sim_1_df_clean %>%
  ggplot() +
  aes(x = rc_beta) +
  geom_density() +
  theme_bw() +
  xlab("Beta Estimate") +
  ylab("Probability Density")

sim_1_df_clean %>%
  ggplot() +
  aes(x = beta) +
  geom_density() +
  theme_bw() +
  xlab("Beta Estimate") +
  ylab("Probability Density") +
  geom_vline(
    xintercept = mean(sim_1_df$beta),
    color = "darkgreen"
  ) +
  geom_vline(
    xintercept = 1,
    lty = "dashed",
    color = "darkblue"
  ) +
  geom_vline(
    xintercept = mean(sim_1_df$naive_beta),
    lty = "dashed",
    color = "red"
  ) +
  annotate(
    geom = "segment",
    x = 0.5, y = 4,
    xend = .81, yend = 3
  ) +
  annotate(
    geom = "label",
    x = 0.5, y = 4,
    label = "Naive Beta Average\n0.819"
  ) +
  annotate(
    geom = "segment",
    x = 1.3, y = 4,
    xend = 1.02, yend = 3
  ) +
  annotate(
    geom = "label",
    x = 1.3, y = 4,
    label = "Our Estimate Average\n1.011"
  )

sim_1_df_clean %>%
  select(naive_beta, rc_beta, beta) %>%
  tidyr::pivot_longer(cols = everything(), names_to = "type", values_to = "estimate") %>%
  mutate(
    type = case_when(
      type == "naive_beta" ~ "Naive",
      type == "rc_beta" ~ "Right-Censored",
      TRUE ~ "LTRC"
    ),
    type = factor(type, levels = c("Naive", "Right-Censored", "LTRC"))
  ) %>%
  ggplot() +
  aes(x = estimate, fill = type) +
  geom_density(alpha = 0.8) +
  theme_bw() +
  xlab("Beta Estimate") +
  ylab("Probability Density") +
  labs(fill = "Method") +
  scale_fill_manual(values = c("darkred", "gold", "darkgreen"))

mean(sim_1_df$norm_score)
median(sim_1_df$norm_score)

sim_1_df %>%
  filter(norm_score < 1000) %>%
  ggplot() +
  aes(x = beta, y = beta_score) +
  geom_point() +
  theme_bw() +
  xlab("Beta Estimate") +
  ylab("Beta Component of Score Vector")

sim_1_df %>%
  ggplot() +
  aes(x = log(norm_score), y = log(norm_rc_score)) +
  geom_point() +
  theme_bw() +
  xlab("Log(Norm of the Score)") +
  ylab("Log(Norm of the RC Score)")

## Plots of the hazard

x <- seq(-3, 3, by = 0.01)
plot(x, rep(0, length(x)), type = "l", ylim = c(0, 10))
for (i in 1:100) {
  y <- predict_hazard(ltrc_obj = sim_1[[i]], vals = x)
  lines(x, y, col = "red")
}
lines(x, true_hazard(x), col = "black", lty = "dashed")

## get avg hazard:
haz_mat <- matrix(NA, nrow = length(sim_1), ncol = length(x))
for (i in 1:length(sim_1)) {
  y <- predict_hazard(ltrc_obj = sim_1[[i]], vals = x, within_bounds = TRUE)
  haz_mat[i,] <- y
}
new_y <- colSums(haz_mat)

lines(x, y, col = "blue")

ggplot() +
  geom_line(
    aes(x = x, y = true_hazard(x)),
    color = "black",
    lty = "dashed"
  ) +
  geom_line(
    aes(x = x, y = y),
    color = "blue"
  ) +
  theme_bw() +
  xlab("Epsilon") +
  ylab("Hazard Function") +
  ggtitle("Average Estimated Hazard Function", "Estimated (blue) vs. Truth (black)")

data.frame(
  beta = c(sim_1_df$beta, sim_2_df$beta, sim_3_df$beta, sim_4_df$beta),
  Error = c(rep(1, nrow(sim_1_df)), rep(2, nrow(sim_2_df)), rep(3, nrow(sim_3_df)), rep(4, nrow(sim_4_df)))
) %>%
ggplot() +
  aes(x = beta) +
  geom_histogram(binwidth = 0.05)+
  theme_bw() +
  facet_wrap(~Error, nrow = 2, labeller = "label_both") +
  xlab("Beta Estimate") +
  ylab("Count")

#### Simulation 2 ----

# Standard normal error, single beta
sim_2 <- readRDS("inst/sim_res/2024-04-02_beta_1_unif_error.rds")

sim_2_df <- sim_res_to_df(sim_2)

sim_2_df_clean <- sim_2_df %>%
  filter(beta_var > 0)

mean(sim_2_df$norm_rc_score)
mean(sim_2_df$norm_score)
median(sim_2_df$norm_rc_score)
median(sim_2_df$norm_score)
mean(sim_2_df$beta_var > 0)

mean(sim_2_df$pct_truncated)
mean(sim_2_df$pct_censored)

mean(sim_2_df$naive_beta)
sd(sim_2_df$naive_beta)
mean(sim_2_df$rc_beta)
sd(sim_2_df$rc_beta)
mean(sim_2_df$beta)
sd(sim_2_df$beta)

mean(sim_2_df_clean$beta)
sd(sim_2_df_clean$beta)

mean(sim_2_df$norm_score < 100)

mean((sim_2_df_clean$beta + 1.96 * sqrt(sim_2_df_clean$beta_var) > 1 ) & (sim_2_df_clean$beta - 1.96 * sqrt(sim_2_df_clean$beta_var) < 1 ), na.rm = TRUE)

mean(sim_2_df$beta_var > 0)

sim_4_df_clean %>%
  select(naive_beta, rc_beta, beta) %>%
  tidyr::pivot_longer(cols = everything(), names_to = "type", values_to = "estimate") %>%
  mutate(
    type = case_when(
      type == "naive_beta" ~ "Naive",
      type == "rc_beta" ~ "Right-Censored",
      TRUE ~ "LTRC"
    ),
    type = factor(type, levels = c("Naive", "Right-Censored", "LTRC"))
  ) %>%
  ggplot() +
  aes(x = estimate, fill = type) +
  geom_density(alpha = 0.8) +
  theme_bw() +
  xlab("Beta Estimate") +
  ylab("Probability Density") +
  labs(fill = "Method") +
  scale_fill_manual(values = c("darkred", "gold", "darkgreen"))

#### Simulation 3 ----
sim_3 <- readRDS("inst/sim_res/2024-04-03_beta_1_ev_error.rds")

sim_3_df <- sim_res_to_df(sim_3)

sim_3_df_clean <- sim_3_df %>%
  filter(norm_score < 50)

mean(sim_3_df$norm_rc_score)
mean(sim_3_df$norm_score)
median(sim_3_df$norm_rc_score)
median(sim_3_df$norm_score)
mean(sim_3_df$beta_var > 0)

mean(sim_3_df$pct_truncated)
mean(sim_3_df$pct_censored)

mean(sim_3_df$naive_beta)
sd(sim_3_df$naive_beta)
mean(sim_3_df$rc_beta)
sd(sim_3_df$rc_beta)
mean(sim_3_df$beta)
sd(sim_3_df$beta)

mean(sim_3_df_clean$beta)
sd(sim_3_df_clean$beta)

mean(sim_3_df$norm_score < 100)

mean((sim_3_df_clean$beta + 1.96 * sqrt(sim_3_df_clean$beta_var) > 1 ) & (sim_3_df_clean$beta - 1.96 * sqrt(sim_3_df_clean$beta_var) < 1 ), na.rm = TRUE)

mean(sim_3_df$beta_var > 0)


#### Simulation 4 ----
sim_4 <- readRDS("inst/sim_res/2024-04-01_beta_1_mixed_error.rds")

sim_4_df <- sim_res_to_df(sim_4)

sim_4_df_clean <- sim_4_df %>%
  filter(beta_var > 0)

mean(sim_4_df$norm_rc_score)
mean(sim_4_df$norm_score)
median(sim_4_df$norm_rc_score)
median(sim_4_df$norm_score)
mean(sim_4_df$beta_var > 0)

mean(sim_4_df$pct_truncated)
mean(sim_4_df$pct_censored)

mean(sim_4_df$naive_beta)
sd(sim_4_df$naive_beta)
mean(sim_4_df$rc_beta)
sd(sim_4_df$rc_beta)
mean(sim_4_df$beta)
sd(sim_4_df$beta)

mean(sim_4_df_clean$beta)
sd(sim_4_df_clean$beta)

mean(sim_4_df$norm_score < 100)

mean((sim_4_df_clean$beta + 1.96 * sqrt(sim_4_df_clean$beta_var) > 1 ) & (sim_4_df_clean$beta - 1.96 * sqrt(sim_4_df_clean$beta_var) < 1 ), na.rm = TRUE)

mean(sim_4_df$beta_var > 0)
