## Simulation results ----

library(dplyr)
library(readr)
library(ggplot2)


sim_dat <- read_csv("inst/updated_sim_res/ltrc_new_new_expanded_sim_res_output.csv")


results_table <- sim_dat %>%
  filter(error != "a") %>%
  filter(sample_size > 150) %>%
  filter(converged == TRUE, errored == FALSE) %>%
  filter(var_2 > 0) %>%
  filter(var_1 < 100, var_2 < 1) %>%
  filter(!is.na(var_2)) %>%
  # filter(abs(bias) < 3*sqrt(var_1) & abs(bias) < 3*sqrt(var_2)) %>%
  # filter(abs(bias) < 2) %>%
  group_by(
    error,
    sample_size,
    censoring_prob,
    parameter
  ) %>%
  summarize(
    n = n(),
    # avg_censoring = mean(actual_pct_censored),
    # avg_truncation = mean(trunc_prob),
    avg_bias = mean(bias),
    var_1 = mean(var_1),
    var_2 = mean(var_2),
    var_3 = var(bias),
    coverage = mean(truth_in_ci_1, na.rm = TRUE)*100
  )

results_table %>%
  select(-n) %>%
  filter(sample_size > 150) %>%
  filter(error != "a") %>%
  mutate(across(c("avg_bias", "var_1", "var_2", "var_3"), function(x){round(x*1000, 1)})) %>%
  tidyr::pivot_wider(
    names_from = "parameter",
    values_from = c("avg_bias", "var_1", "var_2", "var_3", "coverage")
  ) %>%
  xtable::xtable(digits = 1)


results_table %>%
  select(-n, -var_2, -var_3, -coverage) %>%
  filter(sample_size %in% c(100, 200)) %>%
  filter(error %in% c("a" ,"b")) %>%
  arrange(desc(error)) %>%
  mutate(across(c("avg_bias", "var_1"), function(x){round(x*1000, 1)})) %>%
  tidyr::pivot_wider(
    names_from = "parameter",
    values_from = c("avg_bias", "var_1")
  ) %>%
  select(
    error,
    sample_size,
    censoring_prob,
    avg_bias_beta_1,
    var_1_beta_1,
    avg_bias_beta_2,
    var_1_beta_2
  ) %>%
  mutate(test = NA) %>%
  xtable::xtable(digits = 1)

c_and_t <- sim_dat %>%
  filter(error != "a") %>%
  filter(sample_size > 150) %>%
  filter(converged == TRUE, errored == FALSE) %>%
  filter(var_2 > 0) %>%
  filter(var_1 < 100, var_2 < 1) %>%
  filter(!is.na(var_2)) %>%
  mutate(
    obs_n = (1 - trunc_prob) * 2 * sample_size,
    trunc_prob = 1 - obs_n / (10*sample_size)
  ) %>%
  group_by(
    error,
    sample_size,
    censoring_prob,
    parameter
  ) %>%
  summarize(
    n = n(),
    # avg_censoring = mean(actual_pct_censored),
    # avg_truncation = mean(trunc_prob),
    avg_trunc = mean(trunc_prob),
    min_trunc = min(trunc_prob),
    max_trunc = max(trunc_prob),
    avg_cens = mean(actual_pct_censored),
    min_cens = min(actual_pct_censored),
    max_cens = max(actual_pct_censored)
  )


