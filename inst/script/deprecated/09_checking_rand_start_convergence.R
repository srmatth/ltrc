devtools::load_all()
library(dplyr)
library(ggplot2)


res <- readRDS("inst/ltrc_sim_res_FINAL_FINAL/non_centered_mixed_normal_800.rds")

start_end_df <- data.frame()
for (i in res) {
  tmp_dat <- t(i$all_starts) %>%
    as.data.frame() %>%
    magrittr::set_colnames(c("beta_1_start", "beta_2_start"))
  res_vals <- data.frame()
  for (j in i$all_res) {
    res_vals = rbind(res_vals, data.frame("beta_1" = j$theta[1], "beta_2" = j$theta[2]))
  }
  start_end_df <- rbind(start_end_df, cbind(tmp_dat, res_vals))
}

beta_1 <- start_end_df$beta_1
mean(beta_1 > 0 & beta_1 < 2)

beta_2 <- start_end_df$beta_2
mean(beta_2 > 0 & beta_2 < 2)

