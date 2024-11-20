devtools::load_all()
library(dplyr)
library(fs)

dir_path <- "inst/ltrc_sim_res_FINAL_FINAL/"
files <- dir_ls(dir_path)

# order <- c(4,5,6,1,2,3)
order <-  c(10,11,12,7,8,9,1,2,3,4,5,6)
files <- files[order]
vars_gt_0 <- numeric()
vars_gt_0_2 <- numeric()
i <- 1
for (f in files) {
  res <- readRDS(f)

  res_df <- sim_res_to_df(res)

  vars_gt_0[i] <- mean(res_df$var_beta_1 > 0)
  vars_gt_0_2[i] <- mean(res_df$var_beta_2 > 0)

  res_df_clean <- res_df %>%
    filter(beta_2 < 5, beta_2 > -1, var_beta_2 > 0, var_beta_1 > 0)

  p1 <- mean(res_df_clean$beta_1) - 1
  p2 <- mean(res_df_clean$beta_2) -1
  p3 <- mean(res_df_clean$var_beta_1)
  p4 <- mean(res_df_clean$var_beta_2)
  p5 <- var(res_df_clean$beta_1)
  p6 <- var(res_df_clean$beta_2)

  p7 <- mean(res_df_clean$beta_1 - 2 * sqrt(res_df_clean$var_beta_1) < 1 & res_df_clean$beta_1 + 2 * sqrt(res_df_clean$var_beta_1) > 1, na.rm = TRUE)
  p8 <- mean(res_df_clean$beta_2 - 2 * sqrt(res_df_clean$var_beta_2) < 1 & res_df_clean$beta_2 + 2 * sqrt(res_df_clean$var_beta_2) > 1, na.rm = TRUE)



  rc_df_clean <- res_df %>%
    filter(rc_beta_2 > -1, rc_beta_2 < 1)
  p9 <- mean(rc_df_clean$rc_beta_1) - 1
  p10 <- mean(rc_df_clean$rc_beta_2) - 1
  p11 <- var(rc_df_clean$rc_beta_1)
  p12 <- var(rc_df_clean$rc_beta_2)


  p13 <- mean(res_df_clean$naive_beta_1) - 1
  p14 <- mean(res_df_clean$naive_beta_2) - 1
  p15 <- var(res_df_clean$naive_beta_1)
  p16 <- var(res_df_clean$naive_beta_2)

  # cat(round(p1, 4), "&", round(p3, 4), "&", round(p5, 4), "&", round(p7, 4), "\n") #, "& &", round(p9, 4), "&", round(p11, 4), "& &", round(p13, 4), "&", round(p15, 4), "\n")
  # cat(round(p2, 4), "&", round(p4, 4), "&", round(p6, 4), "&", round(p8, 4), "\n") #, "& &", round(p10, 4), "&", round(p12, 4), "& &", round(p14, 4), "&", round(p16, 4), "\n")
  i <- i+1

  cat(round(p1, 4), "&", round(p5, 4), "& &", round(p9, 4), "&", round(p11, 4), "& &", round(p13, 4), "&", round(p15, 4), "\n")
  cat(round(p2, 4), "&", round(p6, 4), "& &", round(p10, 4), "&", round(p12, 4), "& &", round(p14, 4), "&", round(p16, 4), "\n")
}

for (f in files) {
  res <- readRDS(f)

  res_df <- sim_res_to_df(res)

  p1 <- mean(res_df$pct_truncated)
  p2 <- mean(res_df$pct_censored)
  p3 <- mean(res_df$var_beta_1 > 0 & res_df$var_beta_2 > 0)

  cat(paste0(round(p1 * 100, 1), "\\% & ", round(p2 * 100, 1), "\\% & ", round(p3 * 100, 1), "\\% \\\\"), "\n")

}

#### One Variable ----

res <- readRDS("inst/ltrc_sim_res_1_var/non_centered_mixed_normal_200.rds")

res_df <- sim_res_to_df(res)

mean(res_df$beta_var > 0)

p1 <- mean(res_df$beta) - 1
p3 <- mean(sqrt(res_df$beta_var), na.rm = TRUE)
p5 <- sd(res_df$beta)

p7 <- mean(res_df$beta - 2 * sqrt(res_df$beta_var) < 1 & res_df$beta + 2 * sqrt(res_df$beta_var) > 1, na.rm = TRUE)

p9 <- mean(res_df$rc_beta) - 1
p11 <- sd(res_df$rc_beta)


p13 <- mean(res_df$naive_beta) - 1
p15 <- sd(res_df$naive_beta)

cat(round(p1, 4), "&", round(p3, 4), "&", round(p5, 4), "&", round(p7, 4), "& &", round(p9, 4), "&", round(p11, 4), "& &", round(p13, 4), "&", round(p15, 4), "\n")

#### Only normal error ----

library(dplyr)
library(fs)

dir_path <- "inst/ltrc_sim_res_extreme_truncation/"
files <- dir_ls(dir_path)

vars_gt_0 <- numeric()
vars_gt_0_2 <- numeric()
i <- 1
for (f in files) {
  res <- readRDS(f)

  res_df <- sim_res_to_df(res)

  vars_gt_0[i] <- mean(res_df$var_beta_1 > 0)
  vars_gt_0_2[i] <- mean(res_df$var_beta_2 > 0)

  res_df_clean <- res_df %>%
    filter(beta_2 < 5, beta_2 > -1)

  p1 <- mean(res_df_clean$beta_1) - 1
  p2 <- mean(res_df_clean$beta_2) -1
  p3 <- mean(res_df_clean$var_2_beta_1)
  p4 <- mean(res_df_clean$var_2_beta_2)
  p5 <- var(res_df_clean$beta_1)
  p6 <- var(res_df_clean$beta_2)

  p7 <- mean(res_df_clean$beta_1 - 2 * sqrt(res_df_clean$var_beta_1) < 1 & res_df_clean$beta_1 + 2 * sqrt(res_df_clean$var_beta_1) > 1, na.rm = TRUE)
  p8 <- mean(res_df_clean$beta_2 - 2 * sqrt(res_df_clean$var_beta_2) < 1 & res_df_clean$beta_2 + 2 * sqrt(res_df_clean$var_beta_2) > 1, na.rm = TRUE)



  rc_df_clean <- res_df %>%
    filter(rc_beta_2 > -1, rc_beta_2 < 1)
  p9 <- mean(rc_df_clean$rc_beta_1) - 1
  p10 <- mean(rc_df_clean$rc_beta_2) - 1
  p11 <- sd(rc_df_clean$rc_beta_1)
  p12 <- sd(rc_df_clean$rc_beta_2)


  p13 <- mean(res_df_clean$naive_beta_1) - 1
  p14 <- mean(res_df_clean$naive_beta_2) - 1
  p15 <- sd(res_df_clean$naive_beta_1)
  p16 <- sd(res_df_clean$naive_beta_2)

  cat(round(p1, 4), "&", round(p3, 4), "&", round(p5, 4), "&", round(p7, 4), "\n") #, "& &", round(p9, 4), "&", round(p11, 4), "& &", round(p13, 4), "&", round(p15, 4), "\n")
  cat(round(p2, 4), "&", round(p4, 4), "&", round(p6, 4), "&", round(p8, 4), "\n") #, "& &", round(p10, 4), "&", round(p12, 4), "& &", round(p14, 4), "&", round(p16, 4), "\n")
  i <- i+1
}

