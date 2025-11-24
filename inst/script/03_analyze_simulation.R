## Analyze the simulation study on the server (lots of memory and such)

library(readr)
library(stringr)
library(fs)
library(dplyr)

paths <- dir_ls("ltrc_new_sim_res")

# We need two parts to dealing with this: the actual result part, and then the file name information part
big_final_df <- data.frame()
for (file in paths) {
  tryCatch({
  cat("Working on File:", file, "\n")
  ## File information part
  file_name <- path_file(file)
  info_vec <- strsplit(file_name, "_")[[1]]
  error <- info_vec[2]
  sample_size <- as.numeric(info_vec[3])
  censoring_prob <- as.numeric(info_vec[4]) / 100

  ## Actual result part
  tmp_res <- read_rds(file)
  out_df <- data.frame()
  for (i in tmp_res) {
    if (is.null(i$beta)) next

    var_1 <- tryCatch({
      solve(i$efficient_score_info) %>% diag()
    },
    error = function(e) {
      return(c(NA, NA))
    })
    var_2 <- tryCatch({
      solve(i$inform) %>% diag()
    },
    error = function(e) {
      return(c(NA, NA))
    })

    tmp_out <- data.frame(
      parameter = c("beta_1", "beta_2"),
      truth = c(0.5, 1),
      estimate = i$beta,
      var_1 = var_1[1:2],
      var_2 = var_2[1:2],
      converged = i$converge,
      errored = i$errored,
      score_norm = sum(i$score^2),
      trunc_prob = i$pct_truncated,
      actual_pct_censored = i$pct_censored
    ) %>%
      mutate(
        bias = estimate - truth,
        ci_lower_1 = estimate - 2 * sqrt(var_1),
        ci_upper_1 = estimate + 2 * sqrt(var_1),
        ci_lower_2 = estimate - 2 * sqrt(var_2),
        ci_upper_2 = estimate + 2 * sqrt(var_2),
        truth_in_ci_1 = ifelse((truth >= ci_lower_1) & (truth <= ci_upper_1), 1, 0),
        truth_in_ci_2 = ifelse((truth >= ci_lower_2) & (truth <= ci_upper_2), 1, 0)
      )
    out_df <- out_df %>%
      bind_rows(tmp_out)
  }

  tmp_big_final_df <- out_df %>%
    mutate(
      error = error,
      sample_size = sample_size,
      censoring_prob = censoring_prob
    ) %>%
    select(
      error,
      sample_size,
      censoring_prob,
      parameter,
      bias,
      var_1,
      var_2,
      truth_in_ci_1,
      truth_in_ci_2,
      truth,
      estimate,
      ci_lower_1,
      ci_upper_1,
      ci_lower_2,
      ci_upper_2,
      converged,
      errored,
      score_norm,
      trunc_prob,
      actual_pct_censored
    )
  big_final_df <- big_final_df %>%
    bind_rows(tmp_big_final_df)
  },
  error = function(e){
    cat("ERROR \n\n")
  })
}

write_csv(big_final_df, "ltrc_new_sim_res_output.csv")

q("no")
