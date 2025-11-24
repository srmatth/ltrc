## Analyze the simulation study on the server (lots of memory and such)

library(readr)
library(stringr)
library(fs)
library(dplyr)

paths <- dir_ls("ltrc_updated_sim_res")
paths <- stringr::str_subset(paths, "error_c_200_50")

# We need two parts to dealing with this: the actual result part, and then the file name information part
big_final_df <- data.frame()
for (file in paths) {
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
      var_2 = var_2[1:2]
    ) %>%
      mutate(
        bias = estimate - truth,
        ci_lower = estimate - 1.96 * sqrt(var_2),
        ci_upper = estimate + 1.96 * sqrt(var_2),
        truth_in_ci = ifelse((truth >= ci_lower) & (truth <= ci_upper), 1, 0)
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
      truth_in_ci,
      truth,
      estimate,
      ci_lower,
      ci_upper
    )
  big_final_df <- big_final_df %>%
    bind_rows(tmp_big_final_df)
}

write_csv(big_final_df, "ltrc_updated_sim_res_output_bin.csv")

q("no")
