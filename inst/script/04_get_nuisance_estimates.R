## Get the data to plot the nuisance parameters

library(dplyr)
library(ltrc)
library(fs)
library(stringr)
library(readr)

SUBSETS <- c(
  "error_b_800_15",
  "error_c_800_15",
  "error_d_800_15",
  "error_b_800_30",
  "error_c_800_30",
  "error_d_800_30",
  "error_b_800_50",
  "error_c_800_50",
  "error_d_800_50"
)

for (SUBSET in SUBSETS){

  paths <- dir_ls("ltrc_new_sim_res") %>%
    str_subset(SUBSET)

  # We need two parts to dealing with this: the actual result part, and then the file name information part
  out_df <- data.frame()
  k <- 1
  for (file in paths) {
    cat("Working on File:", file, "\n")
    ## File information part
    file_name <- path_file(file)
    info_vec <- strsplit(file_name, "_")[[1]]
    error <- info_vec[2]
    sample_size <- as.numeric(info_vec[3])
    censoring_prob <- as.numeric(info_vec[4]) / 100

    x_vals <- case_when(
      error == "b" ~ seq(0.573, 1.682, length.out = 300),
      # error == "b" ~ seq(-1.31, 3.67, length.out = 300),
      error == "c" ~ seq(-0.469, 2.405, length.out = 300),
      error == "d" ~ seq(0.505, 1.718, length.out = 300)
    )

    ## Actual result part
    tmp_res <- read_rds(file)

    for (i in tmp_res) {
      k <- k + 1
      if (is.null(i$beta)) next

      tmp_out <- data.frame(
        error = error,
        sample_size = sample_size,
        censoring_prob = censoring_prob,
        sim_iter = k,
        x = x_vals,
        est_y = log(ltrc:::predict_hazard(i, x_vals))
      )

      out_df <- out_df %>%
        bind_rows(tmp_out)
    }
  }

  file_name <- paste0("nuisance/", SUBSET, ".csv")
  write_csv(out_df, file_name)
}

q("no")
