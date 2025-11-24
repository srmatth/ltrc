## Load the libraries

library(extRemes)
library(ltrc)
library(readr)

args <- commandArgs(trailingOnly = TRUE)
FILE_NUM <- as.integer(args[1])
cat("FILE NUMBER:", FILE_NUM, "\n")

N <- 20 # 5000
true_beta <- c(0.5, 1)
n_start <- 10
SAMPLE_SIZE_MULT <- 10

## 50% Censoring ----
PCT_CENS <- 50

## Standard EVD error ----
C_UB <- 11
cat("Working on Standard EVD error, 200 Observations\n")

FINAL_KNOTS <- 1
error_dist <- function(x) {revd(x, loc = 0.87, scale = 0.2250791)}
SAMPLE_SIZE <- 200

big_result <- list()
for (i in 1:N) {
  test <- tryCatch({
    ltrc:::sim_iteration_updated(
      i = i,
      FINAL_KNOTS = FINAL_KNOTS,
      true_beta = true_beta,
      n = SAMPLE_SIZE,
      error_dist = error_dist,
      sd_multiplier = 3,
      n_start = n_start,
      C_UB = C_UB,
      sample_size_mult = SAMPLE_SIZE_MULT
    )
  },
  error = function(e) {
    return(list(beta = NULL))
  })
  if (is.null(test$beta)) {
    cat("Iteration failed!")
    next
  }
  big_result <- append(big_result, list(test))
}
out_file <- paste0("ltrc_new_sim_res/error_b_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Standard EVD error, 400 Observations\n")

FINAL_KNOTS <- 1
error_dist <- function(x) {revd(x, loc = 0.87, scale = 0.2250791)}
SAMPLE_SIZE <- 400

big_result <- list()
for (i in 1:N) {
  test <- tryCatch({
    ltrc:::sim_iteration_updated(
      i = i,
      FINAL_KNOTS = FINAL_KNOTS,
      true_beta = true_beta,
      n = SAMPLE_SIZE,
      error_dist = error_dist,
      sd_multiplier = 3,
      n_start = n_start,
      C_UB = C_UB,
      sample_size_mult = SAMPLE_SIZE_MULT
    )
  },
  error = function(e) {
    return(list(beta = NULL))
  })
  if (is.null(test$beta)) {
    cat("Iteration failed!")
    next
  }
  big_result <- append(big_result, list(test))
}
out_file <- paste0("ltrc_new_sim_res/error_b_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Standard EVD error, 800 Observations\n")

FINAL_KNOTS <- 1
error_dist <- function(x) {revd(x, loc = 0.87, scale = 0.2250791)}
SAMPLE_SIZE <- 800

big_result <- list()
for (i in 1:N) {
  test <- tryCatch({
    ltrc:::sim_iteration_updated(
      i = i,
      FINAL_KNOTS = FINAL_KNOTS,
      true_beta = true_beta,
      n = SAMPLE_SIZE,
      error_dist = error_dist,
      sd_multiplier = 3,
      n_start = n_start,
      C_UB = C_UB,
      sample_size_mult = SAMPLE_SIZE_MULT
    )
  },
  error = function(e) {
    return(list(beta = NULL))
  })
  if (is.null(test$beta)) {
    cat("Iteration failed!")
    next
  }
  big_result <- append(big_result, list(test))
}
out_file <- paste0("ltrc_new_sim_res/error_b_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)


## Mixed Normal Error (zero-centered) ----

C_UB <- 13
cat("Working on Centered Mixed Normal error, 200 Observations\n")

FINAL_KNOTS <- 2
error_dist <- function(x) {
  coin_flip <- runif(x)
  ep1 <- rnorm(x, 0, sqrt(1/8))
  ep2 <- rnorm(x, 0, sqrt(1/24))
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2) + 1
  return(err_vec)
}
SAMPLE_SIZE <- 200

big_result <- list()
for (i in 1:N) {
  test <- tryCatch({
    ltrc:::sim_iteration_updated(
      i = i,
      FINAL_KNOTS = FINAL_KNOTS,
      true_beta = true_beta,
      n = SAMPLE_SIZE,
      error_dist = error_dist,
      sd_multiplier = 3,
      n_start = n_start,
      C_UB = C_UB,
      sample_size_mult = SAMPLE_SIZE_MULT
    )
  },
  error = function(e) {
    print(e)
    return(list(beta = NULL))
  })
  if (is.null(test$beta)) {
    cat("Iteration failed!")
    next
  }
  big_result <- append(big_result, list(test))
}
out_file <- paste0("ltrc_new_sim_res/error_c_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Centered Mixed Normal error, 400 Observations\n")

FINAL_KNOTS <- 3
error_dist <- function(x) {
  coin_flip <- runif(x)
  ep1 <- rnorm(x, 0, sqrt(1/8))
  ep2 <- rnorm(x, 0, sqrt(1/24))
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2) + 1
  return(err_vec)
}
SAMPLE_SIZE <- 400

big_result <- list()
for (i in 1:N) {
  test <- tryCatch({
    ltrc:::sim_iteration_updated(
      i = i,
      FINAL_KNOTS = FINAL_KNOTS,
      true_beta = true_beta,
      n = SAMPLE_SIZE,
      error_dist = error_dist,
      sd_multiplier = 3,
      n_start = n_start,
      C_UB = C_UB,
      sample_size_mult = SAMPLE_SIZE_MULT
    )
  },
  error = function(e) {
    print(e)
    return(list(beta = NULL))
  })
  if (is.null(test$beta)) {
    cat("Iteration failed!")
    next
  }
  big_result <- append(big_result, list(test))
}
out_file <- paste0("ltrc_new_sim_res/error_c_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Centered Mixed Normal error, 800 Observations\n")

FINAL_KNOTS <- 4
error_dist <- function(x) {
  coin_flip <- runif(x)
  ep1 <- rnorm(x, 0, sqrt(1/8))
  ep2 <- rnorm(x, 0, sqrt(1/24))
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2) + 1
  return(err_vec)
}
SAMPLE_SIZE <- 800

big_result <- list()
for (i in 1:N) {
  test <- tryCatch({
    ltrc:::sim_iteration_updated(
      i = i,
      FINAL_KNOTS = FINAL_KNOTS,
      true_beta = true_beta,
      n = SAMPLE_SIZE,
      error_dist = error_dist,
      sd_multiplier = 3,
      n_start = n_start,
      C_UB = C_UB,
      sample_size_mult = SAMPLE_SIZE_MULT
    )
  },
  error = function(e) {
    print(e)
    return(list(beta = NULL))
  })
  if (is.null(test$beta)) {
    cat("Iteration failed!")
    next
  }
  big_result <- append(big_result, list(test))
}
out_file <- paste0("ltrc_new_sim_res/error_c_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)


## Mixed Normal (Non-Zero-Centered) ----

C_UB <- 11

cat("Working on Non-Centered Mixed Normal error, 200 Observations\n")

FINAL_KNOTS <- 2
error_dist <- function(x) {
  coin_flip <- runif(x)
  ep1 <- rnorm(x, 0, sqrt(1/12))
  ep2 <- rnorm(x, -0.3, sqrt(1/24))
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2) + 1.15
  return(err_vec)
}
SAMPLE_SIZE <- 200

big_result <- list()
for (i in 1:N) {
  test <- tryCatch({
    ltrc:::sim_iteration_updated(
      i = i,
      FINAL_KNOTS = FINAL_KNOTS,
      true_beta = true_beta,
      n = SAMPLE_SIZE,
      error_dist = error_dist,
      sd_multiplier = 3,
      n_start = n_start,
      C_UB = C_UB,
      sample_size_mult = SAMPLE_SIZE_MULT
    )
  },
  error = function(e) {
    print(e)
    return(list(beta = NULL))
  })
  if (is.null(test$beta)) {
    cat("Iteration failed!")
    next
  }
  big_result <- append(big_result, list(test))
}
out_file <- paste0("ltrc_new_sim_res/error_d_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Non-Centered Mixed Normal error, 400 Observations\n")

FINAL_KNOTS <- 3
error_dist <- function(x) {
  coin_flip <- runif(x)
  ep1 <- rnorm(x, 0, sqrt(1/12))
  ep2 <- rnorm(x, -0.3, sqrt(1/24))
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2) + 1.15
  return(err_vec)
}
SAMPLE_SIZE <- 400

big_result <- list()
for (i in 1:N) {
  test <- tryCatch({
    ltrc:::sim_iteration_updated(
      i = i,
      FINAL_KNOTS = FINAL_KNOTS,
      true_beta = true_beta,
      n = SAMPLE_SIZE,
      error_dist = error_dist,
      sd_multiplier = 3,
      n_start = n_start,
      C_UB = C_UB,
      sample_size_mult = SAMPLE_SIZE_MULT
    )
  },
  error = function(e) {
    print(e)
    return(list(beta = NULL))
  })
  if (is.null(test$beta)) {
    cat("Iteration failed!")
    next
  }
  big_result <- append(big_result, list(test))
}
out_file <- paste0("ltrc_new_sim_res/error_d_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Non-Centered Mixed Normal error, 800 Observations\n")

FINAL_KNOTS <- 4
error_dist <- function(x) {
  coin_flip <- runif(x)
  ep1 <- rnorm(x, 0, sqrt(1/12))
  ep2 <- rnorm(x, -0.3, sqrt(1/24))
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2) + 1.15
  return(err_vec)
}
SAMPLE_SIZE <- 800

big_result <- list()
for (i in 1:N) {
  test <- tryCatch({
    ltrc:::sim_iteration_updated(
      i = i,
      FINAL_KNOTS = FINAL_KNOTS,
      true_beta = true_beta,
      n = SAMPLE_SIZE,
      error_dist = error_dist,
      sd_multiplier = 3,
      n_start = n_start,
      C_UB = C_UB,
      sample_size_mult = SAMPLE_SIZE_MULT
    )
  },
  error = function(e) {
    print(e)
    return(list(beta = NULL))
  })
  if (is.null(test$beta)) {
    cat("Iteration failed!")
    next
  }
  big_result <- append(big_result, list(test))
}
out_file <- paste0("ltrc_new_sim_res/error_d_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

## 30% Censoring ----
PCT_CENS <- 30


## Standard EVD error ----
C_UB <- 17
cat("Working on Standard EVD error, 200 Observations\n")

FINAL_KNOTS <- 1
error_dist <- function(x) {revd(x, loc = 0.87, scale = 0.2250791)}
SAMPLE_SIZE <- 200

big_result <- list()
for (i in 1:N) {
  test <- tryCatch({
    ltrc:::sim_iteration_updated(
      i = i,
      FINAL_KNOTS = FINAL_KNOTS,
      true_beta = true_beta,
      n = SAMPLE_SIZE,
      error_dist = error_dist,
      sd_multiplier = 3,
      n_start = n_start,
      C_UB = C_UB,
      sample_size_mult = SAMPLE_SIZE_MULT
    )
  },
  error = function(e) {
    return(list(beta = NULL))
  })
  if (is.null(test$beta)) {
    cat("Iteration failed!")
    next
  }
  big_result <- append(big_result, list(test))
}
out_file <- paste0("ltrc_new_sim_res/error_b_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Standard EVD error, 400 Observations\n")

FINAL_KNOTS <- 1
error_dist <- function(x) {revd(x, loc = 0.87, scale = 0.2250791)}
SAMPLE_SIZE <- 400

big_result <- list()
for (i in 1:N) {
  test <- tryCatch({
    ltrc:::sim_iteration_updated(
      i = i,
      FINAL_KNOTS = FINAL_KNOTS,
      true_beta = true_beta,
      n = SAMPLE_SIZE,
      error_dist = error_dist,
      sd_multiplier = 3,
      n_start = n_start,
      C_UB = C_UB,
      sample_size_mult = SAMPLE_SIZE_MULT
    )
  },
  error = function(e) {
    return(list(beta = NULL))
  })
  if (is.null(test$beta)) {
    cat("Iteration failed!")
    next
  }
  big_result <- append(big_result, list(test))
}
out_file <- paste0("ltrc_new_sim_res/error_b_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Standard EVD error, 800 Observations\n")

FINAL_KNOTS <- 1
error_dist <- function(x) {revd(x, loc = 0.87, scale = 0.2250791)}
SAMPLE_SIZE <- 800

big_result <- list()
for (i in 1:N) {
  test <- tryCatch({
    ltrc:::sim_iteration_updated(
      i = i,
      FINAL_KNOTS = FINAL_KNOTS,
      true_beta = true_beta,
      n = SAMPLE_SIZE,
      error_dist = error_dist,
      sd_multiplier = 3,
      n_start = n_start,
      C_UB = C_UB,
      sample_size_mult = SAMPLE_SIZE_MULT
    )
  },
  error = function(e) {
    return(list(beta = NULL))
  })
  if (is.null(test$beta)) {
    cat("Iteration failed!")
    next
  }
  big_result <- append(big_result, list(test))
}
out_file <- paste0("ltrc_new_sim_res/error_b_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)


## Mixed Normal Error (zero-centered) ----

C_UB <- 24
cat("Working on Centered Mixed Normal error, 200 Observations\n")

FINAL_KNOTS <- 2
error_dist <- function(x) {
  coin_flip <- runif(x)
  ep1 <- rnorm(x, 0, sqrt(1/8))
  ep2 <- rnorm(x, 0, sqrt(1/24))
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2) + 1
  return(err_vec)
}
SAMPLE_SIZE <- 200

big_result <- list()
for (i in 1:N) {
  test <- tryCatch({
    ltrc:::sim_iteration_updated(
      i = i,
      FINAL_KNOTS = FINAL_KNOTS,
      true_beta = true_beta,
      n = SAMPLE_SIZE,
      error_dist = error_dist,
      sd_multiplier = 3,
      n_start = n_start,
      C_UB = C_UB,
      sample_size_mult = SAMPLE_SIZE_MULT
    )
  },
  error = function(e) {
    print(e)
    return(list(beta = NULL))
  })
  if (is.null(test$beta)) {
    cat("Iteration failed!")
    next
  }
  big_result <- append(big_result, list(test))
}
out_file <- paste0("ltrc_new_sim_res/error_c_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Centered Mixed Normal error, 400 Observations\n")

FINAL_KNOTS <- 3
error_dist <- function(x) {
  coin_flip <- runif(x)
  ep1 <- rnorm(x, 0, sqrt(1/8))
  ep2 <- rnorm(x, 0, sqrt(1/24))
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2) + 1
  return(err_vec)
}
SAMPLE_SIZE <- 400

big_result <- list()
for (i in 1:N) {
  test <- tryCatch({
    ltrc:::sim_iteration_updated(
      i = i,
      FINAL_KNOTS = FINAL_KNOTS,
      true_beta = true_beta,
      n = SAMPLE_SIZE,
      error_dist = error_dist,
      sd_multiplier = 3,
      n_start = n_start,
      C_UB = C_UB,
      sample_size_mult = SAMPLE_SIZE_MULT
    )
  },
  error = function(e) {
    print(e)
    return(list(beta = NULL))
  })
  if (is.null(test$beta)) {
    cat("Iteration failed!")
    next
  }
  big_result <- append(big_result, list(test))
}
out_file <- paste0("ltrc_new_sim_res/error_c_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Centered Mixed Normal error, 800 Observations\n")

FINAL_KNOTS <- 4
error_dist <- function(x) {
  coin_flip <- runif(x)
  ep1 <- rnorm(x, 0, sqrt(1/8))
  ep2 <- rnorm(x, 0, sqrt(1/24))
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2) + 1
  return(err_vec)
}
SAMPLE_SIZE <- 800

big_result <- list()
for (i in 1:N) {
  test <- tryCatch({
    ltrc:::sim_iteration_updated(
      i = i,
      FINAL_KNOTS = FINAL_KNOTS,
      true_beta = true_beta,
      n = SAMPLE_SIZE,
      error_dist = error_dist,
      sd_multiplier = 3,
      n_start = n_start,
      C_UB = C_UB,
      sample_size_mult = SAMPLE_SIZE_MULT
    )
  },
  error = function(e) {
    print(e)
    return(list(beta = NULL))
  })
  if (is.null(test$beta)) {
    cat("Iteration failed!")
    next
  }
  big_result <- append(big_result, list(test))
}
out_file <- paste0("ltrc_new_sim_res/error_c_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)


## Mixed Normal (Non-Zero-Centered) ----

C_UB <- 18

cat("Working on Non-Centered Mixed Normal error, 200 Observations\n")

FINAL_KNOTS <- 2
error_dist <- function(x) {
  coin_flip <- runif(x)
  ep1 <- rnorm(x, 0, sqrt(1/12))
  ep2 <- rnorm(x, -0.3, sqrt(1/24))
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2) + 1.15
  return(err_vec)
}
SAMPLE_SIZE <- 200

big_result <- list()
for (i in 1:N) {
  test <- tryCatch({
    ltrc:::sim_iteration_updated(
      i = i,
      FINAL_KNOTS = FINAL_KNOTS,
      true_beta = true_beta,
      n = SAMPLE_SIZE,
      error_dist = error_dist,
      sd_multiplier = 3,
      n_start = n_start,
      C_UB = C_UB,
      sample_size_mult = SAMPLE_SIZE_MULT
    )
  },
  error = function(e) {
    print(e)
    return(list(beta = NULL))
  })
  if (is.null(test$beta)) {
    cat("Iteration failed!")
    next
  }
  big_result <- append(big_result, list(test))
}
out_file <- paste0("ltrc_new_sim_res/error_d_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Non-Centered Mixed Normal error, 400 Observations\n")

FINAL_KNOTS <- 3
error_dist <- function(x) {
  coin_flip <- runif(x)
  ep1 <- rnorm(x, 0, sqrt(1/12))
  ep2 <- rnorm(x, -0.3, sqrt(1/24))
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2) + 1.15
  return(err_vec)
}
SAMPLE_SIZE <- 400

big_result <- list()
for (i in 1:N) {
  test <- tryCatch({
    ltrc:::sim_iteration_updated(
      i = i,
      FINAL_KNOTS = FINAL_KNOTS,
      true_beta = true_beta,
      n = SAMPLE_SIZE,
      error_dist = error_dist,
      sd_multiplier = 3,
      n_start = n_start,
      C_UB = C_UB,
      sample_size_mult = SAMPLE_SIZE_MULT
    )
  },
  error = function(e) {
    print(e)
    return(list(beta = NULL))
  })
  if (is.null(test$beta)) {
    cat("Iteration failed!")
    next
  }
  big_result <- append(big_result, list(test))
}
out_file <- paste0("ltrc_new_sim_res/error_d_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Non-Centered Mixed Normal error, 800 Observations\n")

FINAL_KNOTS <- 3
error_dist <- function(x) {
  coin_flip <- runif(x)
  ep1 <- rnorm(x, 0, sqrt(1/12))
  ep2 <- rnorm(x, -0.3, sqrt(1/24))
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2) + 1.15
  return(err_vec)
}
SAMPLE_SIZE <- 800

big_result <- list()
for (i in 1:N) {
  test <- tryCatch({
    ltrc:::sim_iteration_updated(
      i = i,
      FINAL_KNOTS = FINAL_KNOTS,
      true_beta = true_beta,
      n = SAMPLE_SIZE,
      error_dist = error_dist,
      sd_multiplier = 3,
      n_start = n_start,
      C_UB = C_UB,
      sample_size_mult = SAMPLE_SIZE_MULT
    )
  },
  error = function(e) {
    print(e)
    return(list(beta = NULL))
  })
  if (is.null(test$beta)) {
    cat("Iteration failed!")
    next
  }
  big_result <- append(big_result, list(test))
}
out_file <- paste0("ltrc_new_sim_res/error_d_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

## 15% Censoring ----
PCT_CENS <- 15

## Standard EVD error ----
C_UB <- 32
cat("Working on Standard EVD error, 200 Observations\n")

FINAL_KNOTS <- 1
error_dist <- function(x) {revd(x, loc = 0.87, scale = 0.2250791)}
SAMPLE_SIZE <- 200

big_result <- list()
for (i in 1:N) {
  test <- tryCatch({
    ltrc:::sim_iteration_updated(
      i = i,
      FINAL_KNOTS = FINAL_KNOTS,
      true_beta = true_beta,
      n = SAMPLE_SIZE,
      error_dist = error_dist,
      sd_multiplier = 3,
      n_start = n_start,
      C_UB = C_UB,
      sample_size_mult = SAMPLE_SIZE_MULT
    )
  },
  error = function(e) {
    return(list(beta = NULL))
  })
  if (is.null(test$beta)) {
    cat("Iteration failed!")
    next
  }
  big_result <- append(big_result, list(test))
}
out_file <- paste0("ltrc_new_sim_res/error_b_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Standard EVD error, 400 Observations\n")

FINAL_KNOTS <- 1
error_dist <- function(x) {revd(x, loc = 0.87, scale = 0.2250791)}
SAMPLE_SIZE <- 400

big_result <- list()
for (i in 1:N) {
  test <- tryCatch({
    ltrc:::sim_iteration_updated(
      i = i,
      FINAL_KNOTS = FINAL_KNOTS,
      true_beta = true_beta,
      n = SAMPLE_SIZE,
      error_dist = error_dist,
      sd_multiplier = 3,
      n_start = n_start,
      C_UB = C_UB,
      sample_size_mult = SAMPLE_SIZE_MULT
    )
  },
  error = function(e) {
    return(list(beta = NULL))
  })
  if (is.null(test$beta)) {
    cat("Iteration failed!")
    next
  }
  big_result <- append(big_result, list(test))
}
out_file <- paste0("ltrc_new_sim_res/error_b_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Standard EVD error, 800 Observations\n")

FINAL_KNOTS <- 1
error_dist <- function(x) {revd(x, loc = 0.87, scale = 0.2250791)}
SAMPLE_SIZE <- 800

big_result <- list()
for (i in 1:N) {
  test <- tryCatch({
    ltrc:::sim_iteration_updated(
      i = i,
      FINAL_KNOTS = FINAL_KNOTS,
      true_beta = true_beta,
      n = SAMPLE_SIZE,
      error_dist = error_dist,
      sd_multiplier = 3,
      n_start = n_start,
      C_UB = C_UB,
      sample_size_mult = SAMPLE_SIZE_MULT
    )
  },
  error = function(e) {
    return(list(beta = NULL))
  })
  if (is.null(test$beta)) {
    cat("Iteration failed!")
    next
  }
  big_result <- append(big_result, list(test))
}
out_file <- paste0("ltrc_new_sim_res/error_b_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)


## Mixed Normal Error (zero-centered) ----

C_UB <- 50

cat("Working on Centered Mixed Normal error, 200 Observations\n")

FINAL_KNOTS <- 2
error_dist <- function(x) {
  coin_flip <- runif(x)
  ep1 <- rnorm(x, 0, sqrt(1/8))
  ep2 <- rnorm(x, 0, sqrt(1/24))
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2) + 1
  return(err_vec)
}
SAMPLE_SIZE <- 200

big_result <- list()
for (i in 1:N) {
  test <- tryCatch({
    ltrc:::sim_iteration_updated(
      i = i,
      FINAL_KNOTS = FINAL_KNOTS,
      true_beta = true_beta,
      n = SAMPLE_SIZE,
      error_dist = error_dist,
      sd_multiplier = 3,
      n_start = n_start,
      C_UB = C_UB,
      sample_size_mult = SAMPLE_SIZE_MULT
    )
  },
  error = function(e) {
    print(e)
    return(list(beta = NULL))
  })
  if (is.null(test$beta)) {
    cat("Iteration failed!")
    next
  }
  big_result <- append(big_result, list(test))
}
out_file <- paste0("ltrc_new_sim_res/error_c_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Centered Mixed Normal error, 400 Observations\n")

FINAL_KNOTS <- 3
error_dist <- function(x) {
  coin_flip <- runif(x)
  ep1 <- rnorm(x, 0, sqrt(1/8))
  ep2 <- rnorm(x, 0, sqrt(1/24))
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2) + 1
  return(err_vec)
}
SAMPLE_SIZE <- 400

big_result <- list()
for (i in 1:N) {
  test <- tryCatch({
    ltrc:::sim_iteration_updated(
      i = i,
      FINAL_KNOTS = FINAL_KNOTS,
      true_beta = true_beta,
      n = SAMPLE_SIZE,
      error_dist = error_dist,
      sd_multiplier = 3,
      n_start = n_start,
      C_UB = C_UB,
      sample_size_mult = SAMPLE_SIZE_MULT
    )
  },
  error = function(e) {
    print(e)
    return(list(beta = NULL))
  })
  if (is.null(test$beta)) {
    cat("Iteration failed!")
    next
  }
  big_result <- append(big_result, list(test))
}
out_file <- paste0("ltrc_new_sim_res/error_c_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Centered Mixed Normal error, 800 Observations\n")

FINAL_KNOTS <- 4
error_dist <- function(x) {
  coin_flip <- runif(x)
  ep1 <- rnorm(x, 0, sqrt(1/8))
  ep2 <- rnorm(x, 0, sqrt(1/24))
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2) + 1
  return(err_vec)
}
SAMPLE_SIZE <- 800

big_result <- list()
for (i in 1:N) {
  test <- tryCatch({
    ltrc:::sim_iteration_updated(
      i = i,
      FINAL_KNOTS = FINAL_KNOTS,
      true_beta = true_beta,
      n = SAMPLE_SIZE,
      error_dist = error_dist,
      sd_multiplier = 3,
      n_start = n_start,
      C_UB = C_UB,
      sample_size_mult = SAMPLE_SIZE_MULT
    )
  },
  error = function(e) {
    print(e)
    return(list(beta = NULL))
  })
  if (is.null(test$beta)) {
    cat("Iteration failed!")
    next
  }
  big_result <- append(big_result, list(test))
}
out_file <- paste0("ltrc_new_sim_res/error_c_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)


## Mixed Normal (Non-Zero-Centered) ----

C_UB <- 32

cat("Working on Non-Centered Mixed Normal error, 200 Observations\n")

FINAL_KNOTS <- 2
error_dist <- function(x) {
  coin_flip <- runif(x)
  ep1 <- rnorm(x, 0, sqrt(1/12))
  ep2 <- rnorm(x, -0.3, sqrt(1/24))
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2) + 1.15
  return(err_vec)
}
SAMPLE_SIZE <- 200

big_result <- list()
for (i in 1:N) {
  test <- tryCatch({
    ltrc:::sim_iteration_updated(
      i = i,
      FINAL_KNOTS = FINAL_KNOTS,
      true_beta = true_beta,
      n = SAMPLE_SIZE,
      error_dist = error_dist,
      sd_multiplier = 3,
      n_start = n_start,
      C_UB = C_UB,
      sample_size_mult = SAMPLE_SIZE_MULT
    )
  },
  error = function(e) {
    print(e)
    return(list(beta = NULL))
  })
  if (is.null(test$beta)) {
    cat("Iteration failed!")
    next
  }
  big_result <- append(big_result, list(test))
}
out_file <- paste0("ltrc_new_sim_res/error_d_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Non-Centered Mixed Normal error, 400 Observations\n")

FINAL_KNOTS <- 3
error_dist <- function(x) {
  coin_flip <- runif(x)
  ep1 <- rnorm(x, 0, sqrt(1/12))
  ep2 <- rnorm(x, -0.3, sqrt(1/24))
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2) + 1.15
  return(err_vec)
}
SAMPLE_SIZE <- 400

big_result <- list()
for (i in 1:N) {
  test <- tryCatch({
    ltrc:::sim_iteration_updated(
      i = i,
      FINAL_KNOTS = FINAL_KNOTS,
      true_beta = true_beta,
      n = SAMPLE_SIZE,
      error_dist = error_dist,
      sd_multiplier = 3,
      n_start = n_start,
      C_UB = C_UB,
      sample_size_mult = SAMPLE_SIZE_MULT
    )
  },
  error = function(e) {
    print(e)
    return(list(beta = NULL))
  })
  if (is.null(test$beta)) {
    cat("Iteration failed!")
    next
  }
  big_result <- append(big_result, list(test))
}
out_file <- paste0("ltrc_new_sim_res/error_d_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Non-Centered Mixed Normal error, 800 Observations\n")

FINAL_KNOTS <- 4
error_dist <- function(x) {
  coin_flip <- runif(x)
  ep1 <- rnorm(x, 0, sqrt(1/12))
  ep2 <- rnorm(x, -0.3, sqrt(1/24))
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2) + 1.15
  return(err_vec)
}
SAMPLE_SIZE <- 800

big_result <- list()
for (i in 1:N) {
  test <- tryCatch({
    ltrc:::sim_iteration_updated(
      i = i,
      FINAL_KNOTS = FINAL_KNOTS,
      true_beta = true_beta,
      n = SAMPLE_SIZE,
      error_dist = error_dist,
      sd_multiplier = 3,
      n_start = n_start,
      C_UB = C_UB,
      sample_size_mult = SAMPLE_SIZE_MULT
    )
  },
  error = function(e) {
    print(e)
    return(list(beta = NULL))
  })
  if (is.null(test$beta)) {
    cat("Iteration failed!")
    next
  }
  big_result <- append(big_result, list(test))
}
out_file <- paste0("ltrc_new_sim_res/error_d_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("All Done!\n")

q(save = "no")
