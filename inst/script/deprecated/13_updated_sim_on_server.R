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
C_UB <- 11
SAMPLE_SIZE_MULT <- 20

## 50% Censoring ----
PCT_CENS <- 50

## Normal(1, 1/12) error ----
cat("Working on N(1, 1/12) error, 100 Observations\n")

FINAL_KNOTS <- 1
error_dist <- function(x) {rnorm(x, 1, sqrt(1/12))}
SAMPLE_SIZE <- 100

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
out_file <- paste0("ltrc_updated_sim_res/error_a_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on N(1, 1/12) error, 200 Observations\n")

FINAL_KNOTS <- 1
error_dist <- function(x) {rnorm(x, 1, sqrt(1/12))}
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
out_file <- paste0("ltrc_updated_sim_res/error_a_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on N(1, 1/12) error, 400 Observations\n")

FINAL_KNOTS <- 1
error_dist <- function(x) {rnorm(x, 1, sqrt(1/12))}
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
out_file <- paste0("ltrc_updated_sim_res/error_a_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on N(1, 1/12) error, 800 Observations\n")

FINAL_KNOTS <- 1
error_dist <- function(x) {rnorm(x, 1, sqrt(1/12))}
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
out_file <- paste0("ltrc_updated_sim_res/error_a_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)


## Uniform(-0.5, 0.5) error ----
cat("Working on Unif(0.5, 1.5) error, 100 Observations\n")

FINAL_KNOTS <- 1
error_dist <- function(x) {runif(x, 0.5, 1.5)}
SAMPLE_SIZE <- 100
C_UB <- 11

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
out_file <- paste0("ltrc_updated_sim_res/error_b_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Unif(0.5, 1.5) error, 200 Observations\n")

FINAL_KNOTS <- 1
error_dist <- function(x) {runif(x, 0.5, 1.5)}
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
out_file <- paste0("ltrc_updated_sim_res/error_b_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Unif(0.5, 1.5) error, 400 Observations\n")

FINAL_KNOTS <- 1
error_dist <- function(x) {runif(x, 0.5, 1.5)}
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
out_file <- paste0("ltrc_updated_sim_res/error_b_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Unif(0.5, 1.5) error, 800 Observations\n")

FINAL_KNOTS <- 1
error_dist <- function(x) {runif(x, 0.5, 1.5)}
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
out_file <- paste0("ltrc_updated_sim_res/error_b_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

## Standard extreme value error ----

C_UB <- 15

cat("Working on Standard EVD error, 100 Observations\n")

FINAL_KNOTS <- 1
error_dist <- function(x) {revd(x)}
SAMPLE_SIZE <- 100

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
out_file <- paste0("ltrc_updated_sim_res/error_c_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Standard EVD error, 200 Observations\n")

FINAL_KNOTS <- 1
error_dist <- function(x) {revd(x)}
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
out_file <- paste0("ltrc_updated_sim_res/error_c_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Standard EVD error, 400 Observations\n")

FINAL_KNOTS <- 1
error_dist <- function(x) {revd(x)}
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
out_file <- paste0("ltrc_updated_sim_res/error_c_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Standard EVD error, 800 Observations\n")

FINAL_KNOTS <- 2
error_dist <- function(x) {revd(x)}
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
out_file <- paste0("ltrc_updated_sim_res/error_c_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

## Mixed Normal Error (zero-centered) ----

C_UB <- 50

cat("Working on Centered Mixed Normal error, 100 Observations\n")

FINAL_KNOTS <- 2
error_dist <- function(x) {
  coin_flip <- runif(x)
  ep1 <- rnorm(x) + 1
  ep2 <- rnorm(x, 0, 3) + 1
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2)
  return(err_vec)
}
SAMPLE_SIZE <- 100

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
out_file <- paste0("ltrc_updated_sim_res/error_d_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Centered Mixed Normal error, 200 Observations\n")

FINAL_KNOTS <- 2
error_dist <- function(x) {
  coin_flip <- runif(x)
  ep1 <- rnorm(x) + 1
  ep2 <- rnorm(x, 0, 3) + 1
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2)
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
out_file <- paste0("ltrc_updated_sim_res/error_d_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Centered Mixed Normal error, 400 Observations\n")

FINAL_KNOTS <- 3
error_dist <- function(x) {
  coin_flip <- runif(x)
  ep1 <- rnorm(x) + 1
  ep2 <- rnorm(x, 0, 3) + 1
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2)
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
out_file <- paste0("ltrc_updated_sim_res/error_d_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Centered Mixed Normal error, 800 Observations\n")

FINAL_KNOTS <- 4
error_dist <- function(x) {
  coin_flip <- runif(x)
  ep1 <- rnorm(x) + 1
  ep2 <- rnorm(x, 0, 3) + 1
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2)
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
out_file <- paste0("ltrc_updated_sim_res/error_d_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)


## Mixed Normal (Non-Zero-Centered) ----

C_UB <- 17

cat("Working on Non-Centered Mixed Normal error, 100 Observations\n")

FINAL_KNOTS <- 2
error_dist <- function(x) {
  coin_flip <- runif(x)
  ep1 <- rnorm(x) + 1.5
  ep2 <- rnorm(x, -1, 0.5) + 1.5
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2)
  return(err_vec)
}
SAMPLE_SIZE <- 100

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
out_file <- paste0("ltrc_updated_sim_res/error_e_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Non-Centered Mixed Normal error, 200 Observations\n")

FINAL_KNOTS <- 2
error_dist <- function(x) {
  coin_flip <- runif(x)
  ep1 <- rnorm(x) + 1.5
  ep2 <- rnorm(x, -1, 0.5) + 1.5
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2)
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
out_file <- paste0("ltrc_updated_sim_res/error_e_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Non-Centered Mixed Normal error, 400 Observations\n")

FINAL_KNOTS <- 2
error_dist <- function(x) {
  coin_flip <- runif(x)
  ep1 <- rnorm(x) + 1.5
  ep2 <- rnorm(x, -1, 0.5) + 1.5
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2)
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
out_file <- paste0("ltrc_updated_sim_res/error_e_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Non-Centered Mixed Normal error, 800 Observations\n")

FINAL_KNOTS <- 3
error_dist <- function(x) {
  coin_flip <- runif(x)
  ep1 <- rnorm(x) + 1.5
  ep2 <- rnorm(x, -1, 0.5) + 1.5
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2)
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
out_file <- paste0("ltrc_updated_sim_res/error_e_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

## 30% Censoring ----
SAMPLE_SIZE_MULT <- 20
PCT_CENS <- 30

## Normal(1, 1/12) error ----

C_UB <- 17

cat("Working on N(1, 1/12) error, 100 Observations\n")

FINAL_KNOTS <- 1
error_dist <- function(x) {rnorm(x, 1, sqrt(1/12))}
SAMPLE_SIZE <- 100

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
out_file <- paste0("ltrc_updated_sim_res/error_a_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on N(1, 1/12) error, 200 Observations\n")

FINAL_KNOTS <- 1
error_dist <- function(x) {rnorm(x, 1, sqrt(1/12))}
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
out_file <- paste0("ltrc_updated_sim_res/error_a_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on N(1, 1/12) error, 400 Observations\n")

FINAL_KNOTS <- 1
error_dist <- function(x) {rnorm(x, 1, sqrt(1/12))}
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
out_file <- paste0("ltrc_updated_sim_res/error_a_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on N(1, 1/12) error, 800 Observations\n")

FINAL_KNOTS <- 1
error_dist <- function(x) {rnorm(x, 1, sqrt(1/12))}
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
out_file <- paste0("ltrc_updated_sim_res/error_a_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)


## Uniform(-0.5, 0.5) error ----

C_UB <- 17

cat("Working on Unif(0.5, 1.5) error, 100 Observations\n")

FINAL_KNOTS <- 1
error_dist <- function(x) {runif(x, 0.5, 1.5)}
SAMPLE_SIZE <- 100

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
out_file <- paste0("ltrc_updated_sim_res/error_b_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Unif(0.5, 1.5) error, 200 Observations\n")

FINAL_KNOTS <- 1
error_dist <- function(x) {runif(x, 0.5, 1.5)}
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
out_file <- paste0("ltrc_updated_sim_res/error_b_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Unif(0.5, 1.5) error, 400 Observations\n")

FINAL_KNOTS <- 1
error_dist <- function(x) {runif(x, 0.5, 1.5)}
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
out_file <- paste0("ltrc_updated_sim_res/error_b_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Unif(0.5, 1.5) error, 800 Observations\n")

FINAL_KNOTS <- 1
error_dist <- function(x) {runif(x, 0.5, 1.5)}
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
out_file <- paste0("ltrc_updated_sim_res/error_b_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

## Standard extreme value error ----

C_UB <- 60

cat("Working on Standard EVD error, 100 Observations\n")

FINAL_KNOTS <- 1
error_dist <- function(x) {revd(x)}
SAMPLE_SIZE <- 100

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
out_file <- paste0("ltrc_updated_sim_res/error_c_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Standard EVD error, 200 Observations\n")

FINAL_KNOTS <- 1
error_dist <- function(x) {revd(x)}
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
out_file <- paste0("ltrc_updated_sim_res/error_c_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Standard EVD error, 400 Observations\n")

FINAL_KNOTS <- 1
error_dist <- function(x) {revd(x)}
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
out_file <- paste0("ltrc_updated_sim_res/error_c_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Standard EVD error, 800 Observations\n")

FINAL_KNOTS <- 2
error_dist <- function(x) {revd(x)}
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
out_file <- paste0("ltrc_updated_sim_res/error_c_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

## Mixed Normal Error (zero-centered) ----

C_UB <- 190

cat("Working on Centered Mixed Normal error, 100 Observations\n")

FINAL_KNOTS <- 2
error_dist <- function(x) {
  coin_flip <- runif(x)
  ep1 <- rnorm(x) + 1
  ep2 <- rnorm(x, 0, 3) + 1
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2)
  return(err_vec)
}
SAMPLE_SIZE <- 100

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
out_file <- paste0("ltrc_updated_sim_res/error_d_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Centered Mixed Normal error, 200 Observations\n")

FINAL_KNOTS <- 2
error_dist <- function(x) {
  coin_flip <- runif(x)
  ep1 <- rnorm(x) + 1
  ep2 <- rnorm(x, 0, 3) + 1
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2)
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
out_file <- paste0("ltrc_updated_sim_res/error_d_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Centered Mixed Normal error, 400 Observations\n")

FINAL_KNOTS <- 3
error_dist <- function(x) {
  coin_flip <- runif(x)
  ep1 <- rnorm(x) + 1
  ep2 <- rnorm(x, 0, 3) + 1
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2)
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
out_file <- paste0("ltrc_updated_sim_res/error_d_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Centered Mixed Normal error, 800 Observations\n")

FINAL_KNOTS <- 4
error_dist <- function(x) {
  coin_flip <- runif(x)
  ep1 <- rnorm(x) + 1
  ep2 <- rnorm(x, 0, 3) + 1
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2)
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
out_file <- paste0("ltrc_updated_sim_res/error_d_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)


## Mixed Normal (Non-Zero-Centered) ----

C_UB <- 45

cat("Working on Non-Centered Mixed Normal error, 100 Observations\n")

FINAL_KNOTS <- 2
error_dist <- function(x) {
  coin_flip <- runif(x)
  ep1 <- rnorm(x) + 1.5
  ep2 <- rnorm(x, -1, 0.5) + 1.5
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2)
  return(err_vec)
}
SAMPLE_SIZE <- 100

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
out_file <- paste0("ltrc_updated_sim_res/error_e_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Non-Centered Mixed Normal error, 200 Observations\n")

FINAL_KNOTS <- 2
error_dist <- function(x) {
  coin_flip <- runif(x)
  ep1 <- rnorm(x) + 1.5
  ep2 <- rnorm(x, -1, 0.5) + 1.5
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2)
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
out_file <- paste0("ltrc_updated_sim_res/error_e_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Non-Centered Mixed Normal error, 400 Observations\n")

FINAL_KNOTS <- 2
error_dist <- function(x) {
  coin_flip <- runif(x)
  ep1 <- rnorm(x) + 1.5
  ep2 <- rnorm(x, -1, 0.5) + 1.5
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2)
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
out_file <- paste0("ltrc_updated_sim_res/error_e_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Non-Centered Mixed Normal error, 800 Observations\n")

FINAL_KNOTS <- 3
error_dist <- function(x) {
  coin_flip <- runif(x)
  ep1 <- rnorm(x) + 1.5
  ep2 <- rnorm(x, -1, 0.5) + 1.5
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2)
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
out_file <- paste0("ltrc_updated_sim_res/error_e_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)


cat("All Done with 30% Censoring!\n")

## 15% Censoring ----
SAMPLE_SIZE_MULT <- 20
PCT_CENS <- 15

## Normal(1, 1/12) error ----

C_UB <- 30

cat("Working on N(1, 1/12) error, 100 Observations\n")

FINAL_KNOTS <- 1
error_dist <- function(x) {rnorm(x, 1, sqrt(1/12))}
SAMPLE_SIZE <- 100

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
out_file <- paste0("ltrc_updated_sim_res/error_a_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on N(1, 1/12) error, 200 Observations\n")

FINAL_KNOTS <- 1
error_dist <- function(x) {rnorm(x, 1, sqrt(1/12))}
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
out_file <- paste0("ltrc_updated_sim_res/error_a_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on N(1, 1/12) error, 400 Observations\n")

FINAL_KNOTS <- 1
error_dist <- function(x) {rnorm(x, 1, sqrt(1/12))}
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
out_file <- paste0("ltrc_updated_sim_res/error_a_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on N(1, 1/12) error, 800 Observations\n")

FINAL_KNOTS <- 1
error_dist <- function(x) {rnorm(x, 1, sqrt(1/12))}
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
out_file <- paste0("ltrc_updated_sim_res/error_a_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)


## Uniform(-0.5, 0.5) error ----

C_UB <- 30

cat("Working on Unif(0.5, 1.5) error, 100 Observations\n")

FINAL_KNOTS <- 1
error_dist <- function(x) {runif(x, 0.5, 1.5)}
SAMPLE_SIZE <- 100

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
out_file <- paste0("ltrc_updated_sim_res/error_b_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Unif(0.5, 1.5) error, 200 Observations\n")

FINAL_KNOTS <- 1
error_dist <- function(x) {runif(x, 0.5, 1.5)}
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
out_file <- paste0("ltrc_updated_sim_res/error_b_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Unif(0.5, 1.5) error, 400 Observations\n")

FINAL_KNOTS <- 1
error_dist <- function(x) {runif(x, 0.5, 1.5)}
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
out_file <- paste0("ltrc_updated_sim_res/error_b_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Unif(0.5, 1.5) error, 800 Observations\n")

FINAL_KNOTS <- 1
error_dist <- function(x) {runif(x, 0.5, 1.5)}
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
out_file <- paste0("ltrc_updated_sim_res/error_b_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

## Standard extreme value error ----

C_UB <- 200


cat("Working on Standard EVD error, 100 Observations\n")

FINAL_KNOTS <- 1
error_dist <- function(x) {revd(x)}
SAMPLE_SIZE <- 100

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
out_file <- paste0("ltrc_updated_sim_res/error_c_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Standard EVD error, 200 Observations\n")

FINAL_KNOTS <- 1
error_dist <- function(x) {revd(x)}
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
out_file <- paste0("ltrc_updated_sim_res/error_c_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Standard EVD error, 400 Observations\n")

FINAL_KNOTS <- 1
error_dist <- function(x) {revd(x)}
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
out_file <- paste0("ltrc_updated_sim_res/error_c_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Standard EVD error, 800 Observations\n")

FINAL_KNOTS <- 2
error_dist <- function(x) {revd(x)}
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
out_file <- paste0("ltrc_updated_sim_res/error_c_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

## Mixed Normal Error (zero-centered) ----

C_UB <- 800


cat("Working on Centered Mixed Normal error, 100 Observations\n")

FINAL_KNOTS <- 2
error_dist <- function(x) {
  coin_flip <- runif(x)
  ep1 <- rnorm(x) + 1
  ep2 <- rnorm(x, 0, 3) + 1
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2)
  return(err_vec)
}
SAMPLE_SIZE <- 100

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
out_file <- paste0("ltrc_updated_sim_res/error_d_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Centered Mixed Normal error, 200 Observations\n")

FINAL_KNOTS <- 2
error_dist <- function(x) {
  coin_flip <- runif(x)
  ep1 <- rnorm(x) + 1
  ep2 <- rnorm(x, 0, 3) + 1
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2)
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
out_file <- paste0("ltrc_updated_sim_res/error_d_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Centered Mixed Normal error, 400 Observations\n")

FINAL_KNOTS <- 3
error_dist <- function(x) {
  coin_flip <- runif(x)
  ep1 <- rnorm(x) + 1
  ep2 <- rnorm(x, 0, 3) + 1
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2)
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
out_file <- paste0("ltrc_updated_sim_res/error_d_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Centered Mixed Normal error, 800 Observations\n")

FINAL_KNOTS <- 4
error_dist <- function(x) {
  coin_flip <- runif(x)
  ep1 <- rnorm(x) + 1
  ep2 <- rnorm(x, 0, 3) + 1
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2)
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
out_file <- paste0("ltrc_updated_sim_res/error_d_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)


## Mixed Normal (Non-Zero-Centered) ----

C_UB <- 400


cat("Working on Non-Centered Mixed Normal error, 100 Observations\n")

FINAL_KNOTS <- 2
error_dist <- function(x) {
  coin_flip <- runif(x)
  ep1 <- rnorm(x) + 1.5
  ep2 <- rnorm(x, -1, 0.5) + 1.5
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2)
  return(err_vec)
}
SAMPLE_SIZE <- 100

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
out_file <- paste0("ltrc_updated_sim_res/error_e_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Non-Centered Mixed Normal error, 200 Observations\n")

FINAL_KNOTS <- 2
error_dist <- function(x) {
  coin_flip <- runif(x)
  ep1 <- rnorm(x) + 1.5
  ep2 <- rnorm(x, -1, 0.5) + 1.5
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2)
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
out_file <- paste0("ltrc_updated_sim_res/error_e_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Non-Centered Mixed Normal error, 400 Observations\n")

FINAL_KNOTS <- 2
error_dist <- function(x) {
  coin_flip <- runif(x)
  ep1 <- rnorm(x) + 1.5
  ep2 <- rnorm(x, -1, 0.5) + 1.5
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2)
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
out_file <- paste0("ltrc_updated_sim_res/error_e_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)

cat("Working on Non-Centered Mixed Normal error, 800 Observations\n")

FINAL_KNOTS <- 3
error_dist <- function(x) {
  coin_flip <- runif(x)
  ep1 <- rnorm(x) + 1.5
  ep2 <- rnorm(x, -1, 0.5) + 1.5
  err_vec <- ifelse(coin_flip > 0.5, ep1, ep2)
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
out_file <- paste0("ltrc_updated_sim_res/error_e_", SAMPLE_SIZE, "_", PCT_CENS, "_", FILE_NUM, ".rds")
write_rds(big_result, out_file)


cat("All Done with 15% Censoring!\n")



cat("All Done!\n")

q(save = "no")
