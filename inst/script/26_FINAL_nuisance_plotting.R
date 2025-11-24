## Get plots of nuisance parameters

library(ggplot2)
library(dplyr)
library(readr)

devtools::load_all()

proper_negative_format <- function(x) {
  x <- as.character(x)
  x <- gsub("-", "\u2212", x)
  return(x)
}

plot_nuisance <- function(file) {
  file_name <- stringr::str_remove(fs::path_file(file), "\\..*$")
  name_parts <- stringr::str_split(file_name, "_")[[1]]

  nuisance_df <- read_csv(file)
  error <- name_parts[2]
  censoring_pct <- name_parts[4]
  print(error)

  if (error == "b"){
    true_haz_fn <- true_hazard_b
  } else if (error == "c") {
    true_haz_fn <- true_hazard_c
  } else if (error == "d") {
    true_haz_fn <- true_hazard_d
  }

  y_lims <- case_when(
    error == "b" ~ c(-2, 2),
    error == "c" ~ c(-3, 3),
    error == "d" ~ c(-2, 2.5)
  )
  x_lims <- case_when(
    error == "b" ~ c(0.573, 1.682),
    error == "c" ~ c(0.4147, 1.592),
    error == "d" ~ c(0.505, 1.718)
  )

  error_lab <- case_when(
    error == "b" ~ "a",
    error == "c" ~ "b",
    error == "d" ~ "c"
  )

  sampled_vals <- sample(unique(nuisance_df$sim_iter), size = 100)

  ests_df <- nuisance_df %>%
    filter(sim_iter %in% sampled_vals)

  avg_df <- ests_df %>%
    group_by(error, sample_size, censoring_prob, x) %>%
    summarize(value= mean(est_y)) %>%
    ungroup() %>%
    mutate(type = "Average Estimate")

  true_df <- data.frame(
    x = avg_df$x,
    name = "avg",
    type = "True Log(Hazard)",
    value = log(true_haz_fn(avg_df$x))
  )


  p <- ggplot() +
    geom_line(
      data = ests_df,
      aes(x = x, y = est_y, group = sim_iter, color = "Single Estimate", linetype = "Single Estimate"),  # Same label for color and linetype
      lwd = 0.5
    ) +
    geom_line(
      data = true_df,
      aes(x = x, y = value, color = "True Log(Hazard)", linetype = "True Log(Hazard)"),  # Same label for color and linetype
      lwd = 2
    ) +
    geom_line(
      data = avg_df,
      aes(x = x, y = value, color = "Average Estimate", linetype = "Average Estimate"),  # Same label for color and linetype
      lwd = 1
    ) +
    scale_color_manual(
      name = "Estimate Type",  # Unified legend title
      values = c("Single Estimate" = "lightblue", "Average Estimate" = "red", "True Log(Hazard)" = "black")
    ) +
    scale_linetype_manual(
      name = "Estimate Type",  # Unified legend title, same as color scale
      values = c("Single Estimate" = "solid", "True Log(Hazard)" = "dashed", "Average Estimate" = "solid")
    ) +
    theme_bw() +
    theme(
      text = element_text(family = "Times"),
      legend.title = element_blank(),
      legend.position = "none",
      legend.background = element_rect(color = "darkgrey"),
      plot.title = element_text(hjust = 0.5),
      axis.title.x = element_text(size = 15), # Adjust the size value as needed
      axis.title.y = element_text(size = 12),
      axis.text = element_text(size = 11)
    )  +
    labs(
      x = expression(epsilon),
      y = expression(log(lambda))
    ) +
    scale_x_continuous(labels = proper_negative_format, limits = x_lims) +
    scale_y_continuous(labels = proper_negative_format, limits = y_lims) +
    ggtitle(paste0("Error (", error_lab, "), ", censoring_pct, "% Censoring")) +
    coord_cartesian(expand = FALSE)

  return(p)
}

files <- fs::dir_ls("inst/updated_sim_res/nuisance")
files <- files[3:length(files)]

(plot_nuisance(files[1]) | plot_nuisance(files[2]) | plot_nuisance(files[3])) /
  (plot_nuisance(files[4]) | plot_nuisance(files[5]) | plot_nuisance(files[6])) /
  (plot_nuisance(files[7]) | plot_nuisance(files[8]) | plot_nuisance(files[9]))


