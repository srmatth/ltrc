## Plotting the nuisance parameters

devtools::load_all()
library(ggplot2)
library(dplyr)
library(fs)

dir_path <- "inst/ltrc_sim_res_FINAL/"
files <- dir_ls(dir_path)

# Function to format the axis labels
proper_negative_format <- function(x) {
  x <- as.character(x)
  x <- gsub("-", "\u2212", x)
  return(x)
}

order <- c(10,11,12,7,8,9,1,2,3,4,5,6)
files <- files[order]

#### Standard Normal Error ----

sim_res <- readRDS(files[3])

x <- seq(-3, 3, by = 0.01)
nuisance_df <- matrix(NA, nrow = length(sim_res), ncol = length(x))
for (i in 1:length(sim_res)) {
  nuisance_df[i,] <- log(predict_hazard(sim_res[[i]], x))
}

# plot(x, log(true_hazard_a(x)), lwd = 6, type = "l", lty = "dashed",
#      xlab = "Epsilon", ylab = "log(Hazard)", main = "Standard Normal Error, 800 Obs")
#
# for ( i in sample(1:nrow(nuisance_df), size = 100, replace = FALSE)) {
#   lines(x, nuisance_df[i,], col = "lightblue")
# }
#
# avg_hazard <- colSums(nuisance_df) / nrow(nuisance_df)
# lines(x, log(true_hazard_a(x)), lwd = 6, type = "l", lty = "dashed")
# lines(x, avg_hazard, type = "l", col = "blue", lwd = 4)

sampled_vals <- sample(1:1000, size = 100)
ests_df <- nuisance_df %>%
  as.data.frame() %>%
  tidyr::pivot_longer(
    cols = everything()
  ) %>%
  mutate(
    row_name = rep(1:1000, each = length(x)),
    x = rep(x, 1000),
    type = "Single Estimate"
  ) %>%
  filter(row_name %in% sampled_vals)

avg_df <- ests_df %>%
  group_by(name, type) %>%
  summarize(x= mean(x), value = mean(value)) %>%
  ungroup() %>%
  mutate(type = "Average Estimate")

true_df <- data.frame(
  x = x,
  name = "avg",
  type = "True Log(Hazard)",
  value = log(true_hazard_a(x))
)

p_norm <- ggplot() +
  geom_line(
    data = ests_df,
    aes(x = x, y = value, group = row_name, color = "Single Estimate", linetype = "Single Estimate"),  # Same label for color and linetype
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
  scale_x_continuous(labels = proper_negative_format) +
  scale_y_continuous(labels = proper_negative_format) +
  ggtitle("(a)") # save as a pdf, 5x3.5 inches

p_norm

#### Standard EV Error ----

sim_res <- readRDS(files[6])

x <- seq(-1.5, 4, by = 0.01)
nuisance_df <- matrix(NA, nrow = length(sim_res), ncol = length(x))
for (i in 1:length(sim_res)) {
  nuisance_df[i,] <- log(predict_hazard(sim_res[[i]], x))
}

sampled_vals <- sample(1:1000, size = 100)
ests_df <- nuisance_df %>%
  as.data.frame() %>%
  tidyr::pivot_longer(
    cols = everything()
  ) %>%
  mutate(
    row_name = rep(1:1000, each = length(x)),
    x = rep(x, 1000),
    type = "Single Estimate"
  ) %>%
  filter(row_name %in% sampled_vals)

avg_df <- ests_df %>%
  group_by(name, type) %>%
  summarize(x= mean(x), value = mean(value)) %>%
  ungroup() %>%
  mutate(type = "Average Estimate")

true_df <- data.frame(
  x = x,
  name = "avg",
  type = "True Log(Hazard)",
  value = log(true_hazard_b(x))
)


p_ev <- ggplot() +
  geom_line(
    data = ests_df,
    aes(x = x, y = value, group = row_name, color = "Single Estimate", linetype = "Single Estimate"),  # Same label for color and linetype
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
  scale_x_continuous(labels = proper_negative_format) +
  scale_y_continuous(labels = proper_negative_format) +
  ggtitle("(b)")

p_ev
#### Centered Mixed Normal Error ----

sim_res <- readRDS(files[9])

x <- seq(-5, 5, by = 0.01)
nuisance_df <- matrix(NA, nrow = length(sim_res), ncol = length(x))
for (i in 1:length(sim_res)) {
  nuisance_df[i,] <- log(predict_hazard(sim_res[[i]], x))
}

sampled_vals <- sample(1:1000, size = 100)
ests_df <- nuisance_df %>%
  as.data.frame() %>%
  tidyr::pivot_longer(
    cols = everything()
  ) %>%
  mutate(
    row_name = rep(1:1000, each = length(x)),
    x = rep(x, 1000),
    type = "Single Estimate"
  ) %>%
  filter(row_name %in% sampled_vals)

avg_df <- ests_df %>%
  group_by(name, type) %>%
  summarize(x= mean(x), value = mean(value)) %>%
  ungroup() %>%
  mutate(type = "Average Estimate")

true_df <- data.frame(
  x = x,
  name = "avg",
  type = "True Log(Hazard)",
  value = log(true_hazard_c(x))
)


p_mixed <- ggplot() +
  geom_line(
    data = ests_df,
    aes(x = x, y = value, group = row_name, color = "Single Estimate", linetype = "Single Estimate"),  # Same label for color and linetype
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
  scale_x_continuous(labels = proper_negative_format) +
  scale_y_continuous(labels = proper_negative_format) +
  ggtitle("(c)")

p_mixed

#### Non-Centered Mixed Normal Error ----

sim_res <- readRDS(files[12])

x <- seq(-2, 1.8, by = 0.01)
nuisance_df <- matrix(NA, nrow = length(sim_res), ncol = length(x))
for (i in 1:length(sim_res)) {
  nuisance_df[i,] <- log(predict_hazard(sim_res[[i]], x))
}

sampled_vals <- sample(1:1000, size = 100)
ests_df <- nuisance_df %>%
  as.data.frame() %>%
  tidyr::pivot_longer(
    cols = everything()
  ) %>%
  mutate(
    row_name = rep(1:1000, each = length(x)),
    x = rep(x, 1000),
    type = "Single Estimate"
  ) %>%
  filter(row_name %in% sampled_vals)

avg_df <- ests_df %>%
  group_by(name, type) %>%
  summarize(x= mean(x), value = mean(value)) %>%
  ungroup() %>%
  mutate(type = "Average Estimate")

true_df <- data.frame(
  x = x,
  name = "avg",
  type = "True Log(Hazard)",
  value = log(true_hazard_d(x))
)


p_non <- ggplot() +
  geom_line(
    data = ests_df,
    aes(x = x, y = value, group = row_name, color = "Single Estimate", linetype = "Single Estimate"),  # Same label for color and linetype
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
  scale_x_continuous(labels = proper_negative_format) +
  scale_y_continuous(labels = proper_negative_format) +
  ggtitle("(d)")

p_non


library(patchwork)

(p_norm + p_ev) / (p_mixed + p_non)
