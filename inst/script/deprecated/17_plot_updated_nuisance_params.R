library(ggplot2)
library(dplyr)
library(readr)

devtools::load_all()

# Function to format the axis labels
proper_negative_format <- function(x) {
  x <- as.character(x)
  x <- gsub("-", "\u2212", x)
  return(x)
}


## read in the file
nuisance_df <- read_csv("inst/updated_sim_res/nuisance/error_a_800_50.csv")

pct_subset <- nuisance_df

sampled_vals <- sample(unique(pct_subset$sim_iter), size = 100)

ests_df <- pct_subset %>%
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
  value = log(true_hazard_a(avg_df$x))
)


p_non <- ggplot() +
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
  scale_x_continuous(labels = proper_negative_format) +
  scale_y_continuous(labels = proper_negative_format, limits = c(-4, 1.5)) +
  ggtitle("(a) - 50% Censoring")

p_non

## read in the file
nuisance_df <- read_csv("inst/updated_sim_res/nuisance/error_b_800_50.csv")

pct_subset <- nuisance_df

sampled_vals <- sample(unique(pct_subset$sim_iter), size = 100)

ests_df <- pct_subset %>%
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
  value = log(true_hazard_b(avg_df$x))
)


p_b <- ggplot() +
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
  scale_x_continuous(labels = proper_negative_format) +
  scale_y_continuous(labels = proper_negative_format, limits = c(-3, 1)) +
  ggtitle("(b) - 50% Censoring")

p_b

## read in the file
nuisance_df <- read_csv("inst/updated_sim_res/nuisance/error_c_800_50.csv")

pct_subset <- nuisance_df

sampled_vals <- sample(unique(pct_subset$sim_iter), size = 100)

ests_df <- pct_subset %>%
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
  value = log(true_hazard_c(avg_df$x))
)


p_c <- ggplot() +
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
  scale_x_continuous(labels = proper_negative_format) +
  scale_y_continuous(labels = proper_negative_format, limits = c(-5, 1)) +
  ggtitle("(c) - 50% Censoring")

p_c


## read in the file
nuisance_df <- read_csv("inst/updated_sim_res/nuisance/error_d_800_50.csv")

pct_subset <- nuisance_df

sampled_vals <- sample(unique(pct_subset$sim_iter), size = 100)

ests_df <- pct_subset %>%
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
  value = log(true_hazard_d(avg_df$x))
)


p_e <- ggplot() +
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
  scale_x_continuous(labels = proper_negative_format) +
  scale_y_continuous(labels = proper_negative_format, limits = c(-3, 1.5)) +
  ggtitle("(d) - 50% Censoring")

p_e


## Check and see if 15% censoring does any better

## read in the file
nuisance_df <- read_csv("inst/updated_sim_res/nuisance/error_a_800_15.csv")

pct_subset <- nuisance_df

sampled_vals <- sample(unique(pct_subset$sim_iter), size = 100)

ests_df <- pct_subset %>%
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
  value = log(true_hazard_a(avg_df$x))
)


p_a_15 <- ggplot() +
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
  scale_x_continuous(labels = proper_negative_format) +
  scale_y_continuous(labels = proper_negative_format, limits = c(-4, 1.5)) +
  ggtitle("(a) - 15% Censoring")

p_a_15

## read in the file
nuisance_df <- read_csv("inst/updated_sim_res/nuisance/error_b_800_15.csv")

pct_subset <- nuisance_df

sampled_vals <- sample(unique(pct_subset$sim_iter), size = 100)

ests_df <- pct_subset %>%
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
  value = log(true_hazard_b(avg_df$x))
)


p_b_15 <- ggplot() +
  geom_line(
    data = ests_df,
    aes(x = x-1, y = est_y, group = sim_iter, color = "Single Estimate", linetype = "Single Estimate"),  # Same label for color and linetype
    lwd = 0.5
  ) +
  geom_line(
    data = true_df,
    aes(x = x-1, y = value, color = "True Log(Hazard)", linetype = "True Log(Hazard)"),  # Same label for color and linetype
    lwd = 2
  ) +
  geom_line(
    data = avg_df,
    aes(x = x-1, y = value, color = "Average Estimate", linetype = "Average Estimate"),  # Same label for color and linetype
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
  scale_y_continuous(labels = proper_negative_format, limits = c(-3, 1)) +
  ggtitle("(b) - 15% Censoring")

p_b_15

## read in the file
nuisance_df <- read_csv("inst/updated_sim_res/nuisance/error_c_800_15.csv")

pct_subset <- nuisance_df

sampled_vals <- sample(unique(pct_subset$sim_iter), size = 100)

ests_df <- pct_subset %>%
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
  value = log(true_hazard_c(avg_df$x))
)


p_c_15 <- ggplot() +
  geom_line(
    data = ests_df,
    aes(x = x-1, y = est_y, group = sim_iter, color = "Single Estimate", linetype = "Single Estimate"),  # Same label for color and linetype
    lwd = 0.5
  ) +
  geom_line(
    data = true_df,
    aes(x = x-1, y = value, color = "True Log(Hazard)", linetype = "True Log(Hazard)"),  # Same label for color and linetype
    lwd = 2
  ) +
  geom_line(
    data = avg_df,
    aes(x = x-1, y = value, color = "Average Estimate", linetype = "Average Estimate"),  # Same label for color and linetype
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
  scale_y_continuous(labels = proper_negative_format, limits = c(-5, 1)) +
  ggtitle("(c) - 15% Censoring")

p_c_15

## read in the file
nuisance_df <- read_csv("inst/updated_sim_res/nuisance/error_d_800_15.csv")

pct_subset <- nuisance_df

sampled_vals <- sample(unique(pct_subset$sim_iter), size = 100)

ests_df <- pct_subset %>%
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
  value = log(true_hazard_d(avg_df$x))
)


p_e_15 <- ggplot() +
  geom_line(
    data = ests_df,
    aes(x = x-1, y = est_y, group = sim_iter, color = "Single Estimate", linetype = "Single Estimate"),  # Same label for color and linetype
    lwd = 0.5
  ) +
  geom_line(
    data = true_df,
    aes(x = x-1, y = value, color = "True Log(Hazard)", linetype = "True Log(Hazard)"),  # Same label for color and linetype
    lwd = 2
  ) +
  geom_line(
    data = avg_df,
    aes(x = x-1, y = value, color = "Average Estimate", linetype = "Average Estimate"),  # Same label for color and linetype
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
  scale_y_continuous(labels = proper_negative_format, limits = c(-3, 1.5)) +
  ggtitle("(d) - 15% Censoring")

p_e_15


library(patchwork)


(p_non / p_b / p_c / p_e) | (p_a_15 / p_b_15 / p_c_15 / p_e_15)




