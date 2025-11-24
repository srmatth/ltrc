devtools::load_all()
library(dplyr)
library(ggplot2)
library(fs)


files <- dir_ls("inst/ltrc_sim_res_cv_200/")
files <- dir_ls("inst/ltrc_sim_res_cv_400/")
files <- dir_ls("inst/ltrc_sim_res_cv_800/")

#### Standard Normal ----

## Decision on number of knots (200, 400, 800): (1, 1, 1)

res <- readRDS(files[4])

cv_data <- data.frame()
for (i in 1:length(res)) {
  cv_data <- rbind(cv_data, res[[i]]$cv_results %>% mutate(dataset = i))
}

p1 <- cv_data %>%
  filter(avg_knot_lnlklhds > -1000) %>%
  ggplot() +
  aes(x = knots_tried, y = -avg_knot_lnlklhds) +
  stat_summary(fun = "median", geom = "point", size = 3) +
  stat_summary(fun = "median", geom = "line", linewidth = 0.5) +
  theme_bw() +
  xlab("Knots") +
  ylab("Loss") +
  scale_x_continuous(breaks = 1:10, minor_breaks = NULL) +
  ggtitle("(a)") +
  theme(
    text = element_text(family = "Times"),
    plot.title = element_text(hjust = 0.5)
  ) # save as 7x5 inch PDF

#### Standard Extreme Value ----

## Decision on Number of Knots (200, 400, 800): (1, 1, 2)

res <- readRDS(files[3])

cv_data <- data.frame()
for (i in 1:length(res)) {
  cv_data <- rbind(cv_data, res[[i]]$cv_results %>% mutate(dataset = i))
}

p2 <- cv_data %>%
  filter(avg_knot_lnlklhds > -1000) %>%
  ggplot() +
  aes(x = knots_tried, y = -avg_knot_lnlklhds) +
  stat_summary(fun = "median", geom = "point", size = 3) +
  stat_summary(fun = "median", geom = "line", linewidth = 0.5) +
  theme_bw() +
  xlab("Knots") +
  ylab("Loss") +
  scale_x_continuous(breaks = 1:10, minor_breaks = NULL) +
  ggtitle("(b)") +
  theme(
    text = element_text(family = "Times"),
    plot.title = element_text(hjust = 0.5)
  )

#### Centered Mixed Normal ----

## Decision on number of knots (200, 400, 800): (2, 3, 4)

res <- readRDS(files[1])

cv_data <- data.frame()
for (i in 1:length(res)) {
  cv_data <- rbind(cv_data, res[[i]]$cv_results %>% mutate(dataset = i))
}

p3 <- cv_data %>%
  filter(avg_knot_lnlklhds > -1000) %>%
  ggplot() +
  aes(x = knots_tried, y = -avg_knot_lnlklhds) +
  stat_summary(fun = "median", geom = "point", size = 3) +
  stat_summary(fun = "median", geom = "line", linewidth = 0.5) +
  theme_bw() +
  xlab("Loss") +
  scale_x_continuous(breaks = 1:10, minor_breaks = NULL) +
  ggtitle("(c)") +
  theme(
    text = element_text(family = "Times"),
    plot.title = element_text(hjust = 0.5)
  )


#### Non-Centered Mixed Normal ----

## Decision on number of knots (200, 400, 800): (2, 2, 3)

res <- readRDS(files[2])

cv_data <- data.frame()
for (i in 1:length(res)) {
  cv_data <- rbind(cv_data, res[[i]]$cv_results %>% mutate(dataset = i))
}

p4 <- cv_data %>%
  filter(avg_knot_lnlklhds > -1000) %>%
  ggplot() +
  aes(x = knots_tried, y = -avg_knot_lnlklhds) +
  stat_summary(fun = "median", geom = "point", size = 3) +
  stat_summary(fun = "median", geom = "line", linewidth = 0.5) +
  theme_bw() +
  xlab("Knots") +
  ylab("Loss") +
  scale_x_continuous(breaks = 1:10, minor_breaks = NULL) +
  ggtitle("(d)") +
  theme(
    text = element_text(family = "Times"),
    plot.title = element_text(hjust = 0.5)
  )

library(patchwork)

(p1 | p2) / (p3 | p4)

