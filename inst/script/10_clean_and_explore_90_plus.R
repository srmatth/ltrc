# First look at the data

library(dplyr)
library(readr)
library(ggplot2)

ad_data <- read_csv("inst/extdata/90plus_data.csv")


ad_clean <- ad_data %>% # start with 921 rows
  filter(
    !is.na(agedeath), # goes down to 779 rows
    dement_last == 1, # goes down to 357 rows
  ) %>%
  mutate(
    y = round(age_last, 1),
    delta = dement_last,
    t = age_first - 90,
    y_scaled = ifelse(delta, (y - 90) / (agedeath - 90), 1),
    stroke = ifelse(Stroke == 1 | TIA == 1, 1, 0),
    second_cohort = ifelse(cohort == 1, 0, 1),
    is_female = ifelse(Gender == 1, 1, 0),
    graduated_college = ifelse(Education >= 7, 1, 0),
    t_scaled = t / (agedeath - 90),
    comorbidity = pmax(stroke, heartdis)
  ) %>%
  filter(
    y_scaled != 1, # goes down to 349 rows
  ) %>%
  select(
    id, y, y_scaled, agedeath, t, t_scaled, delta, second_cohort,
    is_female, graduated_college,
    stroke, heartdis, comorbidity
  )

# mean(ad_clean$delta == 0) # 54.17% Censoring
sum(ad_clean$y_scaled == 1)

summary(ad_clean$y_scaled)
summary(ad_clean$t)
sd(ad_clean$t)
summary(ad_clean$second_cohort)
summary(ad_clean$is_female)
summary(ad_clean$graduated_college)
summary(ad_clean$stroke)
summary(ad_clean$heartdis)
summary(ad_clean$t_scaled)

summary(ad_clean$t)
sd(ad_clean$t)
summary(ad_clean$y)
sd(ad_clean$y)
summary(ad_clean$agedeath)
sd(ad_clean$agedeath)
summary(ad_clean$y_scaled)
sd(ad_clean$y_scaled)
sum(ad_clean$second_cohort == 0)
mean(ad_clean$second_cohort == 0)
sum(ad_clean$second_cohort)
mean(ad_clean$second_cohort)
sum(ad_clean$is_female == 0)
mean(ad_clean$is_female == 0)
sum(ad_clean$is_female)
mean(ad_clean$is_female)
sum(ad_clean$graduated_college == 0)
mean(ad_clean$graduated_college == 0)
sum(ad_clean$graduated_college)
mean(ad_clean$graduated_college)
sum(ad_clean$stroke == 0)
mean(ad_clean$stroke == 0)
sum(ad_clean$stroke)
mean(ad_clean$stroke)
sum(ad_clean$heartdis == 0)
mean(ad_clean$heartdis == 0)
sum(ad_clean$heartdis)
mean(ad_clean$heartdis)
sum(ad_clean$comorbidity)
mean(ad_clean$comorbidity)

ad_clean %>%
  ggplot() +
  aes(x = y_scaled) +
  geom_histogram(binwidth = 0.05) +
  theme_bw() +
  xlab("(AD - 90) / (Death - 90)") +
  ylab("Count")

ad_clean %>%
  ggplot() +
  aes(x = log(y_scaled / (1-y_scaled))) +
  geom_histogram(binwidth = 0.25) +
  theme_bw() +
  xlab("log(y / (1-y))") +
  ylab("Count")


# Save the clean_ad data
readr::write_csv(ad_clean, "inst/extdata/ad_clean.csv")

### Fit the model ----

devtools::load_all()

mod_dat <- ad_clean %>%
  mutate(
    mod_y = log(y_scaled / (1-y_scaled)),
    mod_t = log(t_scaled / (1 - t_scaled)),
    mod_death = agedeath - 90
  )

cv_fit <- ltrc(
  survival::Surv(mod_y, delta) ~ second_cohort + is_female + graduated_college + stroke + heartdis,
  trunc_time = mod_dat$mod_t, data = mod_dat,
  n_start = 10, int_knots = -1,return_all = TRUE
)

ggplot() +
  aes(x = 1:2, y = cv_fit$knot_lnlklhds[1:2]) +
  geom_line() +
  geom_point()

mod_fit <- ltrc(
  survival::Surv(mod_y, delta) ~ mod_death + second_cohort + is_female + graduated_college + comorbidity,
  trunc_time = mod_dat$mod_t, data = mod_dat,
  n_start = 10, int_knots = 2,return_all = TRUE
)
betas <- mod_fit$theta[1:5]
se <- (solve(mod_fit$inform) %>% diag() %>% sqrt())[1:5]

exp(betas)
exp(betas - 1.96 * se)
exp(betas + 1.96 * se)

2*pnorm(-abs(betas), 0, se)



thetas <- matrix(NA, nrow = 10, ncol = 11)
theta_path <- data.frame()
for (i in 1:length(mod_fit$all_res)) {
  sub_res <- mod_fit$all_res[[i]]
  theta_path <- theta_path %>%
    rbind(data.frame(
      random_start = i,
      beta = paste0("beta[", 1:5, "]"),
      sd = se,
      final_est = betas,
      param_start = sub_res$theta0[1:5],
      param_end = sub_res$theta[1:5]
    ))
  thetas[i,] <- sub_res$theta
}

# Function to format the axis labels
proper_negative_format <- function(x) {
  x <- as.character(x)
  x <- gsub("-", "\u2212", x)
  return(x)
}

theta_path %>%
  mutate(
    converged = ifelse(param_end > final_est - 2*sd & param_end < final_est + 2*sd, 1, 0)
  ) %>%
  ggplot() +
  geom_point(
    aes(x = param_start, y = random_start, color = as.factor(converged))
  ) +
  geom_segment(
    aes(x = param_start, xend = param_end, y = random_start, yend = random_start, color = as.factor(converged)),
    arrow = arrow(length = unit(0.1, "in"))
  ) +
  geom_vline(
    aes(xintercept = final_est),
    lty = "dashed"
  ) +
  facet_wrap(~beta, labeller = label_parsed) +
  coord_cartesian(xlim=c(-1, 1)) +
  scale_y_continuous(
    breaks = 1:10, minor_breaks = NULL
  ) +
  xlab("Parameter Starting Value") +
  ylab("Random Start") +
  theme_bw() +
  theme(
    text = element_text(family = "Times"),
    legend.title = element_blank(),
    legend.position = "none",
    legend.background = element_rect(color = "darkgrey"),
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(size = 14), # Adjust the size value as needed
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12)
  ) +
  scale_color_manual(values = c("red", "darkgreen")) +
  scale_x_continuous(labels = proper_negative_format)


mod_dat %>%
  ggplot() +
  aes(x = second_cohort, y = mod_y) +
  geom_point()

naive_mod <- lm(mod_y ~ mod_death + second_cohort + is_female + graduated_college + comorbidity, data = mod_dat)

summary(naive_mod)

gamma <- mod_fit$theta[6:11]

mod_fit$basis_internal_knots <- mod_fit$knots
mod_fit$basis_boundary_knots <- mod_fit$boundary_knots
mod_fit$gamma <- gamma
x <- seq(-2, 4, by = 0.01)
y <- log(predict_hazard(mod_fit, x))

resid_haz <- ggplot() +
  geom_line(
    aes(x = x, y = y)
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "Times"),
    plot.title = element_text(hjust = 0.5)
  ) +
  xlab("Residuals") +
  labs(y = expression(log(lambda))) +
  ggtitle("(c)") +
  scale_x_continuous(labels = proper_negative_format) +
  scale_y_continuous(labels = proper_negative_format)




## Get the residuals and predicted values
mod_fit$beta <- mod_fit$theta[1:5]

X <- mod_dat %>%
  select(mod_death, second_cohort, is_female, graduated_college, comorbidity) %>%
  as.matrix()
pred_vals <- X %*% matrix(mod_fit$beta, ncol = 1)

resids <- mod_dat$mod_y - pred_vals
resid_hist <- ggplot() +
  aes(x = resids) +
  geom_histogram(binwidth = 0.25) +
  theme_bw() +
  theme(
    text = element_text(family = "Times"),
    plot.title = element_text(hjust = 0.5)
  ) +
  xlab("Residuals") +
  ylab("Count") +
  ggtitle("(a)") +
  scale_x_continuous(labels = proper_negative_format) +
  scale_y_continuous(labels = proper_negative_format)

gamma
mean(resids)

resid_scatter <- ggplot() +
  aes(x = pred_vals, y = resids) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  theme(
    text = element_text(family = "Times"),
    plot.title = element_text(hjust = 0.5)
  ) +
  xlab("Predicted Values") +
  ylab("Residuals") +
  ggtitle("(b)") +
  scale_x_continuous(labels = proper_negative_format) +
  scale_y_continuous(labels = proper_negative_format)

library(patchwork)

(resid_hist | resid_scatter) / resid_haz

