## CHSA DAta Analysis

library(rio)
library(dplyr)
library(ltrc)
library(ggplot2)
devtools::load_all()

chsa <- import("~/Documents/research/Left Truncation/CHSA Data/chsa_823_20171101.xlsx", which = 1)

chsa_clean <- chsa %>%
  filter(VITAL != 8, lt_trunc > 0)

mod_dat <- chsa_clean %>%
  mutate(
    dx_2 = ifelse(FINALDX == 2, 1, 0),
    dx_4 = ifelse(FINALDX == 4, 1, 0),
    log_y = log(OBS_TIME),
    log_t = log(lt_trunc),
    is_female = ifelse(SEX == 2, 1, 0)
  ) %>%
  select(
    log_y,
    log_t,
    delta = CENSORX,
    diagnosis_num = FINALDX,
    dx_2,
    dx_4,
    age = AAO,
    is_female
  )

ltrc_mod <- ltrc(
  formula = survival::Surv(log_y, delta) ~ dx_4 + dx_2 + age + is_female,
  trunc_time = mod_dat$log_t,
  data = mod_dat,
  n_start = 10,
  int_knots = 1
)

ltrc_mod_age <- ltrc(
  formula = survival::Surv(log_y, delta) ~ age,
  trunc_time = mod_dat$log_t,
  data = mod_dat,
  n_start = 10,
  int_knots = 1
)

ltrc_mod_female <- ltrc(
  formula = survival::Surv(log_y, delta) ~ is_female,
  trunc_time = mod_dat$log_t,
  data = mod_dat,
  n_start = 10,
  int_knots = 1
)

ltrc_mod_inter <- ltrc(
  formula = survival::Surv(log_y, delta) ~ dx_4*age + dx_2*age + is_female,
  trunc_time = mod_dat$log_t,
  data = mod_dat,
  n_start = 10,
  int_knots = 1
)

ltrc_mod$theta
solve(ltrc_mod$efficient_score_info) %>% diag() %>% sqrt()

ltrc_mod_age$theta
solve(ltrc_mod_age$efficient_score_info) %>% diag() %>% sqrt()

ltrc_mod_female$theta
solve(ltrc_mod_female$efficient_score_info) %>% diag() %>% sqrt()


ltrc_mod_inter$theta
solve(ltrc_mod_inter$efficient_score_info) %>% diag() %>% sqrt()

## Demographic table values ----

chsa_clean %>%
  mutate(
    dx_name = case_when(
      FINALDX == 2 ~ "Probable AD",
      FINALDX == 3 ~ "Possible AD",
      FINALDX == 4 ~ "Vascular Dementia"
    ),
    educ_clean = ifelse(EDUC == 99, NA, EDUC),
    death_time = ifelse(CENSORX == 0, OBS_TIME / 365, NA)
  ) %>%
  group_by(dx_name) %>%
  summarize(
    n = n(),
    age = paste0(round(mean(AAO, na.rm = TRUE),1), " (", round(sd(AAO, na.rm = TRUE),1), ")"),
    censor = paste0(sum(CENSORX), " (", round(mean(CENSORX)*100, 1), "%)"),
    death = paste0(round(mean(death_time, na.rm = TRUE),1), " (", round(sd(death_time, na.rm = TRUE),1), ")"),
    female = paste0(sum(SEX == 2), " (", round(mean(SEX == 2)*100, 1), "%)"),
    male = paste0(sum(SEX == 1), " (", round(mean(SEX == 1)*100, 1), "%)"),
    educ = paste0(round(mean(educ_clean, na.rm = TRUE),1), " (", round(sd(educ_clean, na.rm = TRUE),1), ")"),
    educ_lt_8 = paste0(sum(educ_clean <= 8, na.rm = TRUE), " (", round(sum(educ_clean <= 8, na.rm = TRUE) / n() * 100, 1), "%)"),
    educ_gt_8 = paste0(sum(educ_clean > 8, na.rm = TRUE), " (", round(sum(educ_clean > 8, na.rm = TRUE) / n() * 100, 1), "%)"),
    educ_missing = paste0(sum(EDUC == 99), " (", round(mean(EDUC == 99)*100, 1), "%)")
  ) %>%
  View()




