## Exploring some questions the model led to

library(dplyr)
library(ggplot2)

ad_clean <- readr::read_csv("inst/extdata/ad_clean.csv")

ad_data <- read_csv("inst/extdata/90plus_data.csv")


all_death <- ad_data %>% # start with 921 rows
  filter(
    !is.na(agedeath), # goes down to 779 rows
    dement_last == 0, # goes down to 357 rows
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
    t_scaled = t / (agedeath = 90),
    comorbidity = pmax(stroke, heartdis)
  ) %>%
  select(
    id, y, y_scaled, agedeath, t, t_scaled, delta, second_cohort,
    is_female, graduated_college,
    stroke, heartdis, comorbidity
  )

## Death-time distribution for subjects with a comorbidity

ad_clean %>%
  group_by(comorbidity) %>%
  summarize(
    avg_death = mean(agedeath),
    sd_death = sd(agedeath),
    n = n()
  )

all_death %>%
  group_by(comorbidity) %>%
  summarize(
    avg_death = mean(agedeath),
    sd_death = sd(agedeath),
    n = n()
  )

ad_clean %>%
  ggplot() +
  aes(x = agedeath) +
  geom_histogram() +
  facet_wrap(~comorbidity, scales = "free_y")

## Demographic comparison stratified by cohort
ad_clean %>%
  group_by(second_cohort) %>%
  summarize(
    avg_death = mean(agedeath),
    sd_death = sd(agedeath),
    avg_ad = mean(y),
    sd_ad = sd(y),
    graduated_college = mean(graduated_college),
    is_female = mean(is_female),
    comorbidity = mean(comorbidity)
  )

## Logistic regression model for AD

glm_mod_dat <- ad_data %>% # start with 921 rows
  filter(
    !is.na(agedeath), # goes down to 779 rows
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
    t_scaled = t / (agedeath = 90),
    comorbidity = pmax(stroke, heartdis)
  ) %>%
  select(
    id, y, y_scaled, agedeath, t, t_scaled, delta, second_cohort,
    is_female, graduated_college,
    stroke, heartdis, comorbidity
  )

glm_mod <- glm(
  delta ~ y + second_cohort + is_female + graduated_college + comorbidity,
  data = glm_mod_dat,
  family = "binomial"
)
summary(glm_mod)


ad_data %>% # start with 921 rows
  mutate(
    y = round(age_last, 1),
    delta = dement_last,
    t = age_first - 90,
    y_scaled = ifelse(delta, (y - 90) / (agedeath - 90), 1),
    stroke = ifelse(Stroke == 1 | TIA == 1, 1, 0),
    second_cohort = ifelse(cohort == 1, 0, 1),
    is_female = ifelse(Gender == 1, 1, 0),
    graduated_college = ifelse(Education >= 7, 1, 0),
    t_scaled = t / (agedeath = 90),
    comorbidity = pmax(stroke, heartdis)
  ) %>%
  group_by(cohort) %>%
  summarize(
    avg_death = mean(agedeath),
    sd_death = sd(agedeath),
    avg_age = mean(age_last),
    sd_age = sd(age_last),
    graduated_college = mean(graduated_college),
    is_female = mean(is_female),
    comorbidity = mean(comorbidity)
  )
