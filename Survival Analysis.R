install.packages("tidyverse")
install.packages("survival")
install.packages("survminer")
install.packages("survdiff")

library("tidyverse")
library("survival")
library("survminer")
library("dplyr")
library("tidyr")
library("survdiff")

# Plotting options
options(repr.plot.width = 16, repr.plot.height = 7)

# Read data
all_df = read.csv('E:/PGPDS/Project/heart_failure_clinical_records_dataset.csv')
all_df
# Sample 3 + 3 points from death/censored groups, and plot them for comparison
plot_censoring = all_df %>% group_by(DEATH_EVENT) %>% sample_n(3) %>% ungroup() %>% select(time, DEATH_EVENT)

plot_censoring %>%
  mutate(
    time_start = 0, 
    case_id = factor(c(1:nrow(plot_censoring))),
    death_event = factor(ifelse(DEATH_EVENT == 1, "death", "censored"))
  ) %>%
  pivot_longer(
    cols = c(time, time_start),
    names_to = "source",
    values_to = "time"
  ) %>%
  ggplot(aes(x = time, y = case_id, group = factor(case_id))) + 
  geom_bar(stat = "Identity", aes(fill = death_event), colour = "black", width = 0.3) +
  ggtitle("Time till Death/Censoring - 6 sampled cases from dataset") + 
  theme(plot.title = element_text(size = 22), 
        legend.title = element_text(size = 18), 
        legend.text = element_text(size = 16), 
        axis.title = element_text(size = 16))



# Censoring vs Death
options(repr.plot.width = 10, repr.plot.height = 4)

all_df %>%
  mutate(event_type = factor(ifelse(DEATH_EVENT == 1, "death", "censored"))) %>%
  group_by(event_type) %>%
  tally(name = "count") %>%
  ggplot(aes(x = event_type, y = count)) + 
  geom_bar(stat = "Identity", fill = "blue", width = 0.2, colour = "black") +
  ggtitle("Censored vs Deaths") + 
  theme(plot.title = element_text(size = 22),  
        axis.title = element_text(size = 16))


# Kaplan-meier analysis
km_model <- survfit(Surv(time, DEATH_EVENT) ~ 1, data = all_df)
summary(km_model, times = seq(from = 0, to = 290, by = 30))

# Plot Kaplan-Meier plot
options(repr.plot.width = 18, repr.plot.height = 8)

ggsurvplot(km_model, data = all_df, risk.table = TRUE, 
           break.time.by = 10, size = 0.3, tables.height = 0.15)


# Kaplan-Meier curve based on presence/absence of smoking
km_model <- all_df %>%
  mutate(
    smoking = factor(ifelse(smoking == 0, "non-smoker", "smoker"))
  ) %>%
  survfit(Surv(time, DEATH_EVENT) ~ smoking, data = .)

ggsurvplot(km_model, data = all_df, risk.table = TRUE, 
           break.time.by = 10, size = 0.3, tables.height = 0.20)


# Kaplan-Meier curve based on presence/absence of high_blood_pressure
km_model <- all_df %>%
  mutate(
    high_blood_pressure = factor(ifelse(high_blood_pressure == 0, "Normal BP", "High BP"))
  ) %>%
  survfit(Surv(time, DEATH_EVENT) ~ high_blood_pressure, data = .)

ggsurvplot(km_model, data = all_df, risk.table = TRUE, 
           break.time.by = 10, size = 0.3, tables.height = 0.20)

# Kaplan-Meier curve based on presence/absence of Diabetes
km_model <- all_df %>%
  mutate(
    diabetes = factor(ifelse(diabetes == 0, "Normal Sugar Level", "High Sugar Level"))
  ) %>%
  survfit(Surv(time, DEATH_EVENT) ~ diabetes, data = .)

ggsurvplot(km_model, data = all_df, risk.table = TRUE, 
           break.time.by = 10, size = 0.3, tables.height = 0.20)


#Log-Rank Test between sex, smoking, diabetes and blood_pressure
survdiff(Surv(time, DEATH_EVENT) ~ sex,data=all_df)
survdiff(Surv(time, DEATH_EVENT) ~ smoking,data=all_df)
survdiff(Surv(time, DEATH_EVENT) ~ diabetes,data=all_df)
survdiff(Surv(time, DEATH_EVENT) ~ high_blood_pressure,data=all_df)


# Change columns into factors and scale columns to enable better model fit
all_df <- all_df %>% 
  mutate(
    anaemia = factor(ifelse(anaemia == 1, "anaemic", "non-anaemic"), levels = c("non-anaemic", "anaemic")),
    diabetes = factor(ifelse(diabetes == 1, "diabetic", "non-diabetic"), levels = c("non-diabetic", "diabetic")),
    high_blood_pressure = factor(ifelse(high_blood_pressure == 1, "high-bp", "non-high-bp"), levels = c("non-high-bp", "high-bp")),
    sex = factor(ifelse(sex == 0, "female", "male"), levels = c("female", "male")),
    smoking = factor(ifelse(smoking == 0, "non-smoker", "smoker"), levels = c("non-smoker", "smoker")),
    platelets = platelets/1e4, 
    creatinine_phosphokinase = creatinine_phosphokinase/1e3
  )

all_df %>% head


# Cox proportional hazard model
cox_model <- coxph(Surv(time, DEATH_EVENT) ~ age + anaemia + creatinine_phosphokinase + diabetes + ejection_fraction + 
                     high_blood_pressure + platelets + smoking + sex, 
                   data = all_df)
summary(cox_model)

# Plot the survival for a population with mean value of covariates
ggsurvplot(survfit(cox_model), data = all_df, risk.table = TRUE, break.time.by = 10)



# A data-set with 2 rows. 1 row per factor level. Numerical covariates are set to median value.
compare_smoking_median_age <- tibble(
  age = rep(median(all_df$age), 2), 
  anaemia = factor(c("anaemic", "non-anaemic"), levels = levels(all_df$anaemia)),
  creatinine_phosphokinase = rep(median(all_df$creatinine_phosphokinase), 2),
  diabetes = factor(c("diabetic", "non-diabetic"), levels = levels(all_df$diabetes)),
  ejection_fraction = rep(median(all_df$ejection_fraction), 2),
  high_blood_pressure = factor(c("high-bp", "non-high-bp"), levels = levels(all_df$high_blood_pressure)),
  platelets = rep(median(all_df$platelets), 2), 
  smoking = factor(c("smoker", "non-smoker"), levels = levels(all_df$smoking)), 
  sex = factor(c("male", "female"), levels = levels(all_df$sex)),
)

compare_smoking_median_age

#Compare 2 groups of population
ggsurvplot(survfit(cox_model, data = compare_smoking_median_age, newdata = compare_smoking_median_age), conf.int = TRUE)

#Testing the Proportional Hazard Assumption
cox.zph(cox_model)

