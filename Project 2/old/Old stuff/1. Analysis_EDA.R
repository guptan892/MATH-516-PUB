#PLAN: 
#Can check for AR1/ARH1 using some other graph - is there autocorrelation in residuals
#REML vs ML
#treatment of NaN rows
#residuals --> covariance matrix?
#factorization of session vs day? 

# Overarching question: 
# is there significant inter- and intra-individual variability in heat flow measurements in each of the sensors? 
# -->inter/intra-individual variability in heat flow measurements across sessions, days, activities, and people.
#--> how do variables (person (inter), session, day (temp), activity) effect heat flow measurements??? Does this depend on sensor?
# How are measurements from different sensors related? 
# --> tactically, ????

#FIXED EFFECTS: session, day (temp), activity, sensor, sex (related to person?)
#RANDOM EFFECTS: person (random intercept!)
#test: should session be random (diff people have diff exhaustion rates)? --> random slope!
#another test: could do interaction variables? 
#-->sex*activity means that , sex*session, sex*day, activity*session, activity*day


#1. EDA - note follow up qs re missing data
#2: motivate use of LMM --> independence of subjects (inter) but not within subject (intra) --> how prove inter?
#3: fit LMM
# INTRA:
# - Q1: How does heat flow change for the same person between day 1 and day 2? --> temp
# - Q2: How does heat flow change between session 1 and session 4, doing the same activity?
#--->Do we need random effect? for person yes?, but for 

#variables: activity, day(temperature), session(time), sensor, sex, person
#base model: no random effect
#model 0: random effect = 1 | person
#model 1: random effect = 1 + activity | person --> is activity correlated?
#model 2: random effect = sex

# INTER:
# - Q3: How does heat flow change across participants, for same session, day, and activity? (note to self: look at gender)


#EDA FOLLOW UP QS:
#NOTE: SHOULD DO SOME FOLLOW UP ANALYSIS ON WHERE NAs ARE, ENSURE FEEL GOOD?
#NOTE: SHOULD I DO A SPAGHETTI PLOT FOR THE RESIDUALS??

#Q1:
#how to model this? do I need to motivate 







# Section 1: Import Libraries
library(tidyverse) #installed tidyverse
library(nlme)
library(ggplot2)
library(here)

# Load the dataset
file_path <- here("analysis_and_plots/heat_flow_ICE_EPFL.csv")
df <- read.csv(file_path)

# Section 2: EDA
# General takeaways from step 0: 
# 6 sensors, 24 people (12M/12F), 2 days, 4 sessions, 3 activities
# Not all participants have data for all sessions/activities/sensors (some missing values)
# so removed rows with NA in HF_value

# Remove NaNs before fitting the models
df_clean <- df %>% filter(!is.na(HF_value)) #remove rows with NA in HF_value
print(dim(df_clean)) #2638 rows after removing NAs
print(dim(df)) #2736 rows 

# Q0: Are heat flow measures independent? slide 17, motivate LMM

# Base model with all main effects, no interactions, no person random effect
lm_base <- lm(HF_value ~ factor(session) + factor(activity)+ factor(HF_type) + factor(day) + factor(sex), data = df_clean)
df_clean$resids <- residuals(lm_base)

# Pivot data to compare Session 1 vs Session 4 residuals
df_pivot_narem <- df_clean %>%
  filter(session %in% c(1, 4)) %>%
  pivot_wider(
    id_cols = c(person, day, activity, HF_type),
    names_from = session,
    values_from = resids,
    names_prefix = "res_s"
  ) %>%
  filter(!is.na(res_s1) & !is.na(res_s4)) # Remove rows where one session is missing

# Plot residuals of Session 1 vs Session 4 , by sensor and activity, colored by person to check for independence of measures across sessions
residuals_1_4_narem = ggplot(df_pivot_narem, aes(x = res_s1, y = res_s4, color = factor(person))) +
  geom_point(alpha = 0.7, size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") + 
  facet_grid(day ~ activity) +
  labs(
    title = "Residual Correlation: Session 1 vs Session 4",
    subtitle = "By Sensor and Activity | Colors represent individual participants",
    x = "Residuals from Session 1",
    y = "Residuals from Session 4",
    color = "Person"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("analysis_and_plots/residuals_sess1vs4_narem_bydayandact.png") 


# Pivot data to compare Session 1 vs Session 4 residuals, not deleting rows for na
df_pivot <- df_clean %>%
  filter(session %in% c(1, 4)) %>%
  pivot_wider(
    id_cols = c(person, day, activity, HF_type),
    names_from = session,
    values_from = resids,
    names_prefix = "res_s"
  )

# Plot residuals of Session 1 vs Session 4 , by sensor and activity, colored by person to check for independence of measures across sessions
residuals_1_4 = ggplot(df_pivot, aes(x = res_s1, y = res_s4, color = factor(person))) +
  geom_point(alpha = 0.7, size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") + 
  facet_grid(day ~ activity) +
  labs(
    title = "Residual Correlation: Session 1 vs Session 4",
    subtitle = "By Sensor and Activity | Colors represent individual participants",
    x = "Residuals from Session 1",
    y = "Residuals from Session 4",
    color = "Person"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("analysis_and_plots/residuals_sess1vs4_bydayandact.png") 



# Pivot data to compare Day 1 vs Day 2 residuals
df_pivot_day_narem <- df_clean %>%
  pivot_wider(
    id_cols = c(person, session, activity, HF_type),
    names_from = day,
    values_from = resids,
    names_prefix = "res_day"
  ) %>%
  # Remove cases where data is missing for one of the two days
  filter(!is.na(res_day1) & !is.na(res_day2))

residuals_day1_2_narem <- ggplot(df_pivot_day_narem, aes(x = res_day1, y = res_day2, color = factor(person))) +
  geom_point(alpha = 0.7, size = 2) +
  # Dashed line y=x represents perfect correlation
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") + 
  facet_grid(session ~ activity) +
  labs(
    title = "Residual Correlation: Day 1 vs Day 2",
    subtitle = "Checking for independence across different temperature environments",
    x = "Residuals from Day 1 (24°C)",
    y = "Residuals from Day 2 (18°C)",
    color = "Person"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave("analysis_and_plots/residuals_days_narem_bysessandact.png") 




#dont remove nas
# Pivot data to compare Day 1 vs Day 2 residuals
df_pivot_day <- df_clean %>%
  pivot_wider(
    id_cols = c(person, session, activity, HF_type),
    names_from = day,
    values_from = resids,
    names_prefix = "res_day"
  )

residuals_day1_2 <- ggplot(df_pivot_day, aes(x = res_day1, y = res_day2, color = factor(person))) +
  geom_point(alpha = 0.7, size = 2) +
  # Dashed line y=x represents perfect correlation
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") + 
  facet_grid(session ~ activity) +
  labs(
    title = "Residual Correlation: Day 1 vs Day 2",
    subtitle = "Checking for independence across different temperature environments",
    x = "Residuals from Day 1 (24°C)",
    y = "Residuals from Day 2 (18°C)",
    color = "Person"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave("analysis_and_plots/residuals_days_bysessandact.png") 



#Removing NAs results in a cleaner view, so do that.
