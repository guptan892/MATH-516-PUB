# Section 1: Import Libraries
library(tidyverse) #installed tidyverse
library(nlme)
library(ggplot2)
library(here)
library(kableExtra)
library(modelsummary)

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




#Section 2:
#note: can use REML to copmare across same fixed effects, ML otherwise.
#start with ML.
#Model 0: no random effects
#Model 1A-1X: random intercept model + add nested intercepts
#Model 2A-2X: add random slopes
#Model 4A-4X: check interaction??
#Model 5: check covariance structures??
#testing if variance is 0 --> divide anova pval by 2? (slide 39) --> keep if significant or if AIC dec is large


#model 0: no random effects
#gls - lm is the same but uses OLS not ML/REML like gls and lme do
mdl0_no_random <- gls(
  HF_value ~ factor(day) + factor(session) + factor(activity) + factor(HF_type) + factor(sex), 
  data   = df_clean,
  method = "ML"
)

# #test impact of each fixed effect
# #01a: session
# mdl0_no_random_session <- gls(
#   HF_value ~  factor(session), 
#   data   = df_clean,
#   method = "ML"
# )
# #01b: activity
# mdl0_no_random_session <- gls(
#   HF_value ~  factor(activity), 
#   data   = df_clean,
#   method = "ML"
# )
# #01c: sensor
# mdl0_no_random_activity <- gls(
#   HF_value ~  factor(hf_type), 
#   data   = df_clean,
#   method = "ML"
# )
# #01d: day
# mdl0_no_random_day <- gls(
#   HF_value ~  factor(day), 
#   data   = df_clean,
#   method = "ML"
# )
# #01e: sex
# mdl0_no_random_sex <- gls(
#   HF_value ~  factor(sex), 
#   data   = df_clean,
#   method = "ML"
# )



#model 1a: random intercept - so only person differentiates
mdl1a_ri_person <- lme(
  fixed  = HF_value ~ factor(day) + factor(session) + factor(activity) + factor(HF_type) + factor(sex), 
  random = ~ 1 | person, 
  data   = df_clean,
  method = "ML" 
)
print("mdl 1a done")
print(summary(mdl1a_ri_person))



#model 1b: is there variability in how the same person performs diff activities?
mdl1b_ri_person_activity <- lme(
  fixed  = HF_value ~ factor(day) + factor(session) + factor(activity) + factor(HF_type) + factor(sex),   #how does it work to have session here
  random = ~  1 | person/activity,
  data   = df_clean,
  method = "ML"#, 
#   control = lmeControl(opt = "optim") #convergence error
)
print("mdl 1b done")
print(summary(mdl1b_ri_person_activity))



#model 1c: basically: are measurements from the same sensor on the same person more related than measurements from diff sensors?
mdl1c_ri_person_hftype <- lme(
  fixed  = HF_value ~ factor(day) + factor(session) + factor(activity) + factor(HF_type) + factor(sex),   #how does it work to have session here
  random = ~  1 | person/HF_type,
  data   = df_clean,
  method = "ML"#, 
#   control = lmeControl(opt = "optim") #convergence error
)
print("mdl 1c done")
print(summary(mdl1c_ri_person_hftype))


anova_mdl0_no_random_mdl1a_ri_person = anova(mdl0_no_random,mdl1a_ri_person)
anova_mdl1a_ri_person_mdl1b_ri_person_activity = anova(mdl1a_ri_person, mdl1b_ri_person_activity)
anova_mdl1a_ri_person_mdl1c_ri_person_hftype = anova(mdl1a_ri_person, mdl1c_ri_person_hftype)

print(anova_mdl0_no_random_mdl1a_ri_person)
print("nikita look here!")
#create list of anovas
anova_list_mdl1 <- rbind(
  cbind(Model = "Person", anova_mdl1a_ri_person_mdl1b_ri_person_activity[1, ]),
  cbind(Model = "Nested Activity", anova_mdl1a_ri_person_mdl1b_ri_person_activity[2, ]),
  cbind(Model = "Nested Sensor", anova_mdl1a_ri_person_mdl1c_ri_person_hftype[2, ])
) 
#%>%
#  select(-call) # This removes the messy R code column

anova_list_mdl1 %>%
  kbl(caption = "Model Comparison: Likelihood Ratio Tests against Baseline") %>%
  kable_classic(full_width = F) %>%
  save_kable(file = here("analysis_and_plots/anova_no_call.png"))



mdl1c_ri_person_hftype <- lme(
  fixed  = HF_value ~ factor(day) + factor(session) + factor(activity) + factor(HF_type) + factor(sex),   #how does it work to have session here
  random = ~  1 | person/HF_type,
  data   = df_clean,
  method = "ML"#, 
#   control = lmeControl(opt = "optim") #convergence error
)



#Model 2 section Add random slopes

#does each person have a different rate of cooling?
mdl2a_ri_person_hftype_rs_session_person <- lme(
  fixed  = HF_value ~ factor(day) + factor(session) + factor(activity) + factor(HF_type) + factor(sex),   #how does it work to have session here
  random = list(
    person  = ~ factor(session),  #at the person level, give random slope for session
    HF_type = ~ 1 #at the hf type level (nested within person), give random intercept
  ),
  data   = df_clean,
  method = "ML", 
  control = lmeControl(opt = "optim") #convergence error, so change to this
)

anova_mdl1c_ri_person_hftype_mdl2a_ri_person_hftype_rs_session_person = anova(mdl1c_ri_person_hftype,mdl2a_ri_person_hftype_rs_session_person)


#does each person react to temperature (day) differently?
mdl2b_ri_person_hftype_rs_day_person <- lme(
  fixed  = HF_value ~ factor(day) + factor(session) + factor(activity) + factor(HF_type) + factor(sex),   #how does it work to have session here
  random = list(
    person  = ~ factor(day),  #at the person level, give random slope for day
    HF_type = ~ 1 #at the hf type level (nested within person), give random intercept
  ),
  data   = df_clean,
  method = "ML", 
  control = lmeControl(opt = "optim") #convergence error, so change to this
)
anova_mdl1c_ri_person_hftype_mdl2b_ri_person_hftype_rs_day_person = anova(mdl1c_ri_person_hftype,mdl2b_ri_person_hftype_rs_day_person)


#does each person lose heat based on activity differently?
mdl2c_ri_person_hftype_rs_activity_person <- lme(
  fixed  = HF_value ~ factor(day) + factor(session) + factor(activity) + factor(HF_type) + factor(sex),   #how does it work to have session here
  random = list(
    person  = ~ factor(activity),  #at the person level, give random slope for day
    HF_type = ~ 1 #at the hf type level (nested within person), give random intercept
  ),
  data   = df_clean,
  method = "ML", 
  control = lmeControl(opt = "optim") #convergence error, so change to this
)
anova_mdl1c_ri_person_hftype_mdl2c_ri_person_hftype_rs_activity_person = anova(mdl1c_ri_person_hftype,mdl2c_ri_person_hftype_rs_activity_person)


anova_list_mdl2 <- rbind(
  cbind(Model = "Person + Sensor", anova_mdl1c_ri_person_hftype_mdl2a_ri_person_hftype_rs_session_person[1, ]),
  cbind(Model = "RS: Session | Person", anova_mdl1c_ri_person_hftype_mdl2a_ri_person_hftype_rs_session_person[2, ]),
  cbind(Model = "RS: Day | Person", anova_mdl1c_ri_person_hftype_mdl2b_ri_person_hftype_rs_day_person[2, ]),
  cbind(Model = "RS: Activity | Person", anova_mdl1c_ri_person_hftype_mdl2c_ri_person_hftype_rs_activity_person[2, ])
) 
#%>%
#  select(-call) # This removes the messy R code column

anova_list_mdl2 %>%
  kbl(caption = "Model Comparison: Likelihood Ratio Tests against Baseline") %>%
  kable_classic(full_width = F) %>%
  save_kable(file = here("analysis_and_plots/anova_no_call_model2.png"))




#Model 3 section: add intercepts

#does day interact with sex --> do men and women react differently to cold?

#does session interact with sex --> do men and women get hot at different rates?
# mdl3a_ri_person_hftype_rs_day_person_interact_sex_day <- lme(
#   fixed  = HF_value ~ factor(day) + factor(session) + factor(activity) + factor(HF_type) + factor(sex) + session*sex,   #how does it work to have session here
#   random = list(
#     person  = ~ factor(day),  #at the person level, give random slope for day
#     HF_type = ~ 1 #at the hf type level (nested within person), give random intercept
#   ),
#   data   = df_clean,
#   method = "ML", 
#   control = lmeControl(opt = "optim") #convergence error, so change to this
# )

mdl3a_ri_person_hftype_rs_day_person_interact_sex_day <- gls(
  HF_value ~ factor(day) + factor(session) + factor(activity) + factor(HF_type) + factor(sex) +factor(session)*factor(day) + factor(sex)*factor(session),   #how does it work to have session here
  data   = df_clean,
  method = "ML"#, 
#   control = lmeControl(opt = "optim") #convergence error
)
print("hi")
print(summary(mdl3a_ri_person_hftype_rs_day_person_interact_sex_day))
anovamdl1c_ri_person_hftype_mdl3a_ri_person_hftype_rs_day_person_interact_sex_day = anova(mdl1c_ri_person_hftype,mdl3a_ri_person_hftype_rs_day_person_interact_sex_day)
print("bye")
print(anovamdl1c_ri_person_hftype_mdl3a_ri_person_hftype_rs_day_person_interact_sex_day)
#does session interact with day --> do people get hot at different heats?




# create list of models
model_list <- list(
  "Person" = mdl1a_ri_person,
  "Person + Activity" = mdl1b_ri_person_activity,
  "Person + Sensor" = mdl1c_ri_person_hftype
)

modelsummary(
  model_list, 
  output = "kableExtra",
  estimate = "{estimate} ({std.error}){stars}", #ensure p-value stars still attached 
  statistic = NULL,                      
  stars = TRUE,
  notes = list("+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"), #put explanation in
  coef_rename = c( #renames variables without removing variables not in list
    "(Intercept)" = "Baseline (Intercept)",
    "factor(day)2" = "Day 2 (18°C)",
    "factor(session)2" = "Session 2",
    "factor(session)3" = "Session 3",
    "factor(session)4" = "Session 4",
    "factor(activity)standing" = "Activity: Standing",
    "factor(activity)walking" = "Activity: Walking",
    "factor(sex)M" = "Sex: Male"
  )
) %>% 
  kable_classic(full_width = F) %>%
  save_kable(file = here("analysis_and_plots/model_summary_1.png"))