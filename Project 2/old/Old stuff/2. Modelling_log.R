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

#test impact of each fixed effect
#01a: session
# mdl0_no_random_session <- gls(
#   log(HF_value) ~  factor(session), 
#   data   = df_clean,
#   method = "ML"
# )
# #01b: activity
# mdl0_no_random_activity <- gls(
#   log(HF_value) ~  factor(activity), 
#   data   = df_clean,
#   method = "ML"
# )
# #01c: sensor
# mdl0_no_random_sensor <- gls(
#   log(HF_value) ~  factor(HF_type), 
#   data   = df_clean,
#   method = "ML"
# )
# #01d: day
# mdl0_no_random_day <- gls(
#   log(HF_value) ~  factor(day), 
#   data   = df_clean,
#   method = "ML"
# )
# #01e: sex
# mdl0_no_random_sex <- gls(
#   log(HF_value) ~  factor(sex), 
#   data   = df_clean,
#   method = "ML"
# )

# anova_full_session = anova(mdl0_no_random,mdl0_no_random_session)
# anova_full_activity = anova(mdl0_no_random,mdl0_no_random_activity)
# anova_full_sensor = anova(mdl0_no_random,mdl0_no_random_sensor)
# anova_full_day = anova(mdl0_no_random,mdl0_no_random_day)
# anova_full_sex = anova(mdl0_no_random,mdl0_no_random_sex)

mdl0_no_random <- gls(
  log(HF_value) ~ factor(day) + factor(session) + factor(activity) + factor(HF_type) + factor(sex), 
  data   = df_clean,
  method = "ML"
)
print("full model no interactive")
print(round(summary(mdl0_no_random)$tTable,4))
#sex, day - mdl0_inter_sex_day
#activity, day - mdl0_inter_act_day
#sensor, day - mdl0_inter_sensor_day - YES
#session, day  - mdl0_inter_session_day - YES??
#sex, activity - mdl0_inter_sex_activity 
#sex, sensor - mdl0_inter_sex_sensor - MAYBE??
#sex, session - mdl0_inter_sex_session
#activity, sensor - mdl0_inter_activity_sensor - MAYBE??
#activity, session - mdl0_inter_activity_session - fails
#sensor, session - mdl0_inter_sensor_session = YES
mdl0_inter_sex_day <- gls(
  log(HF_value) ~ factor(day) + factor(session) + factor(activity) + factor(HF_type) + factor(sex) + factor(sex)*factor(day), 
  data   = df_clean,
  method = "ML"
)
print("1 full model interact sex day")
print(round(summary(mdl0_inter_sex_day)$tTable,4))

mdl0_inter_act_day <- gls(
  log(HF_value) ~ factor(day) + factor(session) + factor(activity) + factor(HF_type) + factor(sex) + factor(activity)*factor(day), 
  data   = df_clean,
  method = "ML"
)
print("2 full model interact activity day")
print(round(summary(mdl0_inter_act_day)$tTable,4))

mdl0_inter_sensor_day <- gls(
  log(HF_value) ~ factor(day) + factor(session) + factor(activity) + factor(HF_type) + factor(sex) + factor(HF_type)*factor(day), 
  data   = df_clean,
  method = "ML"
)
print("3 full model interact sensor day")
print(round(summary(mdl0_inter_sensor_day)$tTable,4))

mdl0_inter_session_day <- gls(
  log(HF_value) ~ factor(day) + factor(session) + factor(activity) + factor(HF_type) + factor(sex) + factor(session)*factor(day), 
  data   = df_clean,
  method = "ML"
)
print("4 full model interact session day")
print(round(summary(mdl0_inter_session_day)$tTable,4))

mdl0_inter_sex_activity <- gls(
  log(HF_value) ~ factor(day) + factor(session) + factor(activity) + factor(HF_type) + factor(sex) + factor(sex)*factor(activity), 
  data   = df_clean,
  method = "ML"
)
print("5 full model interact sex activity")
print(round(summary(mdl0_inter_sex_activity)$tTable,4))

mdl0_inter_sex_sensor <- gls(
  log(HF_value) ~ factor(day) + factor(session) + factor(activity) + factor(HF_type) + factor(sex) + factor(sex)*factor(HF_type), 
  data   = df_clean,
  method = "ML"
)
print("6 full model interact sex sensor")
print(round(summary(mdl0_inter_sex_sensor)$tTable,4))

mdl0_inter_sex_session <- gls(
  log(HF_value) ~ factor(day) + factor(session) + factor(activity) + factor(HF_type) + factor(sex) + factor(sex)*factor(session), 
  data   = df_clean,
  method = "ML"
)
print("7 full model interact sex session")
print(round(summary(mdl0_inter_sex_session)$tTable,4))

mdl0_inter_activity_sensor <- gls(
  log(HF_value) ~ factor(day) + factor(session) + factor(activity) + factor(HF_type) + factor(sex) + factor(HF_type)*factor(activity), 
  data   = df_clean,
  method = "ML"
)
print("8 full model interact activity sensor")
print(round(summary(mdl0_inter_activity_sensor)$tTable,4))

print("hi")
# mdl0_inter_activity_session <- gls(
#   log(HF_value) ~ factor(day) + factor(session) + factor(activity) + factor(HF_type) + factor(sex) + factor(session)*factor(activity), 
#   data   = df_clean,
#   method = "ML"
# )
# print("9 full model interact activity session")
# print(round(summary(mdl0_inter_activity_session)$tTable,4))

mdl0_inter_sensor_session <- gls(
  log(HF_value) ~ factor(day) + factor(session) + factor(activity) + factor(HF_type) + factor(sex) + factor(session)*factor(HF_type), 
  data   = df_clean,
  method = "ML"
)
print("10 full model interact sensor session")
print(round(summary(mdl0_inter_sensor_session)$tTable,4))

# print("anova 3")
# print(anova(mdl0_no_random, mdl0_interactive_sess))
# print("anova 4")
# print(anova(mdl0_no_random, mdl0_interactive_sensor))

mdl0_inter_sensor_activity_and_session_day <- gls(
  log(HF_value) ~ factor(day) + factor(session) + factor(activity) + factor(HF_type) + factor(sex) + factor(HF_type)*factor(activity) + factor(session)*factor(day), 
  data   = df_clean,
  method = "ML"
)
print("11 full model interact activity+sensor, day+session")
print(round(summary(mdl0_inter_sensor_session)$tTable,4))
#day+session, sex+sensor, activity+sensor

# print("anova 5")
# print(anova(mdl0_interactive_sensor, mdl0_interactive_sensor_sess))
# print("anova 5")
# print(anova(mdl0_interactive_sess, mdl0_interactive_sensor_sess))
# anova_list_mdl0 <- rbind(
#   cbind(Model = "Full", anova_full_session[1, ]),
#   cbind(Model = "Session", anova_full_session[2, ]),
#   cbind(Model = "Activity", anova_full_activity[2, ]),
#   cbind(Model = "Sensor", anova_full_sensor[2, ]),
#   cbind(Model = "Day", anova_full_day[2, ]),
#   cbind(Model = "Sex", anova_full_sex[2, ])
# ) 
# #%>%
# #  select(-call) # This removes the messy R code column

# anova_list_mdl0 %>%
#   kbl(caption = "Model Comparison: Likelihood Ratio Tests against Baseline") %>%
#   kable_classic(full_width = F) %>%
#   save_kable(file = here("analysis_and_plots/anova_mdls0_log.png"))



# #model 1a: random intercept - so only person differentiates
# mdl1a_ri_person <- lme(
#   fixed  = log(HF_value) ~ factor(day) + factor(session) + factor(activity) + factor(HF_type) + factor(sex), 
#   random = ~ 1 | person, 
#   data   = df_clean,
#   method = "ML" 
# )
# print("mdl 1a done")
# print(summary(mdl1a_ri_person))



# #model 1b: is there variability in how the same person performs diff activities?
# mdl1b_ri_person_activity <- lme(
#   fixed  = log(HF_value) ~ factor(day) + factor(session) + factor(activity) + factor(HF_type) + factor(sex),   #how does it work to have session here
#   random = ~  1 | person/activity,
#   data   = df_clean,
#   method = "ML"#, 
# #   control = lmeControl(opt = "optim") #convergence error
# )
# print("mdl 1b done")
# print(summary(mdl1b_ri_person_activity))



# #model 1c: basically: are measurements from the same sensor on the same person more related than measurements from diff sensors?
# mdl1c_ri_person_hftype <- lme(
#   fixed  = log(HF_value) ~ factor(day) + factor(session) + factor(activity) + factor(HF_type) + factor(sex),   #how does it work to have session here
#   random = ~  1 | person/HF_type,
#   data   = df_clean,
#   method = "ML"#, 
# #   control = lmeControl(opt = "optim") #convergence error
# )
# print("mdl 1c done")
# print(summary(mdl1c_ri_person_hftype))


# anova_mdl0_no_random_mdl1a_ri_person = anova(mdl0_no_random,mdl1a_ri_person)
# anova_mdl1a_ri_person_mdl1b_ri_person_activity = anova(mdl1a_ri_person, mdl1b_ri_person_activity)
# anova_mdl1a_ri_person_mdl1c_ri_person_hftype = anova(mdl1a_ri_person, mdl1c_ri_person_hftype)

# print(anova_mdl0_no_random_mdl1a_ri_person)
# print("nikita look here!")
# #create list of anovas
# anova_list_mdl1 <- rbind(
#   cbind(Model = "Person", anova_mdl1a_ri_person_mdl1b_ri_person_activity[1, ]),
#   cbind(Model = "Nested Activity", anova_mdl1a_ri_person_mdl1b_ri_person_activity[2, ]),
#   cbind(Model = "Nested Sensor", anova_mdl1a_ri_person_mdl1c_ri_person_hftype[2, ])
# ) 
# #%>%
# #  select(-call) # This removes the messy R code column

# anova_list_mdl1 %>%
#   kbl(caption = "Model Comparison: Likelihood Ratio Tests against Baseline") %>%
#   kable_classic(full_width = F) %>%
#   save_kable(file = here("analysis_and_plots/anova_mdls1_log.png"))



# mdl1c_ri_person_hftype <- lme(
#   fixed  = log(HF_value) ~ factor(day) + factor(session) + factor(activity) + factor(HF_type) + factor(sex),   #how does it work to have session here
#   random = ~  1 | person/HF_type,
#   data   = df_clean,
#   method = "ML"#, 
# #   control = lmeControl(opt = "optim") #convergence error
# )



# #Model 2 section Add random slopes
# print("start model 2")
# #does each person have a different rate of cooling?
# # mdl2a_ri_person_hftype_rs_session_person <- lme(
# #   fixed  = log(HF_value) ~ factor(day) + factor(session) + factor(activity) + factor(HF_type) + factor(sex),   #how does it work to have session here
# #   random = list(
# #     person  = ~ factor(session),  #at the person level, give random slope for session
# #     HF_type = ~ 1 #at the hf type level (nested within person), give random intercept
# #   ),
# #   data   = df_clean,
# #   method = "ML", 
# #   control = lmeControl(opt = "optim") #convergence error, so change to this
# # )

# # anova_mdl1c_ri_person_hftype_mdl2a_ri_person_hftype_rs_session_person = anova(mdl1c_ri_person_hftype,mdl2a_ri_person_hftype_rs_session_person)
# print("is this the error 1")

# #does each person react to temperature (day) differently?
# mdl2b_ri_person_hftype_rs_day_person <- lme(
#   fixed  = log(HF_value) ~ factor(day) + factor(session) + factor(activity) + factor(HF_type) + factor(sex),   #how does it work to have session here
#   random = list(
#     person  = ~ factor(day),  #at the person level, give random slope for day
#     HF_type = ~ 1 #at the hf type level (nested within person), give random intercept
#   ),
#   data   = df_clean,
#   method = "ML", 
#   control = lmeControl(opt = "optim") #convergence error, so change to this
# )
# anova_mdl1c_ri_person_hftype_mdl2b_ri_person_hftype_rs_day_person = anova(mdl1c_ri_person_hftype,mdl2b_ri_person_hftype_rs_day_person)

# print("is this the error 2")

# #does each person lose heat based on activity differently?
# mdl2c_ri_person_hftype_rs_activity_person <- lme(
#   fixed  = log(HF_value) ~ factor(day) + factor(session) + factor(activity) + factor(HF_type) + factor(sex),   #how does it work to have session here
#   random = list(
#     person  = ~ factor(activity),  #at the person level, give random slope for day
#     HF_type = ~ 1 #at the hf type level (nested within person), give random intercept
#   ),
#   data   = df_clean,
#   method = "ML", 
#   control = lmeControl(opt = "optim") #convergence error, so change to this
# )
# anova_mdl1c_ri_person_hftype_mdl2c_ri_person_hftype_rs_activity_person = anova(mdl1c_ri_person_hftype,mdl2c_ri_person_hftype_rs_activity_person)
# print("done model 2")

# anova_list_mdl2 <- rbind(
#   cbind(Model = "Person + Sensor", anova_mdl1c_ri_person_hftype_mdl2a_ri_person_hftype_rs_session_person[1, ]),
#   cbind(Model = "RS: Session | Person", anova_mdl1c_ri_person_hftype_mdl2a_ri_person_hftype_rs_session_person[2, ]),
#   cbind(Model = "RS: Day | Person", anova_mdl1c_ri_person_hftype_mdl2b_ri_person_hftype_rs_day_person[2, ]),
#   cbind(Model = "RS: Activity | Person", anova_mdl1c_ri_person_hftype_mdl2c_ri_person_hftype_rs_activity_person[2, ])
# ) 
# #%>%
# #  select(-call) # This removes the messy R code column

# anova_list_mdl2 %>%
#   kbl(caption = "Model Comparison: Likelihood Ratio Tests against Baseline") %>%
#   kable_classic(full_width = F) %>%
#   save_kable(file = here("analysis_and_plots/anova_model2_log.png"))




# #Model 3 section: add interactive??

# #does day interact with sex --> do men and women react differently to cold?
# print("done random slope")
# #does session interact with sex --> do men and women get hot at different rates?
# mdl3a_ri_person_hftype_rs_day_person_interact_sex_day <- lme(
#   fixed  = log(HF_value) ~ factor(day) + factor(session) + factor(activity) + factor(HF_type) + factor(sex) + session*sex,   #how does it work to have session here
#   random = list(
#     person  = ~ factor(day),  #at the person level, give random slope for day
#     HF_type = ~ 1 #at the hf type level (nested within person), give random intercept
#   ),
#   data   = df_clean,
#   method = "ML", 
#   control = lmeControl(opt = "optim") #convergence error, so change to this
# )

# # mdl3a_ri_person_hftype_rs_day_person_interact_sex_day <- gls(
# #   log(HF_value) ~ factor(day) + factor(session) + factor(activity) + factor(HF_type) + factor(sex) +factor(session)*factor(day) + factor(sex)*factor(session),   #how does it work to have session here
# #   data   = df_clean,
# #   method = "ML"#, 
# # #   control = lmeControl(opt = "optim") #convergence error
# # )
# # print("hi")
# # print(summary(mdl3a_ri_person_hftype_rs_day_person_interact_sex_day))
# # anovamdl1c_ri_person_hftype_mdl3a_ri_person_hftype_rs_day_person_interact_sex_day = anova(mdl1c_ri_person_hftype,mdl3a_ri_person_hftype_rs_day_person_interact_sex_day)
# # print("bye")
# # print(anovamdl1c_ri_person_hftype_mdl3a_ri_person_hftype_rs_day_person_interact_sex_day)
# # #does session interact with day --> do people get hot at different heats?


