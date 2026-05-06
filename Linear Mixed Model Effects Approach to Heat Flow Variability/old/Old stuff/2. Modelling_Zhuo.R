


library(tidyverse) #installed tidyverse
library(nlme)
library(ggplot2)
library(here)
library(kableExtra)
library(modelsummary)

# Load the dataset
file_path <- here("analysis_and_plots/heat_flow_ICE_EPFL.csv")
data <- read.csv(file_path)
head(data)
colSums(is.na(data))
nrow(data)

library(tidyverse)
data_clean <- data %>%
  drop_na() %>%
  mutate(
    person = as.factor(person),
    day = as.factor(day), 
    session = as.factor(session),
    activity = as.factor(activity),
    sex = as.factor(sex),
    HF_type = as.factor(HF_type),
    sensor_num = as.numeric(HF_type),
    session_num = as.numeric(session),
    Block_ID = as.factor(paste(person, day, session, activity, sep="_"))
  )

# baselines
levels(data_clean$sex)[1]
levels(data_clean$activity)[1]
levels(data_clean$day)[1]
levels(data_clean$session)[1]
levels(data_clean$HF_type)[1] 

naive_lm <- lm(HF_value ~ session + activity + day + HF_type + sex, data = data_clean)


# plot(naive_lm)

log_naive_lm <- lm(log(HF_value) ~ session + activity + day + HF_type + sex, data = data_clean)

# plot(log_naive_lm)

res_naive <- resid(naive_lm)
res_log   <- resid(log_naive_lm)

# Extract residuals

# 2. Calculate correlation with theoretical quantiles for the Naive model
qq_corr_naive <- cor(res_naive[order(res_naive)], 
                     qnorm(ppoints(length(res_naive))))

# 3. Calculate correlation with theoretical quantiles for the Log model
qq_corr_log   <- cor(res_log[order(res_log)], 
                     qnorm(ppoints(length(res_log))))

# 4. Print the results
print(paste("Correlation Naive Model:", round(qq_corr_naive, 4)))
print(paste("Correlation Log Model:", round(qq_corr_log, 4)))

# Calculate Correlation (r)
r_naive <- cor(res_naive[order(res_naive)], qnorm(ppoints(length(res_naive))))

# Calculate R-Squared (R^2)
r2_naive <- r_naive^2

print(paste("QQ-Plot R-squared (Naive):", round(r2_naive, 4)))

# Calculate Correlation (r)
r_log <- cor(res_log[order(res_log)], qnorm(ppoints(length(res_log))))

# Calculate R-Squared (R^2)
r2_log <- r_log^2

print(paste("QQ-Plot R-squared (Naive):", round(r2_log, 4)))


r2_naive <- (cor(res_naive[order(res_naive)], qnorm(ppoints(length(res_naive)))))^2
r2_log   <- (cor(res_log[order(res_log)], qnorm(ppoints(length(res_log)))))^2

# 2. Constant Variance Check (Correlation between Fitted and Absolute Residuals)
# Lower Correlation (closer to 0) = Better Homoscedasticity
# A high correlation suggests a "fan shape" (variance increases with mean)
var_corr_naive <- cor(fitted(naive_lm), abs(res_naive))
var_corr_log   <- cor(fitted(log_naive_lm), abs(res_log))

# --- Print Final Diagnostics Table ---
cat("\n--- Numerical Diagnostics Comparison ---\n")
cat(sprintf("%-25s | %-15s | %-15s\n", "Metric", "Naive Model", "Log Model"))
cat(paste0(rep("-", 60), collapse = ""), "\n")
cat(sprintf("%-25s | %-15.4f | %-15.4f\n", "QQ-Plot R-squared", r2_naive, r2_log))
cat(sprintf("%-25s | %-15.4f | %-15.4f\n", "Fitted-Resid Corr", var_corr_naive, var_corr_log))

# --- Decision Logic for your Report ---
if (r2_log > r2_naive && abs(var_corr_log) < abs(var_corr_naive)) {
  print("Verdict: Log model is numerically superior for both Normality and Homoscedasticity.")
} else {
  print("Verdict: Mixed results. Check visual plots to see if the improvement is meaningful.")
}