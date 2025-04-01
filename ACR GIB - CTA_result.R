#loading csv
data <- read.csv("C:\\Users\\Michaela\\OneDrive\\Desktop\\R Studio - CTA\\CTA ABD MK v7.csv", header = TRUE)

#convert CTA result & intervention result to factors
data$CTA_result <- as.factor(data$CTA_result)
data$intervention_bleeding <- as.factor(data$intervention_bleeding)
data$intervention_type <- as.factor(data$intervention_type)
data$Anticoagulation <- as.factor(data$Anticoagulation)
data$Gender <- as.factor(data$Gender)
data$gib_location <- as.factor(data$gib_location)

#checking boolean and categorical variables
xtabs (~CTA_result + Anticoagulation, data=data)
xtabs (~CTA_result + Gender, data=data)
xtabs (~CTA_result + gib_location, data=data)
xtabs (~CTA_result + intervention_bleeding, data=data)
xtabs (~CTA_result + intervention_type, data=data)

#take out other columns that I dont need
data$ptid <- NULL
data$Note <- NULL

# Variables for descriptive statistics for CTA result
descriptive_stats <- c("AGE", "Hgb", "Hct", "HR", "systolic_BP", "diastolic_BP", "MAP", "INR","intervention_time")

# Initialize a results data frame
results <- data.frame(
  Variable = character(),
  mean = numeric(),
  SD = numeric(),
  stringsAsFactors = FALSE
)

#check the continuous variables mean/sd stratified by CTA_result
library(dplyr)

# Calculate mean age for CTA+ and CTA- cases
data %>%
  group_by(CTA_result) %>%
  summarise(across(all_of(descriptive_stats), 
                   list(Mean = ~mean(.x, na.rm = TRUE), 
                        SD = ~sd(.x, na.rm = TRUE))))

#Univariate Analysis for CTA result
#Wilcoxon rank
variables <- c("AGE", "HR", "systolic_BP", "diastolic_BP", "MAP", "Hgb", "Hct", "INR")
results <- data.frame(
  Variable = character(),
  Statistic = numeric(),
  P_Value = numeric(),
  Normality = character(),
  stringsAsFactors = FALSE
)
print(results)

#wilcoxon rank test
results <- data.frame(
  Variable = character(),
  Statistic = numeric(),
  P_Value = numeric(),
  Alternative_Hypothesis = character(),
  stringsAsFactors = FALSE
)
for(var in variables) {
  test_result <- wilcox.test(data[[var]]~data$CTA_result)
  results <- rbind(results, data.frame(
    Variable = var,
    Statistic = test_result$statistic,
    P_Value = test_result$p.value,
    Alternative_Hypothesis = test_result$alternative
  ))
  
}
print(results)


#chi square test for 2 binomial variables
variables_binomial <- c("Gender", "Anticoagulation", "gib_location", "intervention_bleeding")
results <- data.frame(
  Variable = character(),
  Statistic = numeric(),
  P_value = numeric(),
  stringsAsFactors = FALSE
)
  
for(var in variables_binomial) {
  tbl <- table(data[[var]], data$CTA_result)
  test_result <- chisq.test(tbl)
  
  results <- rbind(results, data.frame(
    Variable = var,
    Statistic = test_result$statistic,
    P_value = test_result$p.value
  ))
}

print(results)

#Multivariable model - logistic regression
#unadjusted logistic regression
variables <- c("Gender", "AGE", "Hgb", "Hct", "HR", "MAP", "INR", "Anticoagulation", "gib_location", "intervention_bleeding")
results <- data.frame(
  Variable = character(),
  Odds_Ratio = numeric(),
  CI_Lower = numeric(),
  CI_Upper = numeric(),
  P_Value = numeric(),
  stringsAsFactors = FALSE
)
# Loop through each variable and run logistic regression
for (var in variables) {
  # Run unadjusted logistic regression
  formula <- as.formula(paste("CTA_result ~", var))
  model <- glm(formula, family = binomial, data = data)
  
  # Extract coefficients, odds ratios, and confidence intervals
  coef <- summary(model)$coefficients
  odds_ratio <- exp(coef[2, 1])  # Odds ratio for the variable
  ci <- exp(confint(model)[2, ]) # 95% confidence interval
  p_value <- coef[2, 4]          # P-value for the variable
  
  # Add results to the results data frame
  results <- rbind(results, data.frame(
    Variable = var,
    Odds_Ratio = round(odds_ratio, 3),
    CI_Lower = round(ci[1], 3),
    CI_Upper = round(ci[2], 3),
    P_Value = round(p_value, 4)
  ))
}

# View the results table
print(results)

# Adjusted logistic regression for CTA_result
adjusted_model_cta <- glm(CTA_result ~ Gender + AGE + Hgb + Hct + HR + MAP + INR + Anticoagulation + gib_location + intervention_bleeding,
                          family = binomial, data = data)

# View model summary
summary(adjusted_model_cta)

# Extract adjusted odds ratios and 95% confidence intervals
adjusted_results_cta <- data.frame(
  Variable = rownames(coef(summary(adjusted_model_cta)))[-1],  # remove intercept
  OR = round(exp(coef(adjusted_model_cta)[-1]), 3),
  CI_Lower = round(exp(confint(adjusted_model_cta)[-1, 1]), 3),
  CI_Upper = round(exp(confint(adjusted_model_cta)[-1, 2]), 3),
  P_Value = round(coef(summary(adjusted_model_cta))[-1, 4], 4)
)

# View results
print(adjusted_results_cta)








#I need to do the same thing but for bleeding at intervention
#BLEEDING AT INTERVENTIONNNN
# Variables for descriptive statistics for Bleeding at intervention
descriptive_stats <- c("AGE", "Hgb", "Hct", "HR", "MAP", "INR","intervention_time")

# Initialize a results data frame
results <- data.frame(
  Variable = character(),
  mean = numeric(),
  SD = numeric(),
  stringsAsFactors = FALSE
)

#check the continuous variables mean/sd stratified by intervention_bleeding
#library(dplyr)

# Calculate mean age for CTA+ and CTA- cases
#data %>%
  #group_by(CTA_result) %>%
  #summarise(across(all_of(descriptive_stats), 
                   #list(Mean = ~mean(.x, na.rm = TRUE), 
                        #SD = ~sd(.x, na.rm = TRUE))))

#Univariate Analysis for CTA result
#t-test is for norm dist, Wilcoxon is nonparametric
#check for normal distribution

variables <- c("AGE", "HR", "MAP", "Hgb", "Hct", "INR", "intervention_time")
results <- data.frame(
  Variable = character(),
  Statistic = numeric(),
  P_Value = numeric(),
  Normality = character(),
  stringsAsFactors = FALSE
)
print(results)

#wilcoxon rank test
results <- data.frame(
  Variable = character(),
  Statistic = numeric(),
  P_Value = numeric(),
  Alternative_Hypothesis = character(),
  stringsAsFactors = FALSE
)
for(var in variables) {
  test_result <- wilcox.test(data[[var]]~data$intervention_bleeding)
  results <- rbind(results, data.frame(
    Variable = var,
    Statistic = test_result$statistic,
    P_Value = test_result$p.value,
    Alternative_Hypothesis = test_result$alternative
  ))
  
}
print(results)


#chi square test for 2 binomial variables
variables_binomial <- c("Gender", "Anticoagulation", "gib_location")
results <- data.frame(
  Variable = character(),
  Statistic = numeric(),
  P_value = numeric(),
  stringsAsFactors = FALSE
)

for(var in variables_binomial) {
  tbl <- table(data[[var]], data$intervention_bleeding)
  test_result <- chisq.test(tbl)
  
  results <- rbind(results, data.frame(
    Variable = var,
    Statistic = test_result$statistic,
    P_value = test_result$p.value
  ))
}

print(results)

#Multivariable model - logistic regression
#unadjusted logistic regression
variables <- c("Gender", "AGE", "Hgb", "Hct", "HR", "MAP", "INR", "Anticoagulation", "gib_location", "intervention_time")
results <- data.frame(
  Variable = character(),
  Odds_Ratio = numeric(),
  CI_Lower = numeric(),
  CI_Upper = numeric(),
  P_Value = numeric(),
  stringsAsFactors = FALSE
)
# Loop through each variable and run logistic regression
for (var in variables) {
  # Run unadjusted logistic regression
  formula <- as.formula(paste("intervention_bleeding ~", var))
  model <- glm(formula, family = binomial, data = data)
  
  # Extract coefficients, odds ratios, and confidence intervals
  coef <- summary(model)$coefficients
  odds_ratio <- exp(coef[2, 1])  # Odds ratio for the variable
  ci <- exp(confint(model)[2, ]) # 95% confidence interval
  p_value <- coef[2, 4]          # P-value for the variable
  
  # Add results to the results data frame
  results <- rbind(results, data.frame(
    Variable = var,
    Odds_Ratio = round(odds_ratio, 3),
    CI_Lower = round(ci[1], 3),
    CI_Upper = round(ci[2], 3),
    P_Value = round(p_value, 4)
  ))
}

# View the results table
print(results)

# Adjusted logistic regression model
adjusted_model <- glm(intervention_bleeding ~ Gender + AGE + Hgb + Hct + HR + MAP + INR + Anticoagulation + gib_location + intervention_time,
                      family = binomial, data = data)

# View summary (coefficients & p-values)
summary(adjusted_model)

# Get adjusted odds ratios and 95% confidence intervals
adjusted_results <- data.frame(
  Variable = rownames(coef(summary(adjusted_model)))[-1],  # remove intercept
  OR = round(exp(coef(adjusted_model)[-1]), 3),
  CI_Lower = round(exp(confint(adjusted_model)[-1, 1]), 3),
  CI_Upper = round(exp(confint(adjusted_model)[-1, 2]), 3),
  P_Value = round(coef(summary(adjusted_model))[-1, 4], 4)
)

# View results
print(adjusted_results)

