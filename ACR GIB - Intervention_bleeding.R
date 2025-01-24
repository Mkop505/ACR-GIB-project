#loading csv
data <- read.csv("C:\\Users\\Michaela\\OneDrive\\Desktop\\GIB at intervention.csv", header = TRUE)

#convert CTA result & intervention result to factors
data$CTA_result <- as.factor(data$CTA_result)
data$intervention_bleeding <- as.factor(data$intervention_bleeding)
data$Anticoagulation <- as.factor(data$Anticoagulation)

#Take out NAs
nrow(data[is.na(data$intervention_bleeding),])
data <- data[!(is.na(data$intervention_bleeding)),]
nrow(data)

#checking boolean and categorical variables
xtabs (~intervention_bleeding + Anticoagulation, data=data)

#take out other columns that I dont need
data$ptid <- NULL
data$GI_Procedure <- NULL
data$Intervention_type <- NULL
data$pulse_pressure <- NULL
data$CTA_result <- NULL


#report median and Q1Q3
# Variables for descriptive statistics
descriptive_stats <- c("days_to_intervention")

# Initialize a results data frame
results <- data.frame(
  Variable = character(),
  Median = numeric(),
  Q1 = numeric(),
  Q3 = numeric(),
  stringsAsFactors = FALSE
)

# Loop through variables to calculate stats
for (var in descriptive_stats) {
  if (is.numeric(data[[var]])) {  # Ensure the variable is numeric
    median_value <- median(data[[var]], na.rm = TRUE)
    q1 <- quantile(data[[var]], 0.25, na.rm = TRUE)
    q3 <- quantile(data[[var]], 0.75, na.rm = TRUE)
    
    # Add results to the data frame
    results <- rbind(results, data.frame(
      Variable = var,
      Median = median_value,
      Q1 = q1,
      Q3 = q3
    ))
  }
}

# Print results
print(results)


#univariate analysis
#check for normality
shapiro.test(data$days_to_intervention)

#wilcoxon rank test but with less headache
variables <- c("exam_age", "Hgb", "Hct", "HR", "systolic_BP", "diastolic_BP", 
               "MAP", "PT", "INR", "days_to_intervention")
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

#chisquared
table_data <- table(data$Anticoagulation, data$intervention_bleeding)
chisq.test(table_data)





#Multivariable model - logistic regression
#unadjusted logistic regression
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

#adjusted model
logistic <- glm(intervention_bleeding~., data=data, family="binomial")
summary(logistic)

#calculate McFadden Pseudo R2 
ll.null <- logistic$null.deviance/-2
ll.proposed <- logistic$deviance/-2
(ll.null-ll.proposed)/ll.null
1-pchisq(2*(ll.proposed-ll.null), df=(length(logistic$coefficients)-1))


table(data$predictor, data$outcome)
