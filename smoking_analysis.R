
# Effect of Smoking status on Lung Cancer and MI

# Dataset: smoking_data.csv (n = 500) 
# 3 columns are independent variable, 2 columns are dependent variable, all categorical except the age.


# Tasks covered for analysis: 
#                            1) Descriptive
#                            2) Exploratory
#                            3) Chi-square
#                            4) Logistic regression






# Section 1: Data import and correction_________________________________________


rm(list = ls()) # Remove previous variables for a clean start


# Load dataset with integers as a dataframe
dataframe <- read.csv("smoking_data.csv", stringsAsFactors = FALSE)  



str(dataframe) # prints summary


# Check the missing values, and print them 
cat("Rows:", nrow(dataframe), "| Columns:", ncol(dataframe), "\n")
cat("\nMissing values per column:\n")
print(colSums(is.na(dataframe)))



# Required fix: Two rows use text "No" instead of numeric code "2" in lung_cancer
# (1 = No, 2 = Yes)

if ("No" %in% dataframe$lung_cancer)  # runs the code inside, if the condition is True. It checks the "No" values
  
  {
  dataframe$lung_cancer[dataframe$lung_cancer == "No"] <- "1"
}
dataframe$lung_cancer <- as.integer(dataframe$lung_cancer)





# Section 2: Decoding of categorical variables__________________________________


# In contrast of Python, R wants categorical groups given as STRINGS, not the encoded numbers (1,2,3 etc)

# When a column contains integers, R treats those as "continuous" quantities by default.

# Therefore, categorical variables should not be encoded. We can decode them by using "factor()"



# smoking_ever: 1 = Never, 2 = Ever
dataframe$smoking_ever <- factor(
  dataframe$smoking_ever,
  levels = c(1, 2),
  labels = c("Never", "Ever")
)


# Categorical Factors are automatically handled correctly by statistical tests and regression models.




# smoking_duration: 1 = Never, 2 = <1 year, 3 = 2-5 years, 4 = >5 years
dataframe$smoking_duration <- factor(
  dataframe$smoking_duration,
  levels = c(1, 2, 3, 4),
  labels = c("Never", "LessThan1yr", "2to5yrs", "MoreThan5yrs"),
  ordered = TRUE
)


# lung_cancer: 1 = No, 2 = Yes
dataframe$lung_cancer <- factor(dataframe$lung_cancer, levels = c(1, 2), labels = c("No", "Yes"))


# mi: 1 = No, 2 = Yes
dataframe$mi <- factor(dataframe$mi, levels = c(1, 2), labels = c("No", "Yes"))


str(dataframe)





# Section 3: descriptive statistics_____________________________________________

cat("\nDescriptive statistics\n")


# 3.1) Age variable

cat("\nAge summary statistics:\n")
cat("Mean   :", round(mean(dataframe$age), 1))  # round to display less decimals
cat("Median :", median(dataframe$age))
cat("SD     :", round(sd(dataframe$age), 2))
cat("Range  :", range(dataframe$age), "\n")





# 3.2) Cross-tabs (Contingency tables)
# Frequency of a variable, according to output

cat("\nCross-tab: Smoking Ever & MI\n")
print(table(Smoking = dataframe$smoking_ever, MI = dataframe$mi))

cat("\nCross-tab: Smoking Ever & Lung Cancer\n")
print(table(Smoking = dataframe$smoking_ever, LungCancer = dataframe$lung_cancer))

cat("\nCross-tab: Smoking Duration & MI\n")
print(table(Duration = dataframe$smoking_duration, MI = dataframe$mi))

cat("\nCross-tab: Smoking Duration & Lung Cancer\n")
print(table(Duration = dataframe$smoking_duration, LungCancer = dataframe$lung_cancer))




# 3.3) Plotting the Histogram 

hist(                                                          
  dataframe$age,                              # the variable in dataframe
  main = "Age Distribution of Patients",      # title
  xlab = "Age (years)",                       # x-axis label
  col = "steelblue",
  border = "white",
  breaks = 30                                 # number of bars
)



# Save plot as an image in the directory

png("age_distribution.png", width = 700, height = 500)   # save the plot below, as PNG
hist(                                                          
  dataframe$age,                                               
  main = "Age Distribution of Patients",                       
  xlab = "Age (years)",
  col = "steelblue",
  border = "white",
  breaks = 30
)
dev.off()     # save and close



# 3.4) Bar graphs


# Counts of smoking status
png("smoking_ever.png", width = 700, height = 500)
barplot(
  table(dataframe$smoking_ever),
  main = "Patients by Smoking Status",
  xlab = "Smoking Status",
  ylab = "Count",
  col = c("steelblue", "tomato")
)
dev.off()


# Counts of smoking duration
png("smoking_duration.png", width = 700, height = 500)
barplot(
  table(dataframe$smoking_duration),
  main = "Patients by Smoking Duration",
  xlab = "Duration",
  ylab = "Count",
  col = c("steelblue", "lightcoral", "indianred", "darkred")
)
dev.off()


# Counts of lung cancer cases
png("lung_cancer.png", width = 700, height = 500)
barplot(
  table(dataframe$lung_cancer),
  main = "Lung Cancer Cases",
  xlab = "Lung Cancer",
  ylab = "Count",
  col = c("steelblue", "tomato")
)
dev.off()


# Counts of MI cases
png("task1_mi.png", width = 600, height = 500)
barplot(
  table(dataframe$mi),
  main = "Myocardial Infarction (MI) Cases",
  xlab = "MI",
  ylab = "Count",
  col = c("steelblue", "tomato")
)
dev.off()






# Section 4: Exploration of associations________________________________________


# 4.1) Cross-tabs with percentages 

# Generate table variables to plot these cross-tabs


mi_rate_by_smoking_status <- prop.table(        # convert to fractions between 0 and 1
  table(dataframe$smoking_ever, dataframe$mi),  # get the counts
  margin = 1                                    # divide by row totals
) * 100     # convert them to percentages


lung_cancer_rate_by_smoking_status <- prop.table(table(dataframe$smoking_ever, dataframe$lung_cancer), margin = 1) * 100

mi_rate_by_smoking_duration <- prop.table(table(dataframe$smoking_duration, dataframe$mi), margin = 1) * 100

lung_cancer_rate_by_smoking_duration <- prop.table(table(dataframe$smoking_duration, dataframe$lung_cancer), margin = 1) * 100



# 4.2) Plot cross-tabs as bar graphs


# smokings status - MI
png("mi_by_smoking_ever.png", width = 700, height = 500)
barplot(
  t(mi_rate_by_smoking_status),             # transpose the table
  beside = TRUE,
  main = "MI Rate (%) by Smoking Status",
  xlab = "Smoking Status",
  ylab = "Percentage (%)",
  col = c("steelblue", "tomato"),
  legend.text = c("MI: No", "MI: Yes"),
  args.legend = list(x = "topright")
)
dev.off()


# smokings status - Lung Cancer
png("lungcancer_by_smoking_ever.png", width = 700, height = 500)
barplot(
  t(lung_cancer_rate_by_smoking_status),
  beside = TRUE,
  main = "Lung Cancer Rate (%) by Smoking Status",
  xlab = "Smoking Status",
  ylab = "Percentage (%)",
  col = c("steelblue", "tomato"),
  legend.text = c("Lung Cancer: No", "Lung Cancer: Yes"),
  args.legend = list(x = "topright")
)
dev.off()


# smoking duration - MI 
png("mi_by_smoking_duration.png", width = 800, height = 500)
barplot(
  t(mi_rate_by_smoking_duration),
  beside = TRUE,
  main = "MI Rate (%) by Smoking Duration",
  xlab = "Smoking Duration",
  ylab = "Percentage (%)",
  col = c("steelblue", "tomato"),
  legend.text = c("MI: No", "MI: Yes"),
  args.legend = list(x = "topright")
)
dev.off()


# smoking duration - Lung Cancer 
png("lungcancer_by_smoking_duration.png", width = 800, height = 500)
barplot(
  t(lung_cancer_rate_by_smoking_duration),
  beside = TRUE,
  main = "Lung Cancer Rate (%) by Smoking Duration",
  xlab = "Smoking Duration",
  ylab = "Percentage (%)",
  col = c("steelblue", "tomato"),
  legend.text = c("Lung Cancer: No", "Lung Cancer: Yes"),
  args.legend = list(x = "topright")
)
dev.off()






# Section 5: Hypothesis testing with chi-square_________________________________
cat("\nHypothesis testing\n")


# Chi-Square is a one factor vs. one outcome test for categorical variables.

# Note: Chi-square test assumes all expected counts in the table are >=5.

# If any are <5, the p-value may be unreliable. Consider using "fisher.test()" instead.




# Test 1: Smoking Ever x MI
crosstab1 <- table(dataframe$smoking_ever, dataframe$mi)  # chi-square uses cross-tabs
chi_result <- suppressWarnings(chisq.test(crosstab1))     # outputs an object containing the results

cat("\nSmoking Ever x MI\n")
cat("H0: No association | H1: Association exists\n")

cat(sprintf("Chi^2 = %.3f, df = %d, p = %s\n",            # printing the values by a formatting template
            chi_result$statistic, chi_result$parameter, 
            format(chi_result$p.value, digits = 4, scientific = FALSE)))


# Degrees of freedom (df): Defines which value of Chi-square is significant; selects which test distribution to use
# df=(rows???1)??(columns???1)  from the cross-tab




# Test 2: Smoking Ever x Lung Cancer
crosstab2 <- table(dataframe$smoking_ever, dataframe$lung_cancer)
chi_result <- suppressWarnings(chisq.test(crosstab2))
cat("\nSmoking Ever x Lung Cancer\n")
cat("H0: No association | H1: Association exists\n")
cat(sprintf("Chi^2 = %.3f, df = %d, p = %s\n", 
            chi_result$statistic, chi_result$parameter, 
            format(chi_result$p.value, digits = 4, scientific = FALSE)))



# Test 3: Smoking Duration x MI
crosstab3 <- table(dataframe$smoking_duration, dataframe$mi)
chi_result <- suppressWarnings(chisq.test(crosstab3))
cat("\nSmoking Duration x MI\n")
cat("H0: No association | H1: Association exists\n")
cat(sprintf("Chi^2 = %.3f, df = %d, p = %s\n", 
            chi_result$statistic, chi_result$parameter, 
            format(chi_result$p.value, digits = 4, scientific = FALSE)))



# Test 4: Smoking Duration x Lung Cancer
crosstab4 <- table(dataframe$smoking_duration, dataframe$lung_cancer)
chi_result <- suppressWarnings(chisq.test(crosstab4))
cat("\nSmoking Duration x Lung Cancer\n")
cat("H0: No association | H1: Association exists\n")
cat(sprintf("Chi^2 = %.3f, df = %d, p = %s\n", 
            chi_result$statistic, chi_result$parameter, 
            format(chi_result$p.value, digits = 4, scientific = FALSE)))





# Section 6: Logistic Regression ________________________________________________

cat("\nTask 4: Logistic regression\n")

# Scale age to 10-year units so each OR reflects a per-decade change
dataframe$age_10 <- dataframe$age / 10

# Unordered copy of smoking_duration with "Never" as reference level
# ??? glm() will create one beta per level vs Never (interpretable ORs)
# Original smoking_duration stays ordered for trend tests
dataframe$smoking_duration_not_ordinal <- factor(dataframe$smoking_duration, ordered = FALSE)
dataframe$smoking_duration_not_ordinal  <- relevel(dataframe$smoking_duration_not_ordinal, ref = "Never")




# ?????? Single-factor models (unadjusted)



# Model 1: Smoking Ever -> MI 
# Binary predictor: Ever vs Never; single beta ??? single OR
model_mi_smoking_ever <- glm(mi ~ smoking_ever, data = dataframe, family = binomial)

cat("\nModel 1: Smoking Ever -> MI\n")
cat("OR for 'Ever' vs 'Never':",
    round(exp(coef(model_mi_smoking_ever)["smoking_everEver"]), 3),
    "| 95% CI:",
    round(exp(suppressMessages(confint(model_mi_smoking_ever))["smoking_everEver", ]), 3), "\n")



# Model 2: Smoking Ever -> Lung Cancer 
model_lc_smoking_ever <- glm(lung_cancer ~ smoking_ever, data = dataframe, family = binomial)

cat("\nModel 2: Smoking Ever -> Lung Cancer\n")
cat("OR for 'Ever' vs 'Never':",
    round(exp(coef(model_lc_smoking_ever)["smoking_everEver"]), 3),
    "| 95% CI:",
    round(exp(suppressMessages(confint(model_lc_smoking_ever))["smoking_everEver", ]), 3), "\n")



# Model 3: Smoking Duration -> MI 


# 3a: Unordered factor ??? one OR per level vs Never
model_mi_dur_cat <- glm(mi ~ smoking_duration_not_ordinal, data = dataframe, family = binomial)

cat("\nModel 3a: Smoking Duration -> MI (categorical ORs vs Never)\n")

# Build table, then strip the variable name prefix R adds to row names
or_table <- round(exp(cbind(OR = coef(model_mi_dur_cat),
                            suppressMessages(confint(model_mi_dur_cat)))), 3)
rownames(or_table) <- gsub("smoking_duration_not_ordinal", "Duration: ", rownames(or_table))
print(or_table)


# 3b: Ordered factor ??? .L coefficient = linear trend across ordered levels
model_mi_dur_ord <- glm(mi ~ smoking_duration, data = dataframe, family = binomial)

cat("\nModel 3b: Smoking Duration -> MI (linear trend / dose-response)\n")
trend_mi_dur <- coef(summary(model_mi_dur_ord))["smoking_duration.L", ]
cat("Linear trend OR:", round(exp(trend_mi_dur["Estimate"]), 3),
    "| p-value:", formatC(trend_mi_dur["Pr(>|z|)"], format = "f", digits = 4), "\n")



# Model 4: Smoking Duration -> Lung Cancer 

# 4a: Unordered factor ??? one OR per level vs Never
model_lc_dur_cat <- glm(lung_cancer ~ smoking_duration_not_ordinal, data = dataframe, family = binomial)

cat("\nModel 4a: Smoking Duration -> Lung Cancer (categorical ORs vs Never)\n")
or_table <- round(exp(cbind(OR = coef(model_lc_dur_cat),
                            suppressMessages(confint(model_lc_dur_cat)))), 3)
rownames(or_table) <- gsub("smoking_duration_not_ordinal", "Duration: ", rownames(or_table))
print(or_table)


# 4b: Ordered factor ??? linear trend test
model_lc_dur_ord <- glm(lung_cancer ~ smoking_duration, data = dataframe, family = binomial)

cat("\nModel 4b: Smoking Duration -> Lung Cancer (linear trend / dose-response)\n")
trend_lc_dur <- coef(summary(model_lc_dur_ord))["smoking_duration.L", ]
cat("Linear trend OR:", round(exp(trend_lc_dur["Estimate"]), 3),
    "| p-value:", formatC(trend_lc_dur["Pr(>|z|)"], format = "f", digits = 4), "\n")



# Model 5: Age -> MI 
# Continuous predictor; OR = multiplicative change in MI odds per 10 years
model_mi_age <- glm(mi ~ age_10, data = dataframe, family = binomial)

cat("\nModel 5: Age -> MI\n")
cat("OR per 10-year increase:",
    round(exp(coef(model_mi_age)["age_10"]), 3),
    "| 95% CI:",
    round(exp(suppressMessages(confint(model_mi_age))["age_10", ]), 3), "\n")



# Model 6: Age -> Lung Cancer 
model_lc_age <- glm(lung_cancer ~ age_10, data = dataframe, family = binomial)

cat("\nModel 6: Age -> Lung Cancer\n")
cat("OR per 10-year increase:",
    round(exp(coef(model_lc_age)["age_10"]), 3),
    "| 95% CI:",
    round(exp(suppressMessages(confint(model_lc_age))["age_10", ]), 3), "\n")





# ?????? Age-adjusted models ????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????

# Adding age_10 to isolate each predictor's independent effect



# Model 7: Smoking Ever + Age -> MI 
model_mi_ever_adj <- glm(mi ~ smoking_ever + age_10, data = dataframe, family = binomial)

cat("\nModel 7: Smoking Ever + Age -> MI (age-adjusted)\n")
cat("OR for 'Ever' vs 'Never' (adj. for age):",
    round(exp(coef(model_mi_ever_adj)["smoking_everEver"]), 3),
    "| 95% CI:",
    round(exp(suppressMessages(confint(model_mi_ever_adj))["smoking_everEver", ]), 3), "\n")
cat("OR per 10-year age increase (adj. for smoking):",
    round(exp(coef(model_mi_ever_adj)["age_10"]), 3),
    "| 95% CI:",
    round(exp(suppressMessages(confint(model_mi_ever_adj))["age_10", ]), 3), "\n")



# Model 8: Smoking Ever + Age -> Lung Cancer 
model_lc_ever_adj <- glm(lung_cancer ~ smoking_ever + age_10, data = dataframe, family = binomial)

cat("\nModel 8: Smoking Ever + Age -> Lung Cancer (age-adjusted)\n")
cat("OR for 'Ever' vs 'Never' (adj. for age):",
    round(exp(coef(model_lc_ever_adj)["smoking_everEver"]), 3),
    "| 95% CI:",
    round(exp(suppressMessages(confint(model_lc_ever_adj))["smoking_everEver", ]), 3), "\n")
cat("OR per 10-year age increase (adj. for smoking):",
    round(exp(coef(model_lc_ever_adj)["age_10"]), 3),
    "| 95% CI:",
    round(exp(suppressMessages(confint(model_lc_ever_adj))["age_10", ]), 3), "\n")



# Model 9: Smoking Duration + Age -> MI 


# 9a: Unordered ??? per-level ORs vs Never, with age held constant
model_mi_dur_adj_cat <- glm(mi ~ smoking_duration_not_ordinal + age_10, data = dataframe, family = binomial)

cat("\nModel 9a: Smoking Duration + Age -> MI (categorical ORs, age-adjusted)\n")
or_table <- round(exp(cbind(OR = coef(model_mi_dur_adj_cat),
                            suppressMessages(confint(model_mi_dur_adj_cat)))), 3)
rownames(or_table) <- gsub("smoking_duration_not_ordinal", "Duration: ", rownames(or_table))
print(or_table)


# 9b: Ordered ??? linear trend test with age held constant
model_mi_dur_adj_ord <- glm(mi ~ smoking_duration + age_10, data = dataframe, family = binomial)

cat("\nModel 9b: Smoking Duration + Age -> MI (trend test, age-adjusted)\n")
trend_mi_dur_adj <- coef(summary(model_mi_dur_adj_ord))["smoking_duration.L", ]
cat("Linear trend OR:", round(exp(trend_mi_dur_adj["Estimate"]), 3),
    "| p-value:", formatC(trend_mi_dur_adj["Pr(>|z|)"], format = "f", digits = 4), "\n")




# Model 10: Smoking Duration + Age -> Lung Cancer


# 10a: Unordered ??? per-level ORs vs Never, with age held constant
model_lc_dur_adj_cat <- glm(lung_cancer ~ smoking_duration_not_ordinal + age_10, data = dataframe, family = binomial)

cat("\nModel 10a: Smoking Duration + Age -> Lung Cancer (categorical ORs, age-adjusted)\n")
or_table <- round(exp(cbind(OR = coef(model_lc_dur_adj_cat),
                            suppressMessages(confint(model_lc_dur_adj_cat)))), 3)
rownames(or_table) <- gsub("smoking_duration_not_ordinal", "Duration: ", rownames(or_table))
print(or_table)


# 10b: Ordered ??? linear trend test with age held constant
model_lc_dur_adj_ord <- glm(lung_cancer ~ smoking_duration + age_10, data = dataframe, family = binomial)

cat("\nModel 10b: Smoking Duration + Age -> Lung Cancer (trend test, age-adjusted)\n")
trend_lc_dur_adj <- coef(summary(model_lc_dur_adj_ord))["smoking_duration.L", ]
cat("Linear trend OR:", round(exp(trend_lc_dur_adj["Estimate"]), 3),
    "| p-value:", formatC(trend_lc_dur_adj["Pr(>|z|)"], format = "f", digits = 4), "\n")



cat("\nAnalysis complete.\n")
