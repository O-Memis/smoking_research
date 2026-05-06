# Smoking research statistical analysis
# Dataset: smoking_data.csv.csv (n = 500)
# Tasks covered in code: 1) Descriptive, 2) Exploratory, 3) Chi-square, 4) Logistic regression

# Section 0: Setup
setwd("C:/Users/EXCALIBUR/Documents/0-----------------DERS-KİTAPLIK/SOFTWARES/R/smoking_research")
rm(list = ls())

# Section 1: Data import and cleaning
dataframe <- read.csv("smoking_data.csv.csv", stringsAsFactors = FALSE)

str(dataframe)
cat("Rows:", nrow(dataframe), "| Columns:", ncol(dataframe), "\n")
cat("\nMissing values per column:\n")
print(colSums(is.na(dataframe)))

# Data quality fix: two rows use text "No" instead of numeric code 2
# User-provided coding is: 1 = No, 2 = Yes for lung_cancer
if ("No" %in% dataframe$lung_cancer) {
  dataframe$lung_cancer[dataframe$lung_cancer == "No"] <- "1"
}
dataframe$lung_cancer <- as.integer(dataframe$lung_cancer)

# Recode variables using the requested coding map
# smoking_ever: 1 = Never, 2 = Ever
dataframe$smoking_ever <- factor(
  dataframe$smoking_ever,
  levels = c(1, 2),
  labels = c("Never", "Ever")
)

# smoking_duration: 1 = Never, 2 = <1 year, 3 = 2-5 years, 4 = >5 years
dataframe$smoking_duration <- factor(
  dataframe$smoking_duration,
  levels = c(1, 2, 3, 4),
  labels = c("Never", "<1 yr", "2-5 yrs", ">5 yrs"),
  ordered = TRUE
)

# lung_cancer: 1 = No, 2 = Yes
# mi: 1 = No, 2 = Yes
dataframe$lung_cancer <- factor(dataframe$lung_cancer, levels = c(1, 2), labels = c("No", "Yes"))
dataframe$mi <- factor(dataframe$mi, levels = c(1, 2), labels = c("No", "Yes"))

str(dataframe)

# Section 2: Task 1 descriptive statistics
cat("\nTask 1: Descriptive statistics\n")

cat("\nAge summary\n")
cat("Mean   :", round(mean(dataframe$age), 1), "\n")
cat("Median :", median(dataframe$age), "\n")
cat("SD     :", round(sd(dataframe$age), 2), "\n")
cat("Range  :", range(dataframe$age), "\n")

print_frequency_table <- function(variable_values, variable_label) {
  cat("\n", variable_label, "\n", sep = "")
  count_table <- table(variable_values)
  percent_table <- round(prop.table(count_table) * 100, 1)
  print(rbind(Count = count_table, Percent = percent_table))
}

print_frequency_table(dataframe$smoking_ever, "Smoking Ever")
print_frequency_table(dataframe$smoking_duration, "Smoking Duration")
print_frequency_table(dataframe$lung_cancer, "Lung Cancer")
print_frequency_table(dataframe$mi, "Myocardial Infarction (MI)")

cat("\nCross-tab: Smoking Ever x MI\n")
print(table(Smoking = dataframe$smoking_ever, MI = dataframe$mi))

cat("\nCross-tab: Smoking Ever x Lung Cancer\n")
print(table(Smoking = dataframe$smoking_ever, LungCancer = dataframe$lung_cancer))

cat("\nCross-tab: Smoking Duration x MI\n")
print(table(Duration = dataframe$smoking_duration, MI = dataframe$mi))

cat("\nCross-tab: Smoking Duration x Lung Cancer\n")
print(table(Duration = dataframe$smoking_duration, LungCancer = dataframe$lung_cancer))

png("task1_age_distribution.png", width = 700, height = 500)
hist(
  dataframe$age,
  main = "Age Distribution of Patients",
  xlab = "Age (years)",
  col = "steelblue",
  border = "white",
  breaks = 15
)
dev.off()

png("task1_smoking_ever.png", width = 600, height = 500)
barplot(
  table(dataframe$smoking_ever),
  main = "Patients by Smoking Status",
  xlab = "Smoking Status",
  ylab = "Count",
  col = c("steelblue", "tomato")
)
dev.off()

png("task1_smoking_duration.png", width = 700, height = 500)
barplot(
  table(dataframe$smoking_duration),
  main = "Patients by Smoking Duration",
  xlab = "Duration",
  ylab = "Count",
  col = c("grey70", "skyblue", "steelblue", "navy")
)
dev.off()

png("task1_lung_cancer.png", width = 600, height = 500)
barplot(
  table(dataframe$lung_cancer),
  main = "Lung Cancer Cases",
  xlab = "Lung Cancer",
  ylab = "Count",
  col = c("steelblue", "tomato")
)
dev.off()

png("task1_mi.png", width = 600, height = 500)
barplot(
  table(dataframe$mi),
  main = "Myocardial Infarction (MI) Cases",
  xlab = "MI",
  ylab = "Count",
  col = c("steelblue", "tomato")
)
dev.off()

cat("Task 1 plots saved to working directory.\n")

# Section 3: Task 2 exploratory associations
cat("\nTask 2: Exploratory associations\n")

# margin = 1 gives row percentages (within each smoking group)
mi_rate_by_smoking_status <- prop.table(table(dataframe$smoking_ever, dataframe$mi), margin = 1) * 100
lung_cancer_rate_by_smoking_status <- prop.table(table(dataframe$smoking_ever, dataframe$lung_cancer), margin = 1) * 100
mi_rate_by_smoking_duration <- prop.table(table(dataframe$smoking_duration, dataframe$mi), margin = 1) * 100
lung_cancer_rate_by_smoking_duration <- prop.table(table(dataframe$smoking_duration, dataframe$lung_cancer), margin = 1) * 100

cat("\nMI rate (%) by Smoking Ever\n")
print(round(mi_rate_by_smoking_status, 1))

cat("\nLung Cancer rate (%) by Smoking Ever\n")
print(round(lung_cancer_rate_by_smoking_status, 1))

cat("\nMI rate (%) by Smoking Duration\n")
print(round(mi_rate_by_smoking_duration, 1))

cat("\nLung Cancer rate (%) by Smoking Duration\n")
print(round(lung_cancer_rate_by_smoking_duration, 1))

png("task2_mi_by_smoking_ever.png", width = 700, height = 500)
barplot(
  t(mi_rate_by_smoking_status),
  beside = TRUE,
  main = "MI Rate (%) by Smoking Status",
  xlab = "Smoking Status",
  ylab = "Percentage (%)",
  col = c("steelblue", "tomato"),
  legend.text = c("MI: No", "MI: Yes"),
  args.legend = list(x = "topright")
)
dev.off()

png("task2_lungcancer_by_smoking_ever.png", width = 700, height = 500)
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

png("task2_mi_by_smoking_duration.png", width = 800, height = 500)
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

png("task2_lungcancer_by_smoking_duration.png", width = 800, height = 500)
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

cat("Task 2 plots saved to working directory.\n")

# Section 4: Task 3 hypothesis testing (chi-square)
cat("\nTask 3: Hypothesis testing\n")

run_chi_square_test <- function(first_variable, second_variable, relationship_label) {
  contingency_table <- table(first_variable, second_variable)
  chi_square_result <- suppressWarnings(chisq.test(contingency_table))

  cat("\n", relationship_label, "\n", sep = "")
  cat("H0: No association between variables\n")
  cat("H1: There is an association\n")
  cat(sprintf(
    "chi2 = %.3f | df = %d | p = %.4f\n",
    chi_square_result$statistic,
    chi_square_result$parameter,
    chi_square_result$p.value
  ))

  minimum_expected_count <- min(chi_square_result$expected)

  if (minimum_expected_count < 5) {
    cat(
      "Warning: min expected count =", round(minimum_expected_count, 2),
      "-> using Fisher's Exact Test\n"
    )
    fisher_result <- fisher.test(contingency_table)
    cat(sprintf("Fisher p = %.4f\n", fisher_result$p.value))
    final_p_value <- fisher_result$p.value
  } else {
    cat("Assumption OK (min expected =", round(minimum_expected_count, 2), ")\n")
    final_p_value <- chi_square_result$p.value
  }

  if (final_p_value < 0.05) {
    cat("Decision: Significant (p < 0.05), reject H0\n")
  } else {
    cat("Decision: Not significant (p >= 0.05), fail to reject H0\n")
  }
}

run_chi_square_test(dataframe$smoking_ever, dataframe$mi, "Smoking Ever x MI")
run_chi_square_test(dataframe$smoking_ever, dataframe$lung_cancer, "Smoking Ever x Lung Cancer")
run_chi_square_test(dataframe$smoking_duration, dataframe$mi, "Smoking Duration x MI")
run_chi_square_test(dataframe$smoking_duration, dataframe$lung_cancer, "Smoking Duration x Lung Cancer")

# Section 5: Task 4 logistic regression
# family = binomial specifies logistic regression for binary outcomes.
cat("\nTask 4: Logistic regression\n")

dataframe$mi_binary <- ifelse(dataframe$mi == "Yes", 1, 0)
dataframe$lung_cancer_binary <- ifelse(dataframe$lung_cancer == "Yes", 1, 0)

print_logistic_results <- function(logistic_model, model_label) {
  cat("\n", model_label, "\n", sep = "")
  coefficient_summary <- summary(logistic_model)$coefficients
  odds_ratio_values <- exp(coef(logistic_model))
  confidence_interval_values <- exp(suppressMessages(confint(logistic_model)))
  p_values <- coefficient_summary[, 4]

  result_table <- data.frame(
    OR = round(odds_ratio_values, 3),
    CI_lower = round(confidence_interval_values[, 1], 3),
    CI_upper = round(confidence_interval_values[, 2], 3),
    p_value = round(p_values, 4)
  )

  print(result_table)
}

model_1_unadjusted_mi <- glm(mi_binary ~ smoking_ever, data = dataframe, family = binomial)
model_2_unadjusted_lung_cancer <- glm(lung_cancer_binary ~ smoking_ever, data = dataframe, family = binomial)
model_3_adjusted_mi <- glm(mi_binary ~ smoking_ever + age, data = dataframe, family = binomial)
model_4_adjusted_lung_cancer <- glm(lung_cancer_binary ~ smoking_ever + age, data = dataframe, family = binomial)
model_5_dose_response_mi <- glm(mi_binary ~ as.numeric(smoking_duration), data = dataframe, family = binomial)
model_6_dose_response_lung_cancer <- glm(lung_cancer_binary ~ as.numeric(smoking_duration), data = dataframe, family = binomial)

print_logistic_results(model_1_unadjusted_mi, "Model 1 - Unadjusted: Smoking Ever -> MI")
print_logistic_results(model_2_unadjusted_lung_cancer, "Model 2 - Unadjusted: Smoking Ever -> Lung Cancer")
print_logistic_results(model_3_adjusted_mi, "Model 3 - Age-adjusted: Smoking Ever + Age -> MI")
print_logistic_results(model_4_adjusted_lung_cancer, "Model 4 - Age-adjusted: Smoking Ever + Age -> Lung Cancer")
print_logistic_results(model_5_dose_response_mi, "Model 5 - Dose-response: Smoking Duration -> MI")
print_logistic_results(model_6_dose_response_lung_cancer, "Model 6 - Dose-response: Smoking Duration -> Lung Cancer")

cat("\nConfounding check (OR for Smoking Ever vs Never)\n")
cat(sprintf(
  "MI: Unadjusted OR = %.3f | Age-adjusted OR = %.3f\n",
  exp(coef(model_1_unadjusted_mi)["smoking_everEver"]),
  exp(coef(model_3_adjusted_mi)["smoking_everEver"])
))
cat(sprintf(
  "Lung Cancer: Unadjusted OR = %.3f | Age-adjusted OR = %.3f\n",
  exp(coef(model_2_unadjusted_lung_cancer)["smoking_everEver"]),
  exp(coef(model_4_adjusted_lung_cancer)["smoking_everEver"])
))

create_odds_ratio_forest_plot <- function(
  odds_ratio_values,
  lower_confidence_values,
  upper_confidence_values,
  label_values,
  plot_title,
  output_filename
) {
  png(output_filename, width = 750, height = 450)

  number_of_points <- length(odds_ratio_values)
  x_axis_maximum <- max(upper_confidence_values, na.rm = TRUE) + 0.5

  plot(
    odds_ratio_values,
    seq_len(number_of_points),
    xlim = c(0, x_axis_maximum),
    pch = 15,
    cex = 1.5,
    xlab = "Odds Ratio (95% CI)",
    yaxt = "n",
    ylab = "",
    main = plot_title
  )

  axis(2, at = seq_len(number_of_points), labels = label_values, las = 1)
  arrows(
    lower_confidence_values,
    seq_len(number_of_points),
    upper_confidence_values,
    seq_len(number_of_points),
    angle = 90,
    code = 3,
    length = 0.08
  )
  abline(v = 1, lty = 2, col = "red")

  dev.off()
}

create_odds_ratio_forest_plot(
  odds_ratio_values = c(
    exp(coef(model_1_unadjusted_mi)["smoking_everEver"]),
    exp(coef(model_3_adjusted_mi)["smoking_everEver"])
  ),
  lower_confidence_values = c(
    exp(suppressMessages(confint(model_1_unadjusted_mi))["smoking_everEver", 1]),
    exp(suppressMessages(confint(model_3_adjusted_mi))["smoking_everEver", 1])
  ),
  upper_confidence_values = c(
    exp(suppressMessages(confint(model_1_unadjusted_mi))["smoking_everEver", 2]),
    exp(suppressMessages(confint(model_3_adjusted_mi))["smoking_everEver", 2])
  ),
  label_values = c("Unadjusted", "Age-adjusted"),
  plot_title = "OR for Smoking (Ever vs Never) on MI",
  output_filename = "task4_OR_smoking_MI.png"
)

create_odds_ratio_forest_plot(
  odds_ratio_values = c(
    exp(coef(model_2_unadjusted_lung_cancer)["smoking_everEver"]),
    exp(coef(model_4_adjusted_lung_cancer)["smoking_everEver"])
  ),
  lower_confidence_values = c(
    exp(suppressMessages(confint(model_2_unadjusted_lung_cancer))["smoking_everEver", 1]),
    exp(suppressMessages(confint(model_4_adjusted_lung_cancer))["smoking_everEver", 1])
  ),
  upper_confidence_values = c(
    exp(suppressMessages(confint(model_2_unadjusted_lung_cancer))["smoking_everEver", 2]),
    exp(suppressMessages(confint(model_4_adjusted_lung_cancer))["smoking_everEver", 2])
  ),
  label_values = c("Unadjusted", "Age-adjusted"),
  plot_title = "OR for Smoking (Ever vs Never) on Lung Cancer",
  output_filename = "task4_OR_smoking_LungCancer.png"
)

smoking_duration_levels <- levels(dataframe$smoking_duration)
mi_rate_by_duration <- tapply(dataframe$mi_binary, dataframe$smoking_duration, mean) * 100
lung_cancer_rate_by_duration <- tapply(dataframe$lung_cancer_binary, dataframe$smoking_duration, mean) * 100

png("task4_doseresponse_MI.png", width = 700, height = 500)
barplot(
  mi_rate_by_duration,
  names.arg = smoking_duration_levels,
  main = "MI Rate (%) by Smoking Duration - Dose-response",
  xlab = "Smoking Duration",
  ylab = "MI Rate (%)",
  col = "steelblue",
  ylim = c(0, 100)
)
dev.off()

png("task4_doseresponse_LungCancer.png", width = 700, height = 500)
barplot(
  lung_cancer_rate_by_duration,
  names.arg = smoking_duration_levels,
  main = "Lung Cancer Rate (%) by Smoking Duration - Dose-response",
  xlab = "Smoking Duration",
  ylab = "Lung Cancer Rate (%)",
  col = "tomato",
  ylim = c(0, 100)
)
dev.off()

cat("Task 4 plots saved to working directory.\n")
cat("\nAnalysis complete. All PNG plots saved to the working directory.\n")
