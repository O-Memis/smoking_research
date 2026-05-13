# smoking_research

## 1) Dataset
- File: smoking_data.csv <br>
- Size: 500 patients, 6 variables <br>
- Variables: id, age, smoking_ever, smoking_duration, lung_cancer, mi <br>
- For the dataset use, contact: [Professor Lukman Thalib](https://avesis.yildiz.edu.tr/l.thalib) <br><br>


## 2) Steps of the Analysis
### 2.1 Data import and correction <br>
Load the CSV file, inspect the structure, check missing values, and correct inconsistent coding in the lung cancer variable. <br><br>

### 2.2 Decoding categorical variables <br>
Convert coded smoking and outcome variables into labeled factors so they are ready for tables, tests, and models. <br><br>

### 2.3 Descriptive statistics and initial plots <br>
Summarize age, produce cross-tabulations, and export basic plots for age, smoking status, smoking duration, lung cancer, and MI. <br><br>

### 2.4 Exploratory comparisons <br>
Calculate percentage-based comparisons between smoking variables and the two outcomes, then visualize these patterns with grouped bar plots. <br><br>

### 2.5 Chi-square testing <br>
Test the association between smoking variables and each outcome, switching to Fisher's exact test when the table counts are too small. <br><br>

### 2.6 Logistic regression <br>
Fit unadjusted and age-adjusted logistic regression models to estimate odds ratios for MI and lung cancer. <br><br>

---



## 3) How to Run
1. Open smoking_analysis.R in RStudio. <br>
2. Set working directory to this project folder (or keep the setwd path in the script). <br>
3. Run all sections from top to bottom. <br><br>

---


## 4) Contact
[Oguzhan Memis](memisoguzhants@gmail.com)

<br>
https://orcid.org/0009-0006-5210-8621 