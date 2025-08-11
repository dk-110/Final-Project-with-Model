# R 4.5.0 started.
# 
# R version 4.5.0 (2025-04-11 ucrt) -- "How About a Twenty-Six"
# Copyright (C) 2025 The R Foundation for Statistical Computing
# Platform: x86_64-w64-mingw32/x64
# 
# R is free software and comes with ABSOLUTELY NO WARRANTY.
# You are welcome to redistribute it under certain conditions.
# Type 'license()' or 'licence()' for distribution details.
# 
#   Natural language support but running in an English locale
# 
# R is a collaborative project with many contributors.
# Type 'contributors()' for more information and
# 'citation()' on how to cite R or R packages in publications.
# 
# Type 'demo()' for some demos, 'help()' for on-line help, or
# 'help.start()' for an HTML browser interface to help.
# Type 'q()' to quit R.
# 
# Get tidy results from your existing model
tidy_survival <- tidy(model_survival, conf.int = TRUE)

tidy_survival %>%
  select(term, estimate, conf.low, conf.high) %>%
  kable(caption = "Model Estimates with Confidence Intervals") %>%
  kable_styling()
# Error in `tidy()`:
# ! could not find function "tidy"
# CHUNK 1: SETUP AND LIBRARY LOADING
# ====================================================================

# Load required libraries
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(broom)
library(kableExtra)
library(viridis)
library(gganimate)
library(transformr)
library(DT)
library(corrplot)
library(reshape2)
library(gridExtra)
library(RColorBrewer)

# Set theme for consistent styling
theme_set(theme_minimal())

# Warning message:
# package ‘readr’ was built under R version 4.5.1 
# 
# Attaching package: ‘dplyr’
# 
# The following objects are masked from ‘package:stats’:
# 
#     filter, lag
# 
# The following objects are masked from ‘package:base’:
# 
#     intersect, setdiff, setequal, union
# 
# 
# Attaching package: ‘plotly’
# 
# The following object is masked from ‘package:ggplot2’:
# 
#     last_plot
# 
# The following object is masked from ‘package:stats’:
# 
#     filter
# 
# The following object is masked from ‘package:graphics’:
# 
#     layout
# 
# 
# Attaching package: ‘kableExtra’
# 
# The following object is masked from ‘package:dplyr’:
# 
#     group_rows
# 
# Warning message:
# package ‘kableExtra’ was built under R version 4.5.1 
# Loading required package: viridisLite
# Warning message:
# package ‘viridis’ was built under R version 4.5.1 
# Warning message:
# package ‘gganimate’ was built under R version 4.5.1 
# Warning message:
# package ‘transformr’ was built under R version 4.5.1 
# Warning message:
# package ‘DT’ was built under R version 4.5.1 
# corrplot 0.95 loaded
# Warning message:
# package ‘corrplot’ was built under R version 4.5.1 
# Warning message:
# package ‘reshape2’ was built under R version 4.5.1 
# 
# Attaching package: ‘gridExtra’
# 
# The following object is masked from ‘package:dplyr’:
# 
#     combine
# 
# CHUNK 2: DATA PREPARATION 
# Load your dataset
cancer_data_clean <- read_csv("cancer_data_cleaned_version.csv")

# MODEL SETUP
model_survival <- glm(Survival_Status == "Deceased" ~ Age + Gender + Cancer_Type +
                      Stage_at_Diagnosis + Diagnosis_Date, data = cancer_data_clean, family = binomial)
# Rows: 10000 Columns: 29
# ── Column specification ───────────────────────────────────────────
# Delimiter: ","
# chr  (22): Patient_ID, Gender, Province, Urban_Rural, Ethnicity...
# dbl   (5): Age, Tumor_Size, Radiation_Sessions, Survival_Months...
# date  (2): Diagnosis_Date, Surgery_Date
# 
# ℹ Use `spec()` to retrieve the full column specification for this data.
# ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
# Get tidy results from your existing model
tidy_survival <- tidy(model_survival, conf.int = TRUE)

tidy_survival %>%
  select(term, estimate, conf.low, conf.high) %>%
  kable(caption = "Model Estimates with Confidence Intervals") %>%
  kable_styling()
# Error in `x$sizingPolicy`:
# ! $ operator is invalid for atomic vectors
#     ▆
#  1. ├─base (local) `<fn>`(x)
#  2. └─kableExtra:::print.kableExtra(x)
#  3.   ├─base::print(html_kable)
#  4.   └─print.shiny.tag.list(html_kable)
#  5.     └─base::isTRUE(x$sizingPolicy$knitr$figure)
# Predicted probabilities
predicted_probs <- predict(model_survival, type = "response")

# Convert probabilities to predicted classes (threshold = 0.5)
predicted_class <- ifelse(predicted_probs > 0.5, "Deceased", "Alive")

# Confusion matrix
table(Predicted = predicted_class, Actual = cancer_data_clean$Survival_Status)
#           Actual
# Predicted  Alive Deceased
#   Alive     3046     1958
#   Deceased  1709     3287

# Step 1 Load the Libraries
library(tidyverse)
library(readr)     # For reading CSV
library(dplyr)     # For data manipulation
library(skimr)     # For data summaries
library(ggplot2)   # Optional: for future visualizations
library(forcats)
library(lubridate)
library(survminer)
library(broom)
library(gt)
library(stargazer)
# ── Attaching core tidyverse packages ─────────── tidyverse 2.0.0 ──
# ✔ forcats   1.0.0     ✔ stringr   1.5.1
# ✔ lubridate 1.9.4     ✔ tibble    3.3.0
# ✔ purrr     1.1.0     ✔ tidyr     1.3.1
# ── Conflicts ───────────────────────────── tidyverse_conflicts() ──
# ✖ gridExtra::combine()     masks dplyr::combine()
# ✖ plotly::filter()         masks dplyr::filter(), stats::filter()
# ✖ kableExtra::group_rows() masks dplyr::group_rows()
# ✖ dplyr::lag()             masks stats::lag()
# ℹ Use the conflicted package ([object Object])  to force all conflicts to become errors
# Warning message:
# package ‘purrr’ was built under R version 4.5.1 
# Warning message:
# package ‘skimr’ was built under R version 4.5.1 
# Loading required package: ggpubr
# Warning messages:
# 1: package ‘survminer’ was built under R version 4.5.1 
# 2: package ‘ggpubr’ was built under R version 4.5.1 
# Warning message:
# package ‘gt’ was built under R version 4.5.1 
# 
# Please cite as: 
# 
#  Hlavac, Marek (2022). stargazer: Well-Formatted Regression and Summary Statistics Tables.
#  R package version 5.2.3. https://CRAN.R-project.org/package=stargazer 
# 
# Load your dataset
cancer_data_clean <- read_csv("cancer_data_cleaned_version.csv")

# View the first few rows
glimpse(cancer_data_clean)
# Rows: 10000 Columns: 29
# ── Column specification ───────────────────────────────────────────
# Delimiter: ","
# chr  (22): Patient_ID, Gender, Province, Urban_Rural, Ethnicity...
# dbl   (5): Age, Tumor_Size, Radiation_Sessions, Survival_Months...
# date  (2): Diagnosis_Date, Surgery_Date
# 
# ℹ Use `spec()` to retrieve the full column specification for this data.
# ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
# Rows: 10,000
# Columns: 29
# $ Patient_ID         <chr> "CN_CA_00001", "CN_CA_00002", "CN_CA_0…
# $ Age                <dbl> 70, 71, 42, 56, 29, 44, 72, 46, 64, 88…
# $ Gender             <chr> "Male", "Male", "Male", "Female", "Mal…
# $ Province           <chr> "Yunnan", "Henan", "Hubei", "Zhejiang"…
# $ Urban_Rural        <chr> "Urban", "Urban", "Rural", "Rural", "R…
# $ Ethnicity          <chr> "Han", "Hui", "Han", "Han", "Han", "Uy…
# $ Occupation         <chr> "Retired", "Factory Worker", "Unemploy…
# $ Insurance_Type     <chr> "Self-pay", "URBMI", "Self-pay", "NRCM…
# $ Family_History     <chr> "No", "No", "No", "No", "No", "Yes", "…
# $ Cancer_Type        <chr> "Esophageal", "Colorectal", "Breast", …
# $ Stage_at_Diagnosis <chr> "I", "II", "III", "IV", "II", "III", "…
# $ Diagnosis_Date     <date> 2012-07-01, 2016-03-03, 2015-01-03, 2…
# $ Symptoms           <chr> "Fatigue, Pain", "Lump, Swelling", "Na…
# $ Metastasis_Sites   <chr> "Liver, Bone", "Liver, Bone", "Liver, …
# $ Tumor_Size         <dbl> 9.5, 12.6, 0.7, 4.2, 13.5, 7.8, 3.8, 1…
# $ Treatment_Types    <chr> "Surgery, Chemotherapy", "Surgery, Che…
# $ Surgery_Date       <date> 2012-07-23, 2018-09-30, 2016-07-25, N…
# $ Chemotherapy_Drugs <chr> "Cisplatin, Paclitaxel", "Gemcitabine,…
# $ Radiation_Sessions <dbl> 26, 29, 10, 9, 29, 18, 17, 15, 0, 14, …
# $ Immunotherapy      <chr> "No", "No", "Yes", "No", "Yes", "No", …
# $ Targeted_Therapy   <chr> "Yes", "No", "No", "Yes", "No", "No", …
# $ Survival_Status    <chr> "Alive", "Alive", "Deceased", "Decease…
# $ Survival_Months    <dbl> NA, NA, 59, 49, NA, 6, 50, NA, NA, NA,…
# $ Recurrence_Status  <chr> "No", "No", "Yes", "No", "Yes", "Yes",…
# $ Smoking_History    <chr> "Never", "Former", "Never", "Former", …
# $ Alcohol_Use        <chr> "Occasional", "Regular", "Never", "Reg…
# $ BMI                <dbl> 19.0, 32.9, 32.3, 20.0, 28.8, 19.8, 28…
# $ Biomarker_1        <chr> "Negative", "Negative", "Negative", "P…
# $ Biomarker_2        <chr> "Low", "Medium", NA, "Low", "Medium", …
library(readr)
library(dplyr)
library(broom)
library(kableExtra)
# Load your dataset
cancer_data_clean <- read_csv("cancer_data_cleaned_version.csv")
model_survival <- glm(Survival_Status == "Deceased" ~ Age + Gender + Cancer_Type +
                      Stage_at_Diagnosis + Diagnosis_Date, data = cancer_data_clean, family = binomial)
# Rows: 10000 Columns: 29
# ── Column specification ───────────────────────────────────────────
# Delimiter: ","
# chr  (22): Patient_ID, Gender, Province, Urban_Rural, Ethnicity...
# dbl   (5): Age, Tumor_Size, Radiation_Sessions, Survival_Months...
# date  (2): Diagnosis_Date, Surgery_Date
# 
# ℹ Use `spec()` to retrieve the full column specification for this data.
# ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
tidy_survival <- tidy(model_survival, conf.int = TRUE)
tidy_survival %>%
  select(term, estimate, conf.low, conf.high) %>%
  kable(caption = "Model Estimates with Confidence Intervals") %>%
  kable_styling()
# Error in `x$sizingPolicy`:
# ! $ operator is invalid for atomic vectors
#     ▆
#  1. ├─base (local) `<fn>`(x)
#  2. └─kableExtra:::print.kableExtra(x)
#  3.   ├─base::print(html_kable)
#  4.   └─print.shiny.tag.list(html_kable)
#  5.     └─base::isTRUE(x$sizingPolicy$knitr$figure)
# Predicted probabilities
predicted_probs <- predict(model_survival, type = "response")

# Convert probabilities to predicted classes (threshold = 0.5)
predicted_class <- ifelse(predicted_probs > 0.5, "Deceased", "Alive")

# Confusion matrix
table(Predicted = predicted_class, Actual = cancer_data_clean$Survival_Status)
#           Actual
# Predicted  Alive Deceased
#   Alive     3046     1958
#   Deceased  1709     3287
