# Logistic Regression and Classification Models for Goat Herd Data

A compact R script that implements logistic regression workflows on goat herd survey data, including bias-reduced estimation using brglm2, basic preprocessing, and model diagnostics. The script is intended for reproducible, data-driven analysis and quick experimentation with nested model specifications.

## Contents
- `logistic.R`  - Main analysis script. Loads CSV data, declares variables, fits several logistic models including bias-reduced estimation (Firth style) using brglm2, and prints coefficient tables and model summaries.

## Short summary
This repository fits multiple logistic regression specifications to a binary treatment outcome (Treatment.Status) using standard glm and bias-reduced estimation via `brglm2::brglmFit`. It prints coefficient estimates, standard errors, test statistics, p-values, and model summaries to the console. The script is designed to be run interactively inside RStudio or non-interactively with Rscript after updating the data file path.

## Requirements
- R 4.x or later
- R packages:
  - `brglm2`
  - `psych`
  - `detectseparation`

Install required packages from CRAN:
```r
install.packages(c("brglm2", "psych", "detectseparation"))
