# Project 1: Hospital Patient Analytics Using Advanced Regression Modeling

## 🏥 Project Overview
This project analyzes patient data from a Virginia medical center, focusing on predicting mortality risk factors and length of stay patterns. The analysis aims to understand the relationships between patient characteristics and healthcare outcomes.

## 📊 Dataset Overview
**Core Statistics:**
- Sample Size: 978 patient records
- Time Period: January - September 2014
- Variables: 13 key indicators including vital signs and demographics
- Primary Outcomes: Mortality and length of stay

## 🔬 Analytical Framework

### Primary Models
1. **Mortality Analysis**
   - Logistic regression modeling
   - Prediction of patient mortality risk
   - Performance validation through ROC analysis

2. **Length of Stay Analysis**
   - Negative binomial regression
   - Modeling of hospitalization duration
   - Assessment of key determining factors

### Statistical Methodology
The analysis employs three complementary approaches:
1. **Model Validation:** 
   - ROC curve analysis for performance assessment
   - Sensitivity and specificity evaluation
   - Optimal threshold determination

2. **Non-linear Analysis:** 
   - GAM modeling for complex relationships
   - Exploration of variable interactions
   - Assessment of non-linear patterns

3. **Interaction Assessment:**
   - Systematic testing of variable combinations
   - Identification of significant interactions
   - Integration into final models

## 📁 How to Use This Repository

Each project folder contains:
- `code.r`: Main R script with statistical analysis 
- `hospital.txt`: Dataset with 978 patient records from Virginia medical center
- `Submission_Aliieva.pdf`: Complete project report with detailed methodology and findings

To reproduce the analysis:
1. Clone the repository
2. Ensure R and required packages are installed
3. Run the code.r script
4. View results in generated outputs
