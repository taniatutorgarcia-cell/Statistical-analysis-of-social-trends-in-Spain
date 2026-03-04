# Statistical-analysis-of-social-trends-in-Spain
# 📊 Statistical Analysis of Social Trends in Spain  
## Citizens' Perception of Municipal Problems  

Bachelor’s Degree Final Project – Applied Statistics  
Author: Tania Tutor García  

---

## 🎯 Project Objective

The main objective of this study is to identify the sociodemographic, ideological, and territorial factors that influence how citizens perceive the main municipal problems in Spain.

Using microdata from the Spanish Centre for Sociological Research (CIS), different statistical and machine learning classification models were developed and compared to predict which issue is considered the most important by respondents.

---

## 🗂 Data Source

Source: Social Trends Survey – CIS (Spain)

Target variable:
- **PROBLMUN1R** (recoded into the two most frequent categories for binary classification)

Predictor variables:
- PROBLMUN2R  
- PROBLMUN3R  
- EDAD (Age)  
- SEXO (Gender)  
- ESTUDIOS (Education level)  
- ESCIDEOL (Ideological self-placement)  
- INGRESHOG (Household income)  
- TAMUNI (Municipality size)  
- CCAA (Autonomous Community)  
- RELIGION  

These variables were selected based on theoretical relevance and empirical support in sociological and political behavior studies.

---

## ⚙️ Methodology

The analytical workflow included:

1. Data cleaning and preprocessing  
2. Recoding and reduction of the target variable  
3. Train-test split (80% training / 20% testing)  
4. Model fitting and evaluation  

Three classification models were implemented:

- Logistic Regression  
- Random Forest  
- Artificial Neural Network  

Model performance was evaluated using:

- Accuracy  
- Sensitivity  
- Specificity  
- Cohen’s Kappa  
- Balanced Accuracy  

---

## 📈 Models and Results

### 1️⃣ Logistic Regression
- Accuracy: **69.53%**
- Strength: High interpretability through Odds Ratios
- Limitation: Low sensitivity for the minority class

### 2️⃣ Random Forest
- Accuracy: **74.22%**
- Best overall predictive performance
- Most important predictors: PROBLMUN2R, ESCIDEOL, CCAA
- Slight bias toward majority class

### 3️⃣ Artificial Neural Network
- Accuracy: **73.44%**
- Sensitivity (minority class): **45.45%**
- Best balance between precision and minority class detection
- Selected as the final model

---

## 🧠 Key Findings

- Sociocultural and territorial variables (ideology, religion, region of residence) significantly influence problem perception.
- Machine learning models improved predictive performance compared to traditional statistical methods.
- The Artificial Neural Network provided the best trade-off between overall accuracy and minority class detection.

---

## 🚀 Technologies Used

- R  
- tidyverse  
- caret  
- randomForest  
- nnet  
- ggplot2  

---

## 📌 Academic Context

This project was developed as part of a Bachelor’s Degree in Applied Statistics and focuses on the application of statistical modeling and machine learning techniques to real-world sociological survey data.

---

## 📬 Contact

If you would like to discuss this project or collaborate, feel free to connect via LinkedIn.
