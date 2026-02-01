# 📂 Reproduction Materials: SGLT-2 Inhibitor Blood Pressure Meta-Analysis #
This repository serves as a comprehensive resource for transparency and reproducibility of the meta-analysis: 
    "Explaining Blood Pressure Reduction Heterogeneity in SGLT-2 Inhibitor Trials: A Meta-Analysis Stratified by Patient Characteristics."
It contains the statistical code, data extraction tools, and protocol details required to replicate the findings presented in the study, which evaluates the efficacy of SGLT-2 inhibitors on blood pressure across different clinical populations.

## 🛠️ Statistical Analysis Code ## 
This directory contains the R scripts used for all statistical procedures, compatible with R version 4.5.1.

#### Primary Meta-Analysis: 
Scripts for pooling placebo-adjusted systolic and diastolic blood pressure changes using DerSimonian-Laird (DL) and Restricted Maximum Likelihood (REML) random-effects models.
#### Meta-Regressions:
Code for both frequentist and Bayesian multivariable meta-regression models used to explore effect modifiers (Diabetes Status, End-Organ Damage, Measurement Methodology, and Baseline BP).


## 💾 Data Extraction Tools ##
#### Pilot Extraction Form: 
The standardized Word document form used to extract data from the 38 included trials, covering study characteristics, intervention details, BP measurement methodology, and more.
#### LLM Prompts: 
The specific prompts used with Large Language Models (Perplexity, Gemini Pro, etc.) during the hybrid pre-screening and initial data extraction phases.


## 🎙️ Study Protocol ##
#### Protocol Details: 
Documentation of the prospectively registered protocol (PROSPERO ID: CRD420251125931).
#### Search Strategy: 
Detailed search strings and inclusion/exclusion criteria used for PubMed, Embase, and Cochrane’s CENTRAL.


## 🌐 Requirements for reproduction ##
To reproduce the analysis, you will need R Statistical Software (v4.5.1), RStudio and the following packages: brms, meta, RoBMA, metafor

## 📝 For researchers: ##
If you utilize the code or materials from this repository, please cite the original article:
> Alyas, J. T., Nielsen, S. F., & Buus, N. H. (2026). Reproduction Materials for: "Explaining Blood Pressure Reduction Heterogeneity in SGLT-2 Inhibitor Trials: A Meta-Analysis Stratified by Patient Characteristics". Zenodo. https://doi.org/10.5281/zenodo.18451106



> [!NOTE]
For correspondence regarding the data or code, please contact the corresponding author, Josef Toma Alyas at: [joseftomaalyas@gmail.com]
