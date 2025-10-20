# Degradation Kinetics Analysis in R

This repository contains a collection of R scripts designed to evaluate degradation kinetics in line with the 
[FOCUS Kinetics (2014)](https://esdac.jrc.ec.europa.eu/projects/degradation-kinetics) guideline. It provides a modular and 
transparent framework for analyzing environmental degradation data using various kinetic models.

## 📌 Purpose

The scripts support scientists and regulatory professionals in performing kinetic analyses of degradation studies, 
commonly used in environmental risk assessments (e.g., environmental fate studies). The workflow enables model fitting, 
goodness-of-fit evaluation, and parameter extraction.

## 📂 Contents

- `main.R` – Functions for managing scripts
- `model.R` – Applies selected kinetic models (SFO, DFOP)
- `stats.R` – Applies statistical analysis for goodness-of-fit evaluation
- `plot.R` – Generates plots for visualizing fits and residuals
- `input.R` – Functions for reading and structuring input data
- `output.R` – Functions for reading and structuring output data
- Example datasets for demonstration and testing

## ⚙️ Requirements

- R ≥ 4.4.2
- Packages: `tcltk`, `minpack.lm`, `dplyr`, `ggplot2`

To install the required packages:
```r
install.packages(c("tcltk", "minpack.lm", "dplyr", "ggplot2"))`
```

## 🚀 Getting Started
...installation, clone repository, run scripts...

## 📈 Example
...example code...

## 📜 Licence
...licence
