# The Chen Quantile Regression Model: An Application to the CO2 Emissions Analysis

This repository contains the datasets, scripts, and supplementary materials used in the application study of the Chen quantile regression model to carbon dioxide emissions data.

At the time this application was developed, there was no publicly available R package implementing the Chen quantile regression model. For this reason, all estimation procedures and analyses available in this repository were implemented directly through custom R scripts.

Currently, interested researchers can use the `chenreg` package or an interactive Shiny application, both developed by [Alisson R. P. Paes](https://github.com/AlissonRP), which provide accessible implementations of the Chen quantile regression model in R.

---

# Repository Structure and Main Scripts

Below is a brief description of the main scripts available in this repository.

### Model Implementation

#### `Chen_functions.R`

Contains basic implementations related to the Chen distribution, including the probability density function, cumulative distribution function, quantile function, and random number generation function.

#### `Chen_reg_fit.R`

Implements the Chen quantile regression model through a function that receives the dataset and returns the fitted model object.

#### `Residual_analysis.R`

Computes residuals for the fitted Chen regression models and generates residual diagnostic plots.

### Data Preparation

#### `Agricultural_data_filtering.R`

Filters and processes the agricultural datasets obtained from FAOSTAT.

#### `Application_data_filtering.R`

Filters the CO2 emissions and World Bank datasets, merges all variables, and constructs the final `data_2018.csv` dataset used in the application.

### Application

#### `Application.R`

Contains the complete application to the CO2 emissions dataset, including descriptive analysis, model fitting, and diagnostic procedures for the fitted models.

---

# Chen Regression Package

### `chenreg` R package

GitHub repository:
* https://github.com/AlissonRP/chenreg

The `chenreg` package provides functions for fitting Chen quantile regression models in R, including estimation, inference, diagnostic tools, and prediction procedures for bounded response data.

Researchers interested in reproducing or extending the analyses presented in this repository may use the package as an alternative to manually implementing the estimation routines available in the scripts.

# Interactive Shiny Application

An interactive Shiny application for fitting Chen regression models is also available online:

* https://alissonrp.shinyapps.io/full_app/

The application allows users to upload their own datasets, specify regression structures, estimate Chen regression models, and visualize fitted results and diagnostic measures through a graphical interface.

---

# Dataset Codebook

This dataset contains the variables used to model **carbon dioxide emissions per capita** for the year **2018**.

## Response Variable

#### Carbon dioxide emissions per capita [CO2]

**Unit:** tons per capita

**Source:** Our World in Data – CO₂ Emissions

Annual production-based carbon dioxide (CO₂) emissions per person, excluding land-use change. Measured using territorial emissions, without accounting for emissions embedded in trade.


## Explanatory Variables

#### Daily gas consumption per capita [DGCC]

**Unit:** thousand cubic feet per capita per day

**Source:** World Bank

Daily natural gas consumption per person.

---

#### Daily oil consumption per capita [DOCC]

**Unit:** barrels per capita per day

**Source:** World Bank

Daily oil consumption per person.

---

#### Use of cropland area per capita [UCAC]

**Unit:** hectares per capita

**Source:** FAOSTAT

Area of cropland available per person, including arable land and permanent crops.

---

#### Use of nutrient nitrogen per cropland area [UNNC]

**Unit:** kg/ha

**Source:** FAOSTAT

Total nitrogen fertilizer use per hectare of cropland.

---

#### Oil rents [OR]

**Unit:** % of GDP

**Source:** World Bank

Oil rents represent the difference between the value of crude oil production at regional prices and the total production costs, expressed as a percentage of gross domestic product (GDP).

---

#### Gross per Capita Crop Production Index [GCCP]

**Unit:** index

**Source:** FAOSTAT

Index measuring the relative level of gross crop production per capita, using the average production during **2014–2016 = 100** as the reference base. Values above 100 indicate production per capita above the base period average, while values below 100 indicate lower production per capita.

---

# Data Sources

### Our World in Data

* https://ourworldindata.org/co2-emissions

### World Bank

Home page:
* https://www.worldbank.org/ext/en/home

Downloadable Datasets:
* https://databank.worldbank.org/reports.aspx?source=world-development-indicators

### FAOSTAT

Home page:
* https://www.fao.org/home/en/
  
Downloadable Datasets:
* https://www.fao.org/faostat/en/#data/QI
