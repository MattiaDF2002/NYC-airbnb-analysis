# NYC Airbnb Pricing – Data Analysis with R studio

## Overview
This project analyzes an Airbnb dataset of accommodation listings in **New York City** to explore **pricing patterns, seasonal trends, and factors influencing nightly rates**.

It was developed as part of the *Statistical Methods for Business Analysis* course at **Ca’ Foscari University of Venice**.



## Objectives
- Clean and prepare the raw dataset for analysis.
- Explore key variables such as price, room type, reviews, region, and availability.
- Detect and treat outliers.
- Perform **Exploratory Data Analysis (EDA)**.
- Build a **Linear Regression Model** to identify main price drivers.



## 🛠 Tools & Technologies
- **Language:** R
- **Libraries:** tidyverse, dplyr, ggplot2, moments, corrplot
- **IDE:** RStudio



## 📂 Repository Structure
nyc-airbnb-analysis/
│
├─ R/
│ └─ analysis.Rstudio
│
├─ report/
│ └─ REPORT_MSBA_MDF.pdf 
│
├─ data/
│ └─ new_york_listings_2024.csv 
│
└─ README.md 


## Key Insights
- **Room type** is a significant predictor of price.
- Geographic **region** impacts pricing patterns.
- **Number of bedrooms/beds** influences nightly rates.
- Outlier removal and transformations (log scale) improve model accuracy.


## Final Model
**Model formula:**
log(price) ~ room_type + bedrooms + beds + baths + region

**Adjusted R²:** ~0.37


## How to Run

1. Clone the repository:
   ```bash
   git clone https://github.com/YOUR-USERNAME/nyc-airbnb-analysis.git

2. Open R/analysis.R in RStudio.
3. Install required libraries:

install.packages(c("tidyverse", "dplyr", "ggplot2", "moments", "corrplot"))

4. Run the script to reproduce the analysis.
