# IJC437 Introduction to Data Science Coursework – Sheffield Air Quality (Summer 2025)

This repository contains the R code used for my IJC437 coursework project, analysing OpenAQ air quality measurements for Sheffield (UK) from 1 June to 31 August 2025.  
The project applies a simple, reproducible data science workflow (cleaning, aggregation, visualisation) to explore temporal variation in pollutant concentrations.  
The analysis focuses on PM2.5 as a representative pollutant for time-series exploration.

## Research question
**How do air pollutant concentrations in Sheffield vary over time during the summer of 2025?**

## Key findings
- PM2.5 shows clear temporal variation across the summer, with day-to-day fluctuations and medium-term rises and falls visible in the daily trend and 7-day rolling mean.  
- Diurnal (hour-of-day) and weekly (day-of-week) patterns exist but are relatively weak compared with daily/medium-term variation.  
- A small number of unusually high PM2.5 days were identified using a simple threshold (mean + 2SD), suggesting occasional spikes rather than sustained extreme periods.

## Downloading and running the code

### 1) Download the data
Download the OpenAQ Sheffield dataset (CSV) and save it as:  
`data/openaq_location_2508_measurments.csv`

### 2) Install required R packages
Open R/RStudio and run:
install.packages(c("tidyverse", "lubridate", "zoo", "scales"))

run:
source("IJC437.R")

The script will create an outputs/ folder containing:
outputs/figures/ (saved plots)
outputs/tables/ (summary tables)
outputs/sessionInfo.txt (reproducibility information)

