# EuroBoom: Interactive Analysis of Inflation in Portugal and the EU (2000-2024)

## Grade: 18.5/20 :star:

## Introduction

This project, developed as part of the **Data Visualization and Preparation** course, aims to transform complex inflation data into clear, interactive, and meaningful visual insights. By providing dynamic representations of real-world data, the project enables users to explore trends and understand **Portugal's position within the European Union (EU) inflation landscape from 2000 to 2024**.

Data visualization is crucial for revealing patterns, anomalies, and relationships that remain hidden in raw datasets, supporting deeper economic analysis and communication.

## Project Objectives

The primary goal is to develop interactive visuals that make examining inflation data across the EU **simple and informative**. Specifically, the project focuses on:

* **Comparing Portugal's inflation rates** over time with the EU average across important categories of goods and services.
* **Mapping inflation at the national level** to identify regional trends and outliers.
* **Displaying general inflation trends** over time, annotated with comments for significant economic occurrences (e.g., banking crisis, COVID-19 pandemic, Russia-Ukraine conflict).
* **Ranking EU nations by inflation** each year to clarify Portugal's relative position.

## Research Questions

The visualizations in this project are designed to address the following key questions:

1.  How does Portugal’s inflation vary across different categories of goods and services compared to the EU average in a selected year?
2.  How does Portugal’s overall inflation compare to the EU countries with the highest and lowest inflation in each year?
3.  How has Portugal’s total inflation evolved from 2000 to 2024, and how does it compare to the EU average across years, especially during major economic events?
4.  How does Portugal rank among EU countries in terms of annual inflation, particularly compared to the countries with the highest inflation each year?

## Data Source and Pre-Processing

### Dataset Description

The analysis relies on a single, comprehensive dataset from PORDATA: **Inflation rate (%) by goods and services (Portugal in Europe)**. The data covers annual observations across 27 European countries using the **Harmonised Index of Consumer Prices (HICP)**, ensuring cross-country comparability.

* **Source:** PORDATA.
* **Period:** 1996–2024 (Analysis focused on **2000 onwards**).
* **Key Variables:**
    * `01. Ano` (Year)
    * `02. Nome País (Europa)` (Country Name)
    * `03. Filtro 1` (Consumption Category - includes a **Total** aggregate and 12 detailed groups)
    * `08. Valor` (Inflation Rate, %)

### Pre-Processing Highlights

To ensure accurate and meaningful comparisons, the raw data underwent several pre-processing steps:

1.  **Filtering:** Only records from **2000 onwards** were retained.
2.  **Cleaning:** Non-observational lines were removed, and data types were corrected.
3.  **Language Consistency:** Portuguese country and category names were translated into English to align with mapping and visualization clarity.
4.  **EU Average Calculation:** The annual average inflation for the European Union was calculated for both total and category-specific visualizations.
5.  **Data Structuring:** Datasets were converted to a **long format** suitable for plotting and aggregated by year, country, and category as required by each specific visualization.

## Visualization Components

The interactive dashboard provides a comprehensive view of inflation dynamics through four interconnected visualizations:

| Visualization | Type | Purpose | Interactivity |
| :--- | :--- | :--- | :--- |
| **Inflation by Category** | Animated Bar Chart | Compares Portugal's annual inflation by key consumer categories against the EU average. | Year selection. |
| **Average Inflation by Country** | Animated Choropleth Map | Displays total inflation across EU countries geographically, highlighting regional patterns and extremes. | Year selection. |
| **Total Inflation Over Time** | Connected Dot Plot | Shows the evolution of Portugal’s total inflation vs. the EU average, annotated with major economic events (e.g., Global Financial Crisis, COVID-19). | N/A |
| **Ranking of Annual Percentage Change in Inflation** | Rank Plot | Ranks all EU countries annually by inflation rate. Highlights Portugal and allows selection of other countries for comparison. | Country selection. |

The dashboard combines year selection, clear color-coding, and intuitive layouts to deliver actionable insights to the public, scholars, and policymakers.

## Live Dashboard

The full interactive dashboard, which allows dynamic exploration of inflation trends by country and year, is deployed online.

**Access the live application here:**
[**https://mariana-rc01.shinyapps.io/EuroBoom/**](https://mariana-rc01.shinyapps.io/EuroBoom/)
