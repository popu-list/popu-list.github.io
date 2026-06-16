# Welcome to the PopuList Github Repo!

This repository includes the files to create the data visualizations and country-reports of the PopuList.

## Repository Structure and R-File/Quarto File Description

#### Folder: Scripts

`a_preprocessing.R`

- Contains all preliminary data cleaning and wrangling steps to create data files for main visualizations

`b_directory_viz.R`

- Creates the country-map visualization, which serves as the country-report directory

`b_homepage_viz.R`

- Creates the visualizations for the homepage of the PopuList

`b_table_preprocessing.R`

- Preprocesses the PopuList table for the interactive and pdf version of the table

`b_populist_table_pdf.R`

- Creates the pdf version of the PopuList

`c_additional_visualizations.R`

- Creates visualizations for the Gallery page on the PopuList website

#### Folder: Countries

`a_render.R`

- Renders all country reports at once. Contains all country-specific information for each country report.

`a_report.qmd`

- The template for each country report. The file `a_render.R` *renders* this file and creates all reports automatically. 

#### Folder: Visualizations/dashboard

`dashboard.qmd`

- Creates the interactive PopuList table.

#### Folder: Data

`manual_election_data.csv`

- The election data manually collected for the period after mid-2023

`P.csv`

- Contains country specific changes in election percentages for each party class

`G_long.csv`

- Contains the per-year election data for all party classes

`processed_populist.csv`

- Contains the cleaned PopuList table for creating the PDF and the dashboard

`The PopuList 4.0.csv`

- The CSV of the latest PopuList version

##### THE FOLLOWING DATASETS NEED TO BE DOWNLOADED FROM EXTERNAL SOURCES AND THEN RENAMED TO MATCH THEIR NAMES IN THE SCRIPTS

`population_2024.xls`

- Contains World Bank population numbers until 2024; from https://data.worldbank.org/indicator/SP.POP.TOTL?most_recent_year_desc=false&view=map

`ppeg_parl_2025v1.csv`

- Contains the PPEG election data; from https://ppeg.wzb.eu/ 

`view_election.csv`

- The election data from Parlgov for the period until mid-2023; from https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/2VZ5ZC


#### Folder: All_Populist_Versions

Contains the versions 1, 2, 3 and 4 of the PopuList
