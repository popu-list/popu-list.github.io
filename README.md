# Welcome to the PopuList Github Repo!

This repository includes the files to create the data visualizations and country-reports of the PopuList.

## Repository Structure and R-File/Quarto File Description

#### Folder: Scripts

`a_preprocessing.R`

-   Contains all preliminary data cleaning and wrangling steps to create all relevant data files.

`b_directory_viz.R`

- Creates the country-map visualization, which serves as the country-report directory

`b_homepage_viz.R`

- Creates the visualizations for the home-page of the PopuList

`b_populist_table_viz.R`

- Creates the PDF version of the PopuList table

`c_additional_visualizations.R`

- Creates visualizations for the Gallery page on the PopuList website

#### Folder: Countries

`a_render.R`

- Renders all country reports at once. Contains all country-specific information for each country report.

`a_report.qmd`

- The template for each country report. The file `a_render.R` pipes *into* this file. 

#### Folder: Visualizations/dashboard

`dashboard.qmd`

- Creates the interactive PopuList table. IMPORTANT: Changes to the directory script in `b_directory_viz.R` and the PDF populist table functions `b_populist_table_viz.R` need to be copied into this file.  
