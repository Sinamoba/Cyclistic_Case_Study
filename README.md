# Google Data Analytics Case Study: Cyclistic
This project is from coursera's Google data analytics course. Cyclistic is bike-share company in Chicago. The marketing analyst team want to investigae the between casual users and members with the goal of maximizing the number of annual membership.
## Tools
**R** is the main programming language used for this project. The report was prepared using Rstudio's **Qurato**, the next-generation version of R Markdown.
## Data Source
This project uses data from [Motivate International Inc.](https://divvy-tripdata.s3.amazonaws.com/index.html). 
To use this project:

1. Download the 12 monthly data files for 2024 from the above link (starting from `202401-divvy-tripdata.zip`).
2. Place them in a `data/divvy-tripdata.s3.amazonaws.com` folder inside the project directory.
3. Run [`prep_data_files.R`](prep_data_files.R)

Note: Data files are excluded from this repo due to size/privacy.

### results
The report is in html format rendered by [`Cyclistic_report.qmd`](Cyclistic_report.qmd) Due to the large size of the data, all plots and serialized R objects (RDS files) were generated and saved in the [main R script](R_main_code.R).
In the Quarto document, these files are loaded for visualization purposes only, and the embedded code is primarily for presentation rather than computation.
To view the report click [here](https://github.com/Sinamoba/Cyclistic_Case_Study/deployments/github-pages)
