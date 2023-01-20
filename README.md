# Code samples

This repository contains samples of code used for my PhD research. My dissertation is titled **Climate, environment, neighborhood, & health: Pediatric asthma in Kansas City** and investigates how the spatial and temporal variation of acute pediatric asthma relates to the complex interactions between social, environmental, and climatic systems. 

## Climate, environment, neighborhood, & health: Pediatric asthma in Kansas City

### Neighborhood effects on pediatric asthma

We took a Bayesian approach to modeling the interactions between race, income, greenspace, and PM<sub>2.5</sub>. Included are code samples for processing data, modeling relationships, and interpreting results. These samples are not fully inclusive of this research but rather an overview of techniques, process, and style.

1. Data processing:
  - Using [tidycensus to download TIGER tract geometries and calculating the daily acute asthma count](spatial-diss/asthma_tracts.R) per tract
  - Using Google Earth Engine to calculate [land surface temperature (LST)](spatial-diss/daily_LST_tracts.R) and [fractional vegetation (Fr)](spatial-diss/daily_Fr_tracts.R) for each census tract
  - [Merging datasets and calculating summary measures](spatial-diss/data_merge.R) for each census tract

2. Models:
  - Running and saving [Bayesian models](spatial-diss/models.R), including specifying priors for the parameters
  
3. Plots:
  - [Data visualization](spatial-diss/plots.R) including mapping spatial variation and evaluating model output

4. Tables:
  - Creating publication-ready [LaTeX tables](spatial-diss/tables.R)



