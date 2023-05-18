# Do Natural Disasters Increase the Risk of Financial Crises?

This repository contains all the files and materials related to my thesis project.

## Contents

- <mark>[**Most Recent Draft**](MasterThesis_UpToDate.pdf)<mark>: Click here for the most recent version of my thesis</mark>

- [R/](R/): A folder containing all the R code used in my project.
  - <mark>[**MainScript**](R/MainScript_DisasterCrisisProject.R)<mark>: The main R script that reproduces all results!
  - [data/](R/data): A folder containing datasets used in my project (some are downloaded directly in the R script).
  - [tables/](R/tables): A folder containing all the .tex tables generated in R.
  - [plots/](R/plots): A folder containing all the plots generated in R.
  - [miscellaneous/](R/miscellaneous): A folder containing old scripts, notes and extra code.

## Replication Files

To reproduce the results of my thesis, you will need to have R installed on your computer, change the directory accordingly, and, run the [MainScript.R](R/MainScript_DisasterCrisisProject.R) in the [R/](R/) folder.

To save time, if you have RStudio installed, the `setwd(dirname(rstudioapi::getActiveDocumentContext()$path))` command at the begging of the [MainScript.R](R/MainScript_DisasterCrisisProject.R) should automatically set the working directory in R to the directory containing the currently active R script and you should be able to immediately run the code.

## Abstract

 Natural disasters pose a serious threat to the stability and prosperity of economies around the world.This paper is the first to directly investigate their impact on crisis-risk. I find that a 0.1\% of GDP increase in damages from natural disasters is associated with a 6% to 18% increase in the odds of a crisis occurring two years later. Importantly, the risk that they pose to the financial system is amplified when they coincide with a credit boom and the results show that natural disasters have persistent negative impacts on economic growth and bank equity returns.



