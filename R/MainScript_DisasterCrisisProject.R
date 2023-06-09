# ------------------------------------------------------------------
# Author: Daniel Fichmann
# Date: Spring Semester 2023
# Project: Master's Thesis in Economics - Natural Disasters and Financial Crises
# ------------------------------------------------------------------

# INTRODUCTION ####

## Set working directory
#If you downloaded the whole project from github then the following code should automatically set the working directory 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # or you can use this line to set the working directory to the folder where this script is located

# Load or install required libraries
packages <- c("multiwayvcov", # for clustered standard errors
              "tidyverse", # for data manipulation
              "reshape2", # for melting data
              "patchwork", # for combining plots
              "ggplot2", # for plotting
              "readxl", # for reading excel files
              "janitor", # for cleaning data
              "haven", # for reading stata files
              "dplyr", # for data manipulation
              "tidyr", # for data manipulation
              "panelr", # for panel data
              "car", # for clustered standard errors
              "stargazer", # for regression tables
              "latex2exp", # for latex equations
              "expss", # for tables
              "plm", # for panel data
              "lmtest", # for clustered standard errors
              "ggpubr", # for combining plots
              "vars", # for VARs
              "zoo", # for time series
              "lubridate", # for dates
              "scales", # for dates
              "ggthemes", # for plotting
              "wesanderson", # for colors
              "grid", # for plotting
              "countrycode", # for country codes
              "WDI", # for world bank data
              "wbstats", # for world bank data
              "dotwhisker", # for plotting
              "broom", # for plotting
              "sandwich", # for clustered standard errors
              "lpirfs", # for IRFs 
              "tibble",  #for data manipulation 
              "fixest" ,#for fixed effects 
              "RColorBrewer" ,#for colors 
              "xtable", #for tables 
              "ROCR", #for ROC curves 
              "pROC", #for ROC curves 
              "plotROC", #for ROC curves
              "gridExtra",
              "reshape2") #for plotting

for (package in packages) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

# FUNCTIONS: ####
## Functions for creating lags works on the panel data pdata
lagvar <- function(varname, lags, dataset) {
  x <- paste(deparse(substitute(varname))) # get variable name
  for (i in 1:lags) { # loop over lags
    dataset[[paste(x, "_L", i, sep = "")]] <- lag(dataset[[x]], i) # create lagged variable
  }
  return(dataset) # return dataset with new variables
}
## Functions for creating leads
leadvar <- function(varname, leads, dataset) {
  x <- paste(deparse(substitute(varname)))
  for (i in 1:leads) {
    dataset[[paste(x, "_F", i, sep = "")]] <- lead(dataset[[x]], i)
  }
  return(dataset)
}

# COMPILING DATA ####

## Base Panel Data Set (Empty)

# Making it into Panel Data by merging the disaster data onto a base panel
countrylist <- c(
  "ARG", "AUS", "AUT", "BEL", "BRA", "CAN", "CHL", "TWN", "COL", "CZE", "DNK", "FIN", "FRA",
  "DEU", "GRC", "HKG", "HUN", "ISL", "IDN", "IRL", "ISR", "ITA", "JPN", "KOR", "LUX", "MYS",
  "MEX", "NLD", "NZL", "NOR", "PER", "PHL", "PRT", "RUS", "IND", "SGP", "ZAF", "ESP", "SWE",
  "CHE", "THA", "TUR", "EGY", "GBR", "USA", "VEN"
) # list of countries
year <- 1900:2020 # list of years
basepanel <- NULL # empty dataframe
# loop to create base panel
for (i in countrylist) {
  ISO <- rep(i, each = length(year))
  Yr <- year
  new <- cbind(ISO, Yr)
  basepanel <- rbind(basepanel, new)
}
basepanel <- as.data.frame(basepanel) # make it a dataframe
basepanel$year_var <- basepanel$Yr # for merging
basepanel$Yr <- ymd(basepanel$Yr, truncated = 2L) # for merging

countrymeta <- read_csv("data/country_group.csv", show_col_types = FALSE) # read in country metadata
suppressWarnings({
countrymeta$ISO <- countrycode(countrymeta$country,
  origin = "country.name",
  destination = "iso3c"
) # get ISO codes
})
countrymeta <- subset(countrymeta, select = c(income_group, region, ISO)) # keep only relevant columns

# merge
basepanel <- merge(basepanel, countrymeta, by = c("ISO"), all.x = TRUE) # merge

## JST data
# uncomment the next line to download the data instead:
# JST<-read_dta('https://www.macrohistory.net/app/download/9834512469/JSTdatasetR6.dta?t=1662029183')
JST <- read_dta("data/JSTdatasetR6.dta") # read in data
JST$jst <- 1 # for subsetting later
JST$ISO <- JST$iso # for merging
JST$Yr <- ymd(JST$year, truncated = 2L) # for merging

colnames(JST) # check column names
keep <- c(1, 2, 3, 5, 6, 7, 9, 10, 11, 13, 14, 17, 20, 21, 22, 23, 24, 25, 26, 31, 34, 46, 47, 50, 55, 56, 58, 59, 60, 61, 62) # keep only relevant columns
JST <- subset(JST, select = keep) # keep only relevant columns

JST <- JST[order(JST$ISO, JST$year), ] # order by ISO and year
JST$rloans <- log(JST$tloans / JST$cpi) # create log of real loans
# Calculate first difference of rloans and store it in new variable creditD
JST$creditD <- ave(JST$rloans, JST$ISO, FUN = function(x) c(NA, diff(x))) # create first difference of real loans
# Calculate 5-year moving average of creditD using past observations
JST <- JST %>%
  group_by(ISO) %>%
  mutate(creditMA5 = zoo::rollmeanr(creditD, k = 5, fill = NA)) %>%
  ungroup()

JST$rlrevenue <- log(JST$revenue / JST$cpi) # create log of real revenue
JST$rlrevenueD <- ave(JST$rlrevenue, JST$ISO, FUN = function(x) c(NA, diff(x))) # create first difference of real revenue
# Calculate 5-year moving average of rlrevenueD using past observations
JST <- JST %>%
  group_by(ISO) %>%
  mutate(revenueMA5 = zoo::rollmeanr(rlrevenueD, k = 5, fill = NA)) %>%
  ungroup()

JST$rlexp <- log(JST$expenditure / JST$cpi) # create log of real expenditure
JST$rlexpD <- ave(JST$rlexp, JST$ISO, FUN = function(x) c(NA, diff(x))) # create first difference of real expenditure
# Calculate 5-year moving average of rlexpD using past observations
JST <- JST %>%
  group_by(ISO) %>%
  mutate(expMA5 = zoo::rollmeanr(rlexpD, k = 5, fill = NA)) %>%
  ungroup()

JST$r_eq_capgain <- JST$eq_capgain / JST$cpi # create real equity capital gains

# Calculate 5-year moving average of r_eq_capgain using past observations
JST <- JST %>%
  group_by(ISO) %>%
  mutate(capgainMA5 = zoo::rollmeanr(r_eq_capgain, k = 5, fill = NA)) %>%
  ungroup()

## BVX data
# Loading the crisis data: BVX
BVX <- read_dta("data/BVX_annual_regdata.dta") # Crisis data in the folder
BVX$ISO <- BVX$ISO3 # renaming ISO code for merging
BVX$Yr <- ymd(BVX$year, truncated = 2L) # encoding year as a date
colnames(BVX)
drop <- c("year", "country", "ISO3","ccode","decade")
BVX <- BVX[, !(names(BVX) %in% drop)]
BVX$bvx <- 1 # for subsetting later

## GDP data from World Bank API
gdp_data <- wb(indicator = "NY.GDP.MKTP.CD", country = unique(BVX$ISO), startdate = 1960, enddate = 2020) # get GDP data from World Bank API
# select only the relevant columns
gdp_data <- subset(gdp_data, select = c("iso3c", "date", "value")) # keep only relevant columns
# rename columns
colnames(gdp_data) <- c("ISO", "Yr", "GDP_nominal")
gdp_data$GDP_nominal_000US <- gdp_data$GDP_nominal / 1000 # create GDP in 000 US$
# select only the relevant columns
gdp_data <- subset(gdp_data, select = c("ISO", "Yr", "GDP_nominal_000US"))
# convert Year to date format
gdp_data$Yr <- ymd(gdp_data$Yr, truncated = 2L) # encoding year as a date

## Disaster Data

###### Cleaning
EmDat <- read_excel("data/EmDatPublic202211.xlsx") # EmDat data in the folder
colnames(EmDat)
keep <- c(1, 2, 3, 7, 11, 12, 13, 14, 29, 30, 31, 35, 37, 39, 44, 45, 46)
EmDat <- subset(EmDat, select = keep)
# renaming columns
colnames(EmDat)[4] <- "DisasterType"
colnames(EmDat)[9] <- "StartYear"
colnames(EmDat)[10] <- "StartMonth"
colnames(EmDat)[11] <- "StartDay"
colnames(EmDat)[12] <- "TotalDeath"
colnames(EmDat)[13] <- "NoAffected"
colnames(EmDat)[14] <- "TotalAffected"
colnames(EmDat)[15] <- "TotalDamages_000US"
colnames(EmDat)[16] <- "TotalDamagesAdj_000US"

EmDat$DateYM <- paste(EmDat$StartYear, EmDat$StartMonth, sep = "-") # creating date variable
suppressWarnings({
EmDat$DateYM <- ym(EmDat$DateYM) # encoding date
})
EmDat$year_month <- format(EmDat$DateYM, "%Y-%m") # creating year_month variable
EmDat$Yr <- ymd(EmDat$Year, truncated = 2L) # encoding year as a date

### Adding weights:
EmDat$mWeight <- (12 - EmDat$StartMonth) / 12 # weight formula
# creating weighted variables
for (m in c("TotalDeath", "TotalDamages_000US", "TotalDamagesAdj_000US")) {
  EmDat[, paste("weighted", m, sep = "_")] <- EmDat[, paste(m)] * EmDat$mWeight
}

# # Calculating the country specific median
# EmDat2 <- EmDat %>%
#   group_by(ISO) %>%
#   mutate(median_damage = median(TotalDamagesAdj_000US, na.rm = TRUE)) %>%
#   ungroup()

EmDat <- EmDat %>%
  group_by(ISO) %>%
  mutate(median_damage = median(TotalDamagesAdj_000US[!is.na(TotalDamagesAdj_000US)])) %>%
  ungroup()


### Collapsing / Summarizing

# creating a list of disaster types
disisasterlists <- list(
  MyData_plus = c("Drought", "Earthquake", "Epidemic", "Extreme temperature", "Flood", "Storm", "Volcanic activity", "Wildfire"),
  MyData = c("Earthquake", "Storm", "Volcanic activity", "Wildfire")
)

# looping through the list of disaster types to summarize the data
for (i in seq_along(disisasterlists)) {
  list_name <- names(disisasterlists)[i]
  df_name <- paste(list_name)
  d_types <- disisasterlists[[i]]
  
  # function to sum if not all NA
  sum_if_not_all_na <- function(x) {
    if (all(is.na(x))) { # if all is NA
      NA # return NA
    } else {
      sum(x, na.rm = TRUE) # else return the sum and set the NAs to 0
    }
  }
  # creating a function to calculate strong metric
  calculate_strong_metric <- function(
    filtervar,
    threshold,
    var, # the variable that is actually being summed
    value # the value to return if the filter variable is less than the median (this is set to 0 in the loop below)
    ) {
    ifelse(
      filtervar > threshold, # if the filter variable is greater than the country specific median
      var, # return the variable
      value # else return the value (0)
    )
  }
  # summarizing
  DisasterDF <- subset(EmDat, `DisasterType` %in% d_types) %>%
    group_by(Yr, ISO) %>%
    summarize(
      adj_DAM_000US = sum_if_not_all_na(TotalDamagesAdj_000US),
      DAM_000US = sum_if_not_all_na(TotalDamages_000US),
      DEATH = sum_if_not_all_na(TotalDeath),
      w_adj_DAM_000US = sum_if_not_all_na(weighted_TotalDamagesAdj_000US),
      w_DAM_000US = sum_if_not_all_na(weighted_TotalDamages_000US),
      w_DEATH = sum_if_not_all_na(weighted_TotalDeath),
      w_strong_DAM_000US = sum_if_not_all_na(calculate_strong_metric(filtervar = TotalDamagesAdj_000US, threshold = median_damage, var = weighted_TotalDamages_000US, value = 0)),
      adj_strong_DAM_000US = sum_if_not_all_na(calculate_strong_metric(filtervar = TotalDamagesAdj_000US, threshold = median_damage, var = TotalDamagesAdj_000US, value = 0)),
      strong_DAM_000US = sum_if_not_all_na(calculate_strong_metric(filtervar = TotalDamagesAdj_000US, threshold = median_damage, var = TotalDamages_000US, value = 0)),
      strong_DEATH = sum_if_not_all_na(calculate_strong_metric(filtervar = TotalDamagesAdj_000US, threshold = median_damage, var = TotalDeath, value = 0)),
      w_adj_strong_DAM_000US = sum_if_not_all_na(calculate_strong_metric(filtervar = TotalDamagesAdj_000US, threshold = median_damage, var = weighted_TotalDamages_000US, value = 0)),
      w_strong_DEATH = sum_if_not_all_na(calculate_strong_metric(filtervar = TotalDamagesAdj_000US, threshold = median_damage, var = weighted_TotalDeath, value = 0)),
      NoOfStrongDisaster = sum_if_not_all_na(calculate_strong_metric(filtervar = TotalDamagesAdj_000US, threshold = median_damage, var = 1, value = 0)),
      w_NoOfStrongDisaster = sum_if_not_all_na(calculate_strong_metric(filtervar = TotalDamagesAdj_000US, threshold = median_damage, var = mWeight, value = 0)),
      W_NoOfDis = sum_if_not_all_na(mWeight),
      NoOfDis = n(),
      .groups = "drop"
    )


  ### Merging datasets together
  MyData <- merge(basepanel, DisasterDF, by = c("ISO", "Yr"), all.x = TRUE) # merging basepanel with disaster data
  # MyData <- merge(MyData, addstrong, by=c("ISO","Yr"), all.x = TRUE) #adding strong disaster variables
  MyData <- merge(MyData, JST, by = c("ISO", "Yr"), all.x = TRUE) # adding JST data
  MyData <- merge(MyData, BVX, by = c("ISO", "Yr"), all.x = TRUE) # adding BVX data
  MyData <- merge(MyData, gdp_data, by = c("ISO", "Yr"), all.x = TRUE) # adding GDP data

  ### Making indicators:
  MyData$DisDummy <- ifelse(MyData$NoOfDis >= 1, 1, 0) # everything over 1 is coded as 1
  MyData$StrongDisDummy <- ifelse(MyData$NoOfStrongDisaster >= 1, 1, 0) # everything over 1 is coded as 1

  for (varname in c(
    "DisDummy",
    "StrongDisDummy",
    "w_NoOfStrongDisaster", "NoOfStrongDisaster",
    "W_NoOfDis", "NoOfDis",
    "w_adj_DAM_000US", "w_DAM_000US", "w_DEATH", "w_adj_strong_DAM_000US",
    "w_strong_DAM_000US", "w_strong_DEATH", "adj_DAM_000US", "DAM_000US", "DEATH", "adj_strong_DAM_000US",
    "strong_DAM_000US", "strong_DEATH"
  )) {
    MyData[, paste(varname)][is.na(MyData[, paste(varname)])] <- 0 # coding NA as 0 damage (since EmDat is a list of observations)
  }

  MyData$w_strong_DAM_000US <- replace(MyData$w_strong_DAM_000US, MyData$w_strong_DAM_000US < 0, NA)

  # Creating Panel Data
  MyData <- pdata.frame(MyData, index = c("ISO", "Yr"), drop.index = FALSE, row.names = TRUE) # make panel data


  ## Standardize
  MyData$rGDP <- MyData$rgdpmad * MyData$pop # note that population is in thousands
  # adjusting damage data for inflation base 1990, dividing by RealGdp 1990
  MyData <- lagvar(rGDP, 1, MyData) # lagging real GDP by 1 year
  # standardizing damage data by GDP using lagged GDP to avoid simultaneity bias and to account for the fact that disasters may affect GDP
  MyData$stnDAM_strong <- ((MyData$w_strong_DAM_000US / (100 / MyData$cpi)) / MyData$rGDP_L1) * 1000 # dividng cpi 100 because the index was set to 1990 = 100 # then multiplying by 1000 so that the margin is in 0.1 %of GDP

  MyData$stnDAM <- ((MyData$w_DAM_000US / (100 / MyData$cpi)) / MyData$rGDP) * 1000 # for all damages (not just those above the median)

  MyData$stnDAM_strong_uw <- ((MyData$strong_DAM_000US / (100 / MyData$cpi)) / MyData$rGDP) * 1000 # for unweighted damages

  assign(df_name, MyData)
}

saveRDS(MyData, file = "data/MyData.R") # saving data
saveRDS(MyData_plus, file = "data/MyData_robustcheck.R") # saving data

#-------------------------------------------- 

# ANALYSIS  ####
MyData <- readRDS("data/MyData.R") # loading data
MyData_robustcheck <- readRDS("data/MyData_robustcheck.R") # loading data
## REG TABLES 1-3             #######
source("regtable1.R") # regression tables
source("regtable2.R") # regression tables
source("alldisastertypes.R") #using MyData_robustcheck
source("regtable3.R") # regression tables
## SUM STATS TABLE             #######
source("summarystats1.R") # sum stats table 1
## PLOTS                      #######
source("countrydisasterplots.R") 
source("prediction.R")
source("LPs.R")

#Extension:                   #######
#source("monthly_analysis.R")




