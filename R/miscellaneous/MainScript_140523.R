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
              "xtable" ,#for tables 
              "ROCR" ,#for ROC curves 
              "pROC" ,#for ROC curves 
              "plotROC")#for ROC curves

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

countrymeta <- read_csv("data/country_group.csv") # read in country metadata
countrymeta$ISO <- countrycode(countrymeta$country,
  origin = "country.name",
  destination = "iso3c"
) # get ISO codes
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
EmDat$DateYM <- ym(EmDat$DateYM) # encoding date
EmDat$year_month <- format(EmDat$DateYM, "%Y-%m") # creating year_month variable
EmDat$Yr <- ymd(EmDat$Year, truncated = 2L) # encoding year as a date

### Adding weights:
EmDat$mWeight <- (12 - EmDat$StartMonth) / 12 # weight formula
# creating weighted variables
for (m in c("TotalDeath", "TotalDamages_000US", "TotalDamagesAdj_000US")) {
  EmDat[, paste("weighted", m, sep = "_")] <- EmDat[, paste(m)] * EmDat$mWeight
}

# The following is no longer needed
EmDat_Strong <- EmDat %>%
  group_by(ISO) %>%
  mutate(median_damage = median(TotalDamagesAdj_000US, na.rm = TRUE)) %>%
  filter(TotalDamagesAdj_000US > median_damage) %>%
  ungroup()
#

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
    filter_var, # variable to filter the median by (this is set to TotalDamagesAdj_000US in the loop below)
    var, # the variable that is actually being summed
    value # the value to return if the filter variable is less than the median (this is set to 0 in the loop below)
    ) {
    ifelse(
      filter_var > quantile(filter_var, 0.5, na.rm = TRUE), # if the filter variable is greater than the median
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
      w_strong_DAM_000US = sum_if_not_all_na(calculate_strong_metric(TotalDamagesAdj_000US, weighted_TotalDamages_000US, 0)),
      adj_strong_DAM_000US = sum_if_not_all_na(calculate_strong_metric(TotalDamagesAdj_000US, TotalDamagesAdj_000US, 0)),
      strong_DAM_000US = sum_if_not_all_na(calculate_strong_metric(TotalDamagesAdj_000US, TotalDamages_000US, 0)),
      strong_DEATH = sum_if_not_all_na(calculate_strong_metric(TotalDamagesAdj_000US, TotalDeath, 0)),
      w_adj_strong_DAM_000US = sum_if_not_all_na(calculate_strong_metric(TotalDamagesAdj_000US, weighted_TotalDamages_000US, 0)),
      w_strong_DEATH = sum_if_not_all_na(calculate_strong_metric(TotalDamagesAdj_000US, weighted_TotalDeath, 0)),
      NoOfStrongDisaster = sum_if_not_all_na(calculate_strong_metric(TotalDamagesAdj_000US, 1, 0)),
      w_NoOfStrongDisaster = sum_if_not_all_na(calculate_strong_metric(TotalDamagesAdj_000US, mWeight, 0)),
      W_NoOfDis = sum_if_not_all_na(mWeight),
      NoOfDis = n(),
      .groups = "drop"
    )


  ###############################
  # DisasterDF <- replace(DisasterDF, is.na(DisasterDF), -99999)
  # addstrong <- replace(addstrong, is.na(addstrong), -99999)

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
source("regtable1.R") # regression table 1
source("regtable2.R") # regression table 2
source("regtable3.R") # regression table 3
## SUM STATS TABLE             #######
source("summarystats1.R") # sum stats table 1
## PLOTS                      #######
source("countrydisasterplots.R") # plots of disasters
source("prediction.R")
source("LPs.R")














########## Monthly test? ###########
# Loading the crisis data: BVX

BVXmonthly <- read_dta("data/master_data_monthly.dta") # Crisis data in the folder
# Transform into year and month variables
BVXmonthly$year_variable <- as.integer(substring(BVXmonthly$date, 1, 4))
BVXmonthly$month_variable <- as.integer(substring(BVXmonthly$date, 6))

# Transform into date variable
BVXmonthly$date <- as.Date(paste(BVXmonthly$year_variable, BVXmonthly$month_variable, "1", sep = "-"), format = "%Y-%m-%d")
# Convert to ISO codes
BVXmonthly$ISO <- countrycode(BVXmonthly$country, "country.name", "iso3c")
BVXmonthly <- subset(BVXmonthly, select = c("sB", "sC", "rN", "rB", "date", "year_variable", "month_variable", "ISO"))

# Convert date to year and month format
BVXmonthly$year_month <- format(BVXmonthly$date, "%Y-%m")




# Set start and end dates
start_date <- ymd("1900-01-01")
end_date <- ymd("2018-12-01")

# Create a sequence of monthly dates
monthly_dates <- seq(start_date, end_date, by = "month")

# Create a data frame with all combinations of countries and monthly dates
countrylist <- unique(BVXmonthly$ISO)
panel_data <- expand.grid(ISO = countrylist, date = monthly_dates)

# Add separate year and month columns
panel_data$Yr <- year(panel_data$date)
panel_data$Mo <- month(panel_data$date)

# Convert date to year and month format
panel_data$year_month <- format(panel_data$date, "%Y-%m")
panel_data <- panel_data[, c("ISO", "year_month")]

# Merge with basepanel
MyMonthlyData <- merge(panel_data, BVXmonthly, by = c("ISO", "year_month"), all.x = TRUE)

DisasterDF_month <- subset(EmDat, `DisasterType` %in% disasterlist) %>%
  group_by(year_month, ISO) %>% # grouping over year and country
  summarize(
    adj_DAM_000US = sum(TotalDamagesAdj_000US),
    DAM_000US = sum(TotalDamages_000US),
    DEATH = sum(TotalDeath),
    w_adj_DAM_000US = sum(weighted_TotalDamagesAdj_000US),
    w_DAM_000US = sum(weighted_TotalDamages_000US),
    w_DEATH = sum(weighted_TotalDeath),
    NoOfDis = n(),
    .groups = "drop"
  )

addstrong_month <- subset(EmDat, `DisasterType` %in% disasterlist &
  TotalDamagesAdj_000US > median(EmDat$TotalDamagesAdj_000US, na.rm = TRUE)) %>% # only abovemedian disasters
  group_by(year_month, ISO) %>%
  summarize(
    adj_strong_DAM_000US = sum(TotalDamagesAdj_000US),
    strong_DAM_000US = sum(TotalDamages_000US),
    strong_DEATH = sum(TotalDeath),
    w_adj_strong_DAM_000US = sum(weighted_TotalDamagesAdj_000US),
    w_strong_DAM_000US = sum(weighted_TotalDamages_000US),
    w_strong_DEATH = sum(weighted_TotalDeath),
    NoOfStrongDis = n(),
    .groups = "drop"
  )

MyMonthlyData <- merge(MyMonthlyData, DisasterDF_month, by = c("ISO", "year_month"), all.x = TRUE)
MyMonthlyData <- merge(MyMonthlyData, addstrong_month, by = c("ISO", "year_month"), all.x = TRUE)
MyMonthlyData <- pdata.frame(MyMonthlyData, index = c("ISO", "year_month"), drop.index = FALSE, row.names = TRUE)

MyMonthlyData$DisDummy <- ifelse(MyMonthlyData$NoOfDis >= 1, 1, 0) # everything over 1 is coded as 1
MyMonthlyData$StrongDisDummy <- ifelse(MyMonthlyData$NoOfStrongDis >= 1, 1, 0)

for (varname in c(
  "DisDummy", "StrongDisDummy", "w_adj_DAM_000US", "w_DAM_000US", "w_DEATH", "w_adj_strong_DAM_000US",
  "w_strong_DAM_000US", "w_strong_DEATH", "adj_DAM_000US", "DAM_000US", "DEATH", "adj_strong_DAM_000US",
  "strong_DAM_000US", "strong_DEATH"
)) {
  MyMonthlyData[, paste(varname)][is.na(MyMonthlyData[, paste(varname)])] <- 0 # coding NA as 0 damage (since EmDat is a list of observations)
}

MyMonthlyData <- lagvar(adj_strong_DAM_000US, 4, MyMonthlyData)
MyMonthlyData <- lagvar(adj_DAM_000US, 4, MyMonthlyData)

MyMonthlyData2 <- filter(MyMonthlyData, Yr > 1950)
test <- lm(rB ~ adj_DAM_000US + adj_DAM_000US_L1 +
  adj_DAM_000US_L2 + adj_DAM_000US_L3 +
  factor(ISO), MyMonthlyData2)
summary(test)

MyMonthlyData$DisDummy <- ifelse(MyMonthlyData$NoOfDis >= 1, 1, 0) # everything over 1 is coded as 1
MyMonthlyData <- lagvar(StrongDisDummy, 4, MyMonthlyData)
test <- lm(sC ~ StrongDisDummy + StrongDisDummy_L1 +
  StrongDisDummy_L2 + StrongDisDummy_L3 +
  factor(ISO), MyMonthlyData)
summary(test)


######## Local Projections ######

MyMonthlyData <- filter(MyMonthlyData, Yr > 1950)

for (i in 1:12) {
  MyMonthlyData <- MyMonthlyData %>%
    arrange(ISO, year_month) %>%
    group_by(ISO) %>%
    mutate(!!paste0("sC_F", i) := dplyr::lead(sC, i))
}

vars <- list(MyMonthlyData$sC)
for (i in 1:12) {
  vars[[i + 1]] <- MyMonthlyData[[paste0("sC_F", i)]]
}

for (i in 1:12) {
  MyMonthlyData <- MyMonthlyData %>%
    arrange(ISO, year_month) %>%
    group_by(ISO) %>%
    mutate(!!paste0("rB_F", i) := dplyr::lead(rB, i))
}
vars <- list(MyMonthlyData$rB)
for (i in 1:12) {
  vars2[[i + 1]] <- MyMonthlyData[[paste0("rB_F", i)]]
}

for (i in 1:12) {
  MyMonthlyData <- MyMonthlyData %>%
    arrange(ISO, year_month) %>%
    group_by(ISO) %>%
    mutate(!!paste0("sB_F", i) := dplyr::lead(sB, i))
}
vars3 <- list(MyMonthlyData$sB)
for (i in 1:12) {
  vars3[[i + 1]] <- MyMonthlyData[[paste0("sB_F", i)]]
}

for (i in 1:12) {
  MyMonthlyData <- MyMonthlyData %>%
    arrange(ISO, year_month) %>%
    group_by(ISO) %>%
    mutate(!!paste0("rN_F", i) := dplyr::lead(rN, i))
}
vars4 <- list(MyMonthlyData$rN)
for (i in 1:12) {
  vars4[[i + 1]] <- MyMonthlyData[[paste0("rN_F", i)]]
}

# List to store regressions
reglist <- list()
for (i in 1:length(vars2)) {
  x <- vars2[[i]]
  reglist[[i]] <- lm(
    x ~ StrongDisDummy
      + factor(ISO),
    MyMonthlyData
  )
  reglist[[i]] <- coeftest(reglist[[i]], vcov = vcovHC(reglist[[i]], cluster = "ISO", type = "HC2"))
  model <- tidy(reglist[[i]], conf.int = TRUE, conf.level = 0.9) %>%
    filter(term == "StrongDisDummy") %>%
    mutate(model = paste("h=", i, sep = ""))
  if (i == 1) {
    regdf <- model
  } else {
    regdf <- rbind(regdf, model)
  }
}

regdf <- rbind(tibble(
  term = "StrongDisDummy",
  estimate = 0,
  std.error = 0,
  statistic = 0,
  p.value = 0,
  conf.low = 0,
  conf.high = 0,
  model = "h=0"
), regdf)
regdf$lp <- 1

pd <- position_dodge(0.1)
gdplp <- ggplot(regdf, aes(model, estimate, group = lp)) +
  geom_point() +
  geom_line(linetype = "dashed", color = "blue", size = .8) +
  theme_bw() +
  geom_hline(yintercept = 0) +
  scale_y_continuous("log change in real GDP") +
  xlab("Years after disaster") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1, position = pd)

gdplp


######### Monthly crises data #########

BVX_Monthly_List_of_Crises <- read_excel("data/BVX_Monthly_List_of_Crises.xlsx")
BVX_Monthly_List_of_Crises$year_month <- format(parse_date_time(BVX_Monthly_List_of_Crises$Panic_date, orders = "%B %Y"), "%Y-%m")
BVX_Monthly_List_of_Crises$ISO <- countrycode(BVX_Monthly_List_of_Crises$Country, "country.name", "iso3c")

MyMonthlyData <- merge(panel_data, BVX_Monthly_List_of_Crises, by = c("ISO", "year_month"), all.x = TRUE)
MyMonthlyData <- merge(MyMonthlyData, DisasterDF_month, by = c("ISO", "year_month"), all.x = TRUE)
MyMonthlyData <- merge(MyMonthlyData, addstrong_month, by = c("ISO", "year_month"), all.x = TRUE)

MyMonthlyData <- pdata.frame(MyMonthlyData, index = c("ISO", "year_month"), drop.index = FALSE, row.names = TRUE)

MyMonthlyData$DisDummy <- ifelse(MyMonthlyData$NoOfDis >= 1, 1, 0) # everything over 1 is coded as 1
MyMonthlyData$StrongDisDummy <- ifelse(MyMonthlyData$NoOfStrongDis >= 1, 1, 0)

for (varname in c(
  "DisDummy", "StrongDisDummy", "w_adj_DAM_000US", "w_DAM_000US", "w_DEATH", "w_adj_strong_DAM_000US",
  "w_strong_DAM_000US", "w_strong_DEATH", "adj_DAM_000US", "DAM_000US", "DEATH", "adj_strong_DAM_000US",
  "strong_DAM_000US", "Panic"
)) {
  MyMonthlyData[, paste(varname)][is.na(MyMonthlyData[, paste(varname)])] <- 0 # coding NA as 0 damage (since EmDat is a list of observations)
}

MyMonthlyData <- lagvar(adj_strong_DAM_000US, 12, MyMonthlyData)
MyMonthlyData <- lagvar(adj_DAM_000US, 12, MyMonthlyData)
MyMonthlyData <- lagvar(StrongDisDummy, 12, MyMonthlyData)
MyMonthlyData <- lagvar(DisDummy, 12, MyMonthlyData)

MyMonthlyData2 <- filter(MyMonthlyData, Yr > 1950)

test <- lm(Panic ~ adj_strong_DAM_000US + adj_strong_DAM_000US_L1 +
  adj_strong_DAM_000US_L2 + adj_strong_DAM_000US_L3 +
  adj_strong_DAM_000US_L4 + adj_strong_DAM_000US_L5 +
  adj_strong_DAM_000US_L6 + factor(ISO), MyMonthlyData)
summary(test)
test <- lm(Panic ~ DisDummy + DisDummy_L1 +
  DisDummy_L2 + DisDummy_L3 +
  DisDummy_L4 + DisDummy_L5 +
  DisDummy_L6 + factor(ISO), MyMonthlyData)
summary(test)
