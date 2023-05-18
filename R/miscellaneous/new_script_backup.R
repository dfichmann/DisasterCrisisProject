# INTRODUCTION ####
## Set working directory

# to run this script you can change the working directory to this folder: 
setwd("~/Library/CloudStorage/Dropbox/MY_files/School/SciencesPo/Semester_4/Thesis/NaturalDisasters_and_FinancialCrises/R")

## Load Libraries
library(tidyverse) 
library(reshape2)
library(patchwork)
library(ggplot2)
library(readxl)
library(janitor)
library(haven)
library(haven)
library(dplyr)
library(tidyr)
library(panelr)
library(car)
library(stargazer)
library(latex2exp)
library(expss)
library(ggplot2)
library(gridExtra)
library(plm)
library(lmtest)
library(ggpubr)
library(vars)
library(zoo)
library(lubridate)
library(scales)
library(ggthemes)
library(wesanderson)
library(grid)
library(countrycode)
library(WDI)
library(wbstats)
library(dotwhisker)
library(broom)
library(sandwich)
library(lpirfs)
library(tibble)
library(expss)
library(fixest)
library("RColorBrewer")

### Install Packages (if necessary) 

#install.packages("janitor")
#install.packages("panelr")
#install.packages("lubridate")
#install.packages("scales")
#install.packages("ggthemes", dependencies = TRUE)
#install.packages("countrycode")
#install.packages("patchwork")
#install.packages("reshape2")
#install.packages("fixest")
#install.packages("wbstats")
#install.packages("WDI")
#install.packages("wesanderson")

# COMPILING DATA ####

## Disaster Data ####

###### Cleaning
EmDat <- read_excel("data/EmDatPublic202211.xlsx") #EmDat data in the folder
colnames(EmDat)
keep <- c(1,2,3,7,11,12,13,14,29,30,31,35,37,39,44,45,46)
EmDat <- subset(EmDat, select = keep)
colnames(EmDat)[4] = "DisasterType"
colnames(EmDat)[9] = "StartYear"
colnames(EmDat)[10] = "StartMonth"
colnames(EmDat)[11] = "StartDay"
colnames(EmDat)[12] = "TotalDeath"
colnames(EmDat)[13] = "NoAffected"
colnames(EmDat)[14] = "TotalAffected"
colnames(EmDat)[15] = "TotalDamages_000US"
colnames(EmDat)[16] = "TotalDamagesAdj_000US"

EmDat$DateYM <- paste(EmDat$StartYear, EmDat$StartMonth, sep="-")
EmDat$DateYM <- ym(EmDat$DateYM) #encoding date 
EmDat$year_month <- format(EmDat$DateYM, "%Y-%m")
EmDat$Yr <- ymd(EmDat$Year, truncated = 2L) #encoding year as a date 

### Adding cpi to convert real gdp things later
CpiConvert <- subset(EmDat, select = c("ISO", "Year", "CPI"))
CpiConvert <- subset(CpiConvert, `Year` %in% 1990)
CpiConvert$CPI1990to2021 <- CpiConvert$CPI
CpiConvert <- subset(CpiConvert, select= c("CPI1990to2021", "Year", "ISO")) 

### Adding weights:
mWeight <- (12 - EmDat$StartMonth)/12 #weight formula
for (m in c("TotalDeath", "TotalDamages_000US", "TotalDamagesAdj_000US")) {
  EmDat[,paste('weighted',m,sep="_")] <- EmDat[,paste(m)] * mWeight
}

disasterlist = c("Drought", "Earthquake", "Epidemic", 
                 "Extreme temperature", "Flood", 
                 "Storm", "Volcanic activity", "Wildfire")

### Collapsing / Summarizing

DisasterDF <- subset(EmDat,`DisasterType` %in% disasterlist) %>% 
  group_by(Yr,ISO) %>%  # grouping over year and country
  summarize(adj_DAM_000US = sum(TotalDamagesAdj_000US),
            DAM_000US = sum(TotalDamages_000US),
            DEATH = sum(TotalDeath),
            w_adj_DAM_000US = sum(weighted_TotalDamagesAdj_000US),
            w_DAM_000US = sum(weighted_TotalDamages_000US),
            w_DEATH = sum(weighted_TotalDeath),
            NoOfDis = n(),
            .groups ='drop')

addstrong <- subset(EmDat,`DisasterType` %in% disasterlist
                    & TotalDamagesAdj_000US > median(EmDat$TotalDamagesAdj_000US, na.rm=TRUE)) %>%  #only abovemedian disasters
  group_by(Yr, ISO) %>%
  summarize(adj_strong_DAM_000US = sum(TotalDamagesAdj_000US),
            strong_DAM_000US = sum(TotalDamages_000US),
            strong_DEATH = sum(TotalDeath),
            w_adj_strong_DAM_000US = sum(weighted_TotalDamagesAdj_000US),
            w_strong_DAM_000US = sum(weighted_TotalDamages_000US),
            w_strong_DEATH = sum(weighted_TotalDeath),
            NoOfStrongDis = n(),
            .groups ='drop')

### Base Panel Data Set (Empty)

# Making it into Panel Data by merging the disaster data onto a base panel
countrylist <- c("ARG", "AUS", "AUT", "BEL", "BRA", "CAN", "CHL", "TWN", "COL", "CZE", "DNK", "FIN", "FRA",
                 "DEU", "GRC", "HKG", "HUN", "ISL", "IDN", "IRL", "ISR", "ITA", "JPN", "KOR", "LUX", "MYS",
                 "MEX", "NLD", "NZL", "NOR", "PER", "PHL", "PRT", "RUS", "IND", "SGP", "ZAF", "ESP", "SWE",
                 "CHE", "THA", "TUR", "EGY", "GBR", "USA", "VEN")
year <- 1900:2020
basepanel <-NULL
for (i in countrylist) {
  ISO<-rep(i,each=length(year)) 
  Yr<- year
  new <- cbind(ISO, Yr)
  basepanel <- rbind(basepanel, new)
}
basepanel<- as.data.frame(basepanel)
basepanel$Yr <- ymd(basepanel$Yr, truncated = 2L)

### Merging Disaster Data onto it
MyData <- merge(basepanel, DisasterDF, by=c("ISO","Yr"), all.x = TRUE)
MyData <- merge(MyData, addstrong, by=c("ISO","Yr"), all.x = TRUE) #adding strong disaster variables

### Making indicators:
MyData$DisDummy <- ifelse(MyData$NoOfDis >= 1, 1,0) #everything over 1 is coded as 1 
MyData$StrongDisDummy <- ifelse(MyData$NoOfStrongDis >= 1, 1,0) 

for (varname in c('DisDummy', 'StrongDisDummy', 'w_adj_DAM_000US', 'w_DAM_000US', 'w_DEATH', 'w_adj_strong_DAM_000US',
                  'w_strong_DAM_000US', 'w_strong_DEATH', 'adj_DAM_000US', 'DAM_000US', 'DEATH', 'adj_strong_DAM_000US', 
                  'strong_DAM_000US', 'strong_DEATH')) {
  MyData[,paste(varname)][is.na(MyData[,paste(varname)])] <- 0 #coding NA as 0 damage (since EmDat is a list of observations)
}

## JST data ##### 

#uncomment the next line to download the data instead: 
#JST<-read_dta('https://www.macrohistory.net/app/download/9834512469/JSTdatasetR6.dta?t=1662029183')
JST<-read_dta('data/JSTdatasetR6.dta')
JST$jst <- 1 #for subsetting later
JST$ISO <- JST$iso #for merging
JST$Yr <- ymd(JST$year, truncated = 2L)

colnames(JST)
keep <- c(1,2,3,5,6,7,9,11,13,14,17,20,21,22,23,24,25,26,31,34,46,47,50,55,56,58,59,60,61,62)
JST <- subset(JST, select = keep)
MyData <- merge(MyData, JST, by=c("ISO","Yr"), all.x = TRUE)

## BVX data  ##### 

#Loading the crisis data: BVX
BVX <- read_dta("data/BVX_annual_regdata.dta") #Crisis data in the folder
BVX$ISO <- BVX$ISO3 #renaming ISO code for merging
BVX$Yr <- ymd(BVX$year, truncated = 2L) #encoding year as a date
colnames(BVX)
keep <- c("Yr", "ISO", "decade","rgdp_gr","credit_to_gdp","jointcrisis","JC","revisedcrisis","RC","PANIC_ind","bankeqdecline","C_B30")
BVX <- subset(BVX, select = keep)
BVX$bvx <- 1 #for subsetting later
MyData <- merge(MyData, BVX, by=c("ISO","Yr"), all.x = TRUE)

## GDP data from World Bank API ####

gdp_data <- wb(indicator = "NY.GDP.MKTP.CD", country = unique(BVX$ISO), startdate = 1960, enddate = 2020)
# select only the relevant columns
gdp_data <- subset(gdp_data, select = c("iso3c", "date", "value"))
# rename columns
colnames(gdp_data) <- c("ISO", "Yr", "GDP_nominal")
gdp_data$GDP_nominal_000US <- gdp_data$GDP_nominal / 1000
gdp_data <- subset(gdp_data, select = c("ISO", "Yr", "GDP_nominal_000US"))
# convert Year to date format
gdp_data$Yr <- ymd(gdp_data$Yr, truncated = 2L) #encoding year as a date
# Merge
MyData <- merge(MyData, gdp_data, by=c("ISO","Yr"), all.x = TRUE)

MyData <- pdata.frame(MyData, index=c("ISO","Yr"), drop.index=FALSE, row.names=TRUE)


# ANALYSIS  ####
lagvar = function(varname, lags, dataset) {
  x <- paste(deparse(substitute(varname)))
  for (i in 1:lags) {
    dataset[[paste(x,'_L',i,sep="")]] <- lag(dataset[[x]],i)
  }
  return(dataset)
}

leadvar <- function(varname, leads, dataset) {
  x <- paste(deparse(substitute(varname)))
  for (i in 1:leads) {
    dataset[[paste(x, '_F', i, sep="")]] <- lead(dataset[[x]], i)
  }
  return(dataset)
}

#for example: 
#   b <- lagvar(DisDummy, 4, MyData)

## Standardize ####
MyData$rGDP <- MyData$rgdpmad * MyData$pop  #/ 0.48 (this is the less good way of re-basing the gdp data)
#adjusting damage data for inflation base 1990, dividing by RealGdp 1990
MyData$stnDAM_strong <- ((MyData$w_strong_DAM_000US /(100/MyData$cpi)) / MyData$rGDP)*100 
MyData$stnDAM <- ((MyData$w_DAM_000US /(100/MyData$cpi)) / MyData$rGDP)*100 

MyData$stnDAM_strong_uw <- ((MyData$strong_DAM_000US /(100/MyData$cpi)) / MyData$rGDP)*100 

#stnGDP = function(varname,dataset) {
#  x <- paste(deparse(substitute(varname)))
#    dataset[[paste(x,'_stnize',sep="")]] <- (dataset[[paste(x)]] / dataset[[paste('rGDP')]])
#  return(dataset)
#}

## ESTIMATES #######

#using standardized strong disaster variables
MyData <- lagvar(stnDAM_strong, 4, MyData)

M1_glm_fe_stn_strong <- glm(crisisJST ~ stnDAM_strong +stnDAM_strong_L1 + stnDAM_strong_L2 
            + factor(ISO), family = binomial(), MyData)
summary(M1_glm_fe_stn_strong)

M1_glm_fe_stn_strong2 <- glm(crisisJST ~ stnDAM_strong +stnDAM_strong_L1 + stnDAM_strong_L2 
                             + factor(ISO), family = binomial(), filter(MyData, year >= 1950))
summary(M1_glm_fe_stn_strong2)

M2_ols_fe_stn_strong <- lm(crisisJST ~ stnDAM_strong +stnDAM_strong_L1 + stnDAM_strong_L2 
           + factor(ISO) + factor(year), MyData)
summary(M2_ols_fe_stn_strong)

M2_ols_fe_stn_strong2 <- lm(crisisJST ~ stnDAM_strong +stnDAM_strong_L1 + stnDAM_strong_L2 
                           + factor(ISO) + factor(year), filter(MyData, year >= 1950))
summary(M2_ols_fe_stn_strong2)

#using a danger zone:
MyData$crisisJST1 <- lead(MyData$crisisJST,1)
MyData$crisisJST2 <- lead(MyData$crisisJST,2)
MyData$crisisJST3 <- lead(MyData$crisisJST,3)

MyData$dangerzoneJST <- MyData$crisisJST + MyData$crisisJST1 + MyData$crisisJST2 

M3_glm_danger <- glm(dangerzoneJST ~ stnDAM_strong +stnDAM_strong_L1 + stnDAM_strong_L2 
            + factor(ISO), family = binomial(), MyData)
summary(M3_glm_danger)

M3_glm_danger2 <- glm(dangerzoneJST ~ stnDAM_strong +stnDAM_strong_L1 + stnDAM_strong_L2 
                     + factor(ISO), family = binomial(), filter(MyData, year >= 1950))
summary(M3_glm_danger2)

M4_glm_danger <- glm(dangerzoneJST ~ stnDAM_strong_L1 + stnDAM_strong_L2 
            + factor(ISO), family = binomial(), MyData)
summary(M4_glm_danger)

M5_ols_danger <- lm(dangerzoneJST ~ stnDAM_strong +stnDAM_strong_L1 + stnDAM_strong_L2 
           + factor(ISO) + factor(year), MyData)
summary(M5_ols_danger)

M5_ols_danger2 <- lm(dangerzoneJST ~ stnDAM_strong +stnDAM_strong_L1 + stnDAM_strong_L2 
                    + factor(ISO) + factor(year), filter(MyData, year >= 1950))
summary(M5_ols_danger2)

stargazer(M1_glm_fe_stn_strong,
          M3_glm_danger,
          align = TRUE, 
          title = 'Logit estimates of effect of a strong natural disaster on crisis odds (1900 - 2020)',
          label='T1',
          type = 'html', 
          keep=c('stnDAM_strong', 'stnDAM_strong_L1', 'stnDAM_strong_L2'),
          covariate.labels=c("$Damages (pct GDP)_{t}$",
                             "$Damages (pct GDP)_{t-1}$",
                             "$Damages (pct GDP)_{t-2}$"),
          dep.var.labels = c("Crisis in $year = t$","Crisis Danger Zone"),
          out = 'tables/T1.html',
          font.size = "small",
          add.lines = list(c("Country fixed effects","Yes","Yes"),
                           c("Year fixed effects", "No", "No")),
          notes = "Estimates are coefficients and standard errors are shown in parantheses."
)

stargazer(M1_glm_fe_stn_strong2,
          M3_glm_danger2,
          align = TRUE, 
          title = 'Logit estimates of effect of a strong natural disaster on crisis odds (1950 - 2020)',
          label='T1_1',
          type = 'html', 
          keep=c('stnDAM_strong', 'stnDAM_strong_L1', 'stnDAM_strong_L2'),
          covariate.labels=c("$Damages (pct GDP)_{t}$",
                             "$Damages (pct GDP)_{t-1}$",
                             "$Damages (pct GDP)_{t-2}$"),
          dep.var.labels = c("Crisis in $year = t$, Crisis Danger Zone"),
          out = 'tables/T1_1.html',
          font.size = "small",
          add.lines = list(c("Country fixed effects","Yes","Yes"),
                           c("Year fixed effects", "No", "No")),
          notes = "Estimates are coefficients and standard errors are shown in parantheses."
)

stargazer(M2_ols_fe_stn_strong,
          M5_ols_danger,
          align = TRUE, 
          title = 'OLS estimates of effect of a strong natural disaster on crisis odds (1900 - 2020)',
          label='T2',
          type = 'html', 
          keep=c('stnDAM_strong', 'stnDAM_strong_L1', 'stnDAM_strong_L2'),
          covariate.labels=c("$Damages (pct GDP)_{t}$",
                             "$Damages (pct GDP)_{t-1}$",
                             "$Damages (pct GDP)_{t-2}$"),
          dep.var.labels = c("Crisis in $year = t$", "Crisis Danger Zone"),
          out = 'tables/T2.html',
          omit.stat = "aic",
          no.space = TRUE, # to remove the spaces after each line of coefficients
          column.sep.width = "2pt", # to reduce column width
          font.size = "small",
          add.lines = list(c("Country fixed effects","Yes","Yes"),
                           c("Year fixed effects", "Yes", "Yes")),
          notes = "Estimates are coefficients and standard errors are shown in parantheses."
)
stargazer(M2_ols_fe_stn_strong2,
          M5_ols_danger2,
          align = TRUE, 
          title = 'OLS estimates of effect of a strong natural disaster on crisis odds (1950 - 2020)',
          label='T2_2',
          type = 'html', 
          keep=c('stnDAM_strong', 'stnDAM_strong_L1', 'stnDAM_strong_L2'),
          covariate.labels=c("$Damages (pct GDP)_{t}$",
                             "$Damages (pct GDP)_{t-1}$",
                             "$Damages (pct GDP)_{t-2}$"),
          dep.var.labels = c("Crisis in $year = t$", "Crisis Danger Zone"),
          out = 'tables/T2_2.html',
          omit.stat = "aic",
          no.space = TRUE, # to remove the spaces after each line of coefficients
          column.sep.width = "2pt", # to reduce column width
          font.size = "small",
          add.lines = list(c("Country fixed effects","Yes","Yes"),
                           c("Year fixed effects", "Yes", "Yes")),
          notes = "Estimates are coefficients and standard errors are shown in parantheses."
)

#using standardized disaster variables (don't use)
MyData <- lagvar(stnDAM, 4, MyData)

robust_test <- glm(crisisJST ~ stnDAM + stnDAM_L1 + stnDAM_L2 
                            + factor(ISO), family = binomial(), MyData)
summary(robust_test)

robust_test <- lm(crisisJST ~ stnDAM_strong +stnDAM_strong_L1 + stnDAM_strong_L2 
                           + factor(ISO) + factor(year), MyData)
summary(robust_test)

robust_test <- glm(dangerzoneJST ~ stnDAM + stnDAM_L1 + stnDAM_L2 
                   + factor(ISO), family = binomial(), MyData)
summary(robust_test)

MyData <- lagvar(stnDAM_strong_uw, 4, MyData)

robust_test <- glm(crisisJST ~ stnDAM_strong_uw +stnDAM_strong_uw_L1 + stnDAM_strong_uw_L2 
              + factor(ISO), family = binomial(), MyData)
summary(robust_test)

##### testing things out ##########
a<-stnGDP(w_adj_Dam_000US, MyData)



MyData <- lagvar(stnDAM_strong, 4, MyData)

test <- glm(crisisJST ~ stnDAM_strong +stnDAM_strong_L1 + stnDAM_strong_L2 
            + factor(ISO), family = binomial(), MyData)
summary(test)

test <- lm(crisisJST ~ stnDAM_strong +stnDAM_strong_L1 + stnDAM_strong_L2 
            + factor(ISO) + factor(year), MyData)
summary(test)

MyData <- lagvar(w_adj_strong_DAM_000US, 4, MyData)

test <- glm(crisisJST ~ w_adj_strong_DAM_000US +w_adj_strong_DAM_000US_L1  + w_adj_strong_DAM_000US_L2
            + factor(ISO), family = binomial(), MyData)
summary(test)

test <- glm(crisisJST ~ adj_strong_DAM_000US + adj_strong_DAM_000US_L1  + w_adj_strong_DAM_000US_L2
            + factor(ISO), family = binomial(), MyData)
summary(test)


MyData$stnDAM_strong2 <- MyData$adj_strong_DAM_000US / MyData$rGDP

MyData <- lagvar(stnDAM_strong2, 4, MyData)

test <- glm(crisisJST ~ stnDAM_strong2 +stnDAM_strong2_L1 + stnDAM_strong2_L2 
            + factor(ISO), family = binomial(), MyData)
summary(test)

dangerzone

MyData$crisisJST1 <- lead(MyData$crisisJST,1)
MyData$crisisJST2 <- lead(MyData$crisisJST,2)
MyData$crisisJST3 <- lead(MyData$crisisJST,3)

MyData$dangerzoneJST <- MyData$crisisJST + MyData$crisisJST1 + MyData$crisisJST2 

test <- glm(dangerzoneJST ~ stnDAM_strong +stnDAM_strong_L1 + stnDAM_strong_L2 
            + factor(ISO), family = binomial(), MyData)
summary(test)

test <- glm(dangerzoneJST ~ stnDAM_strong_L1 + stnDAM_strong_L2 
            + factor(ISO), family = binomial(), MyData)
summary(test)


test <- lm(dangerzoneJST ~ stnDAM_strong +stnDAM_strong_L1 + stnDAM_strong_L2 
            + factor(ISO) + factor(year), MyData)
summary(test)

test <- glm(dangerzoneJST ~ w_adj_strong_DAM_000US +w_adj_strong_DAM_000US_L1  + w_adj_strong_DAM_000US_L2 
            + factor(ISO) + factor(year), family = binomial(), MyData)
summary(test)

test <- glm(dangerzoneJST ~ w_adj_strong_DAM_000US 
            + factor(ISO) + factor(year), family = binomial(), MyData)
summary(test)

test <- glm(dangerzoneJST ~ w_strong_DEATH + w_strong_DEATH_L1 + w_strong_DEATH_L2
            + factor(ISO) + factor(year), family = binomial(), MyData)
summary(test)

MyData <- lagvar(w_strong_DEATH, 4, MyData)

M1_glm_fe_stn_strong2 <- glm(crisisJST ~ stnDAM_strong +stnDAM_strong_L1 + stnDAM_strong_L2 
                            + factor(ISO), family = binomial(), subset(MyData, `year` > 1949))
summary(M1_glm_fe_stn_strong2)



###random code to perhaps re-use later:####
for (varname in c('DisDummy', 'StrongDisDummy', 'adj_DAM_000US', 'adj_strong_DAM_000US', 'w_adj_DAM_000US')) {
  for (i in 1:4) {
    MyData[,paste(varname,'_L',i,sep="")] <- lag(MyData[,paste(varname)], i)
  }
}

####### PLOTS PLOTS PLOTS ############

plots_dir <- "plots/countryplots/" 
dir.create(plots_dir, showWarnings = FALSE)

for (country_name in unique(MyData$ISO)) {
  country_data <- grouped_data %>% 
    filter(ISO == country_name)
  
  plot <- ggplot(country_data, aes(x = year, y = stnDAM_strong)) +
    geom_line() +
    geom_vline(aes(xintercept = year), data = filter(country_data, crisisJST == 1),
               color = "red", linetype = "dashed") +
    labs(title = paste(country_name, "Yearly Damage", sep = " "),
         x = "Year",
         y = "Yearly Damage") +
    theme_bw()
  
  ggsave(paste0(plots_dir, country_name, "_damage.png"), plot)
  
}

####PANEL COUNTRY PLOTS ####
plots_dir <- "plots/" 
dir.create(plots_dir, showWarnings = FALSE)
# Filter data for selected countries and years
selected_countries <- c("AUS", "BEL", "CHE", "FRA", "ITA", "NLD", "USA", "JPN")  # Change this to select your countries

# Group data by country and year
grouped_data <- filter(MyData, ISO %in% selected_countries, year >= 1950) %>% 
  group_by(ISO, year)

# Create a line plot for each country and year
plot_list <- list()

for (country_name in selected_countries) {
  country_data <- grouped_data %>% 
    filter(ISO == country_name)
  
  plot3 <- ggplot(country_data, aes(x = year, y = stnDAM_strong)) +
    geom_line() +
    geom_vline(aes(xintercept = year), data = filter(country_data, crisisJST == 1),
               color = "red", 
               linetype = "dashed", 
               size = 0.8) +
    #geom_vline(aes(xintercept = year), data = filter(country_data, RC == 1),
    #           color = "blue", 
    #           linetype = "dotted", 
    #           size = 0.5) +
    geom_rect(aes(xmin = year - 0.5, xmax = year + 0.5, ymin = -Inf, ymax = Inf), data = filter(country_data, dangerzoneJST == 1),
              fill = "red", alpha = 0.1) +
    labs(title = country_name,
         x = "Year",
         y = "Damage (%GDP)") +
    theme_bw()
  
  plot_list[[country_name]] <- plot3
}

panel_plot <- wrap_plots(plot_list, ncol = 2)

# Save panel graph as a file
ggsave(paste0(plots_dir, "panel_damage_crisis.png"), width = 12, height = 10)





######## test with BVX and world bank data #######

MyData$stnDAM_strong2 <- (MyData$w_DAM_000US / MyData$GDP_nominal_000US)*100 

MyData <- lagvar(stnDAM_strong2, 4, MyData)

MyData2 <- filter(MyData, year > 1960)

test5 <- glm(RC ~ stnDAM_strong2 +stnDAM_strong2_L1 + stnDAM_strong2_L2 
                            + factor(ISO), family = binomial(), MyData2)
summary(test5)

MyData <- lagvar(strong_DAM_000US, 4, MyData)
test5 <- glm(RC ~ strong_DAM_000US +strong_DAM_000US_L1 + strong_DAM_000US_L2 
             + factor(ISO), family = binomial(), MyData)
summary(test5)

#using a danger zone:
MyData$RC1 <- lead(MyData$RC,1)
MyData$RC2 <- lead(MyData$RC,2)
MyData$RC3 <- lead(MyData$RC,3)

MyData$dangerzoneRC <- MyData$RC + MyData$RC1 + MyData$RC2 
MyData$dangerzoneRC <- ifelse(MyData$dangerzoneRC >= 1, 1,0) 

test5 <- glm(dangerzoneRC ~ stnDAM_strong2 +stnDAM_strong2_L1 + stnDAM_strong2_L2 
             + factor(ISO), family = binomial(), MyData)
summary(test5)

plots_dir <- "plots/countryplots_bvx/" 
dir.create(plots_dir, showWarnings = FALSE)

# Group data by country and year
grouped_data <- filter(MyData, year > 1960) %>% 
  group_by(ISO, year)

for (country_name in unique(MyData$ISO)) {
  country_data <- grouped_data %>% 
    filter(ISO == country_name)
  
  plot <- ggplot(country_data, aes(x = year, y = stnDAM_strong2)) +
    geom_line() +
    geom_vline(aes(xintercept = year), data = filter(country_data, RC == 1),
               color = "red", linetype = "dashed") +
    labs(title = paste(country_name, "Yearly Damage", sep = " "),
         x = "Year",
         y = "Yearly Damage") +
    theme_bw()
  
  ggsave(paste0(plots_dir, country_name, "_damage.png"), plot)
  
  MyData <- lagvar(StrongDisDummy, 3, MyData)
  test5 <- glm(dangerzoneRC ~ StrongDisDummy +StrongDisDummy_L1 + StrongDisDummy_L2 
               + factor(ISO), family = binomial(), MyData)
  summary(test5)
  
}


########## Monthly test? ###########
#Loading the crisis data: BVX

BVXmonthly <- read_dta("data/master_data_monthly.dta") #Crisis data in the folder
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

DisasterDF_month <- subset(EmDat,`DisasterType` %in% disasterlist) %>% 
  group_by(year_month,ISO) %>%  # grouping over year and country
  summarize(adj_DAM_000US = sum(TotalDamagesAdj_000US),
            DAM_000US = sum(TotalDamages_000US),
            DEATH = sum(TotalDeath),
            w_adj_DAM_000US = sum(weighted_TotalDamagesAdj_000US),
            w_DAM_000US = sum(weighted_TotalDamages_000US),
            w_DEATH = sum(weighted_TotalDeath),
            NoOfDis = n(),
            .groups ='drop')

addstrong_month <- subset(EmDat,`DisasterType` %in% disasterlist
                    & TotalDamagesAdj_000US > median(EmDat$TotalDamagesAdj_000US, na.rm=TRUE)) %>%  #only abovemedian disasters
  group_by(year_month, ISO) %>%
  summarize(adj_strong_DAM_000US = sum(TotalDamagesAdj_000US),
            strong_DAM_000US = sum(TotalDamages_000US),
            strong_DEATH = sum(TotalDeath),
            w_adj_strong_DAM_000US = sum(weighted_TotalDamagesAdj_000US),
            w_strong_DAM_000US = sum(weighted_TotalDamages_000US),
            w_strong_DEATH = sum(weighted_TotalDeath),
            NoOfStrongDis = n(),
            .groups ='drop')

MyMonthlyData <- merge(MyMonthlyData, DisasterDF_month, by = c("ISO", "year_month"), all.x = TRUE)
MyMonthlyData <- merge(MyMonthlyData, addstrong_month, by = c("ISO", "year_month"), all.x = TRUE)
MyMonthlyData <- pdata.frame(MyMonthlyData, index=c("ISO","year_month"), drop.index=FALSE, row.names=TRUE)

MyMonthlyData$DisDummy <- ifelse(MyMonthlyData$NoOfDis >= 1, 1,0) #everything over 1 is coded as 1 
MyMonthlyData$StrongDisDummy <- ifelse(MyMonthlyData$NoOfStrongDis >= 1, 1,0) 

for (varname in c('DisDummy', 'StrongDisDummy', 'w_adj_DAM_000US', 'w_DAM_000US', 'w_DEATH', 'w_adj_strong_DAM_000US',
                  'w_strong_DAM_000US', 'w_strong_DEATH', 'adj_DAM_000US', 'DAM_000US', 'DEATH', 'adj_strong_DAM_000US', 
                  'strong_DAM_000US', 'strong_DEATH')) {
  MyMonthlyData[,paste(varname)][is.na(MyMonthlyData[,paste(varname)])] <- 0 #coding NA as 0 damage (since EmDat is a list of observations)
}

MyMonthlyData <- lagvar(adj_strong_DAM_000US, 4, MyMonthlyData)
MyMonthlyData <- lagvar(adj_DAM_000US, 4, MyMonthlyData)

MyMonthlyData2 <- filter(MyMonthlyData, Yr > 1950)
test <- lm(rB ~ adj_DAM_000US +adj_DAM_000US_L1 +
             adj_DAM_000US_L2 + adj_DAM_000US_L3 +
             factor(ISO), MyMonthlyData2)
summary(test)

MyMonthlyData$DisDummy <- ifelse(MyMonthlyData$NoOfDis >= 1, 1,0) #everything over 1 is coded as 1 
MyMonthlyData <- lagvar(StrongDisDummy, 4, MyMonthlyData)
test <- lm(sC ~ StrongDisDummy +StrongDisDummy_L1 +
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
  vars[[i+1]] <- MyMonthlyData[[paste0("sC_F", i)]]
}

for (i in 1:12) {
  MyMonthlyData <- MyMonthlyData %>%
    arrange(ISO, year_month) %>%
    group_by(ISO) %>%
    mutate(!!paste0("rB_F", i) := dplyr::lead(rB, i))
}
vars <- list(MyMonthlyData$rB)
for (i in 1:12) {
  vars2[[i+1]] <- MyMonthlyData[[paste0("rB_F", i)]]
}

for (i in 1:12) {
  MyMonthlyData <- MyMonthlyData %>%
    arrange(ISO, year_month) %>%
    group_by(ISO) %>%
    mutate(!!paste0("sB_F", i) := dplyr::lead(sB, i))
}
vars3 <- list(MyMonthlyData$sB)
for (i in 1:12) {
  vars3[[i+1]] <- MyMonthlyData[[paste0("sB_F", i)]]
}

for (i in 1:12) {
  MyMonthlyData <- MyMonthlyData %>%
    arrange(ISO, year_month) %>%
    group_by(ISO) %>%
    mutate(!!paste0("rN_F", i) := dplyr::lead(rN, i))
}
vars4 <- list(MyMonthlyData$rN)
for (i in 1:12) {
  vars4[[i+1]] <- MyMonthlyData[[paste0("rN_F", i)]]
}

#List to store regressions
reglist<-list()
for(i in 1:length(vars2)){
  x<-vars2[[i]]
  reglist[[i]]<- lm(x ~ StrongDisDummy
                    +factor(ISO),
                    MyMonthlyData)
  reglist[[i]]<-coeftest(reglist[[i]],vcov=vcovHC(reglist[[i]],cluster="ISO",type="HC2"))
  model<-tidy(reglist[[i]], conf.int = TRUE, conf.level = 0.9)%>%
    filter(term=='StrongDisDummy')%>%
    mutate(model=paste('h=',i,sep=''))
  if(i==1){
    regdf<-model
  }else {
    regdf<-rbind(regdf,model)
  }
}

regdf<-rbind(tibble(term = "StrongDisDummy", 
                    estimate = 0, 
                    std.error = 0, 
                    statistic = 0,
                    p.value = 0,
                    conf.low = 0,
                    conf.high = 0,
                    model = "h=0"),regdf)
regdf$lp = 1

pd <- position_dodge(0.1)
gdplp<- ggplot(regdf, aes(model,estimate, group=lp)) +
  geom_point() +
  geom_line(linetype = "dashed",color="blue", size=.8)+
  theme_bw() +
  geom_hline(yintercept=0) + 
  scale_y_continuous("log change in real GDP") +
  xlab("Years after disaster") +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.1, position=pd)

gdplp


######### Monthly crises data #########

BVX_Monthly_List_of_Crises <- read_excel("data/BVX_Monthly_List_of_Crises.xlsx")
BVX_Monthly_List_of_Crises$year_month <- format(parse_date_time(BVX_Monthly_List_of_Crises$Panic_date, orders = "%B %Y"), "%Y-%m")
BVX_Monthly_List_of_Crises$ISO <- countrycode(BVX_Monthly_List_of_Crises$Country, "country.name", "iso3c")

MyMonthlyData <- merge(panel_data, BVX_Monthly_List_of_Crises, by = c("ISO", "year_month"), all.x = TRUE)
MyMonthlyData <- merge(MyMonthlyData, DisasterDF_month, by = c("ISO", "year_month"), all.x = TRUE)
MyMonthlyData <- merge(MyMonthlyData, addstrong_month, by = c("ISO", "year_month"), all.x = TRUE)

MyMonthlyData <- pdata.frame(MyMonthlyData, index=c("ISO","year_month"), drop.index=FALSE, row.names=TRUE)

MyMonthlyData$DisDummy <- ifelse(MyMonthlyData$NoOfDis >= 1, 1,0) #everything over 1 is coded as 1 
MyMonthlyData$StrongDisDummy <- ifelse(MyMonthlyData$NoOfStrongDis >= 1, 1,0) 

for (varname in c('DisDummy', 'StrongDisDummy', 'w_adj_DAM_000US', 'w_DAM_000US', 'w_DEATH', 'w_adj_strong_DAM_000US',
                  'w_strong_DAM_000US', 'w_strong_DEATH', 'adj_DAM_000US', 'DAM_000US', 'DEATH', 'adj_strong_DAM_000US', 
                  'strong_DAM_000US', 'Panic')) {
  MyMonthlyData[,paste(varname)][is.na(MyMonthlyData[,paste(varname)])] <- 0 #coding NA as 0 damage (since EmDat is a list of observations)
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
