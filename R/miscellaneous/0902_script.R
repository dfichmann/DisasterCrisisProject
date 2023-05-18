######## INTRODUCTION ##########
#   to run this script you can change the working directory to this folder:
#setwd("C:\\Users\\settm\\Dropbox\\Disaster_and_Crisis_Project\\DanielsProposal\\") #Set working directory
'setwd("")'
setwd("~/Dropbox/MY_files/School/SciencesPo/Semester_4/Thesis/R")
######## PACKAGES ###########
# Then lets load all the libraries. The necessary packages are commented out:
library(tidyverse) 
library("RColorBrewer")
library(ggplot2)
library(readxl)
#install.packages("janitor")
library(janitor)
library(haven)
library(haven)
library(dplyr)
library(tidyr)
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
#install.packages("lubridate")
library(lubridate)
#install.packages("scales")
library(scales)
#install.packages("ggthemes", dependencies = TRUE)
library(ggthemes)
#install.packages("wesanderson")
library(wesanderson)
library(grid)
#install.packages("countrycode")
library(countrycode)
#install.packages("WDI")
library(WDI)
library(dotwhisker)
library(broom)
library(sandwich)
library(lpirfs)
library(tibble)
library(expss)
#install.packages("fixest")
library(fixest)

####### LOADING DATA ##########
#Loading the disaster data: EmDat
EmDat <- read_excel("data/EmDatPublic202211.xlsx") #EmDat data in the folder
EmDat$Yr <- ymd(EmDat$Year, truncated = 2L) #encoding year as a date 
colnames(EmDat)[45] = "TotalAdjDamage"
median(EmDat$TotalAdjDamage, na.rm=TRUE)
#Loading the crisis data: BVX
BVX <- read_dta("data/BVX_annual_regdata.dta") #Crisis data in the folder
BVX <- subset(BVX, year >= 1900) #subsetting data for post 1900 to match with disaster data
BVX$ISO <- BVX$ISO3 #renaming ISO code for merging
BVX$Yr <- ymd(BVX$year, truncated = 2L) #encoding year as a date
#Loading crisis data: JST
JST<-read_dta('https://www.macrohistory.net/app/download/9834512469/JSTdatasetR6.dta?t=1662029183')
JST$jst <- 1 #for subsetting later
JST$ISO <- JST$iso #for merging
JST$Yr <- ymd(JST$year, truncated = 2L) #encoding year as a date
JST <- subset(JST, year >= 1900) #subsetting data for post 1900 to match with disaster data
#Loading regions and income levels:
countrymeta <- read_csv("data/country_group.csv")
countrymeta$ISO <- countrycode(countrymeta$country, origin = 'country.name', 
                               destination = 'iso3c')
countrymeta <- subset(countrymeta, select = c(income_group,region,ISO))

######## MAKING DISASTER DUMMIES ######
#Selecting the list of disasters:
disasterlist = c("Drought", "Earthquake", "Epidemic", 
                 "Extreme temperature", "Flood", 
                 "Storm", "Volcanic activity", "Wildfire")
#Counting the disasters by year
count_YearALL <- subset(EmDat, 
                        `Disaster Type` %in% disasterlist) %>% #subsetting to the disaster list
  group_by(Yr, ISO) %>% # grouping over year and country
  summarize(NoOfDis = n()) %>% #number of disasters in each country in each year
  ungroup()
#binary encoding
count_YearALL$DisDummy <- ifelse(count_YearALL$NoOfDis >= 1, 1,0) #creating 1 0 dummy. 

#same for strong disaster dummy:
colnames(EmDat)[45] = "TotalAdjDamage"
median(EmDat$TotalAdjDamage, na.rm=TRUE)

count_YearAboveM <- subset(EmDat, 
                           `Disaster Type` %in% disasterlist & #subsetting to the disaster list
                             TotalAdjDamage > median(EmDat$TotalAdjDamage, na.rm=TRUE)) %>% # only above median damage disasters
  group_by(Yr, ISO) %>% # grouping over year and country
  summarize(NoOfStrongDis = n()) %>% #number of disasters in each country in each year
  ungroup()

count_YearAboveM$StrongDisDummy <- ifelse(count_YearAboveM$NoOfStrongDis >= 1, 1,0) #creating 1 0 dummy.


########## MERGING DATA ##########
JstDisaster <- merge(JST, count_YearALL, by=c("ISO","Yr"), all.x = TRUE) #MERGING
JstDisaster$DisDummy[is.na(JstDisaster$DisDummy)] <- 0 #creating dummy (this is just because all the 0s were n\a but the disaster data coverage is not a problem here)
JstDisaster$NoOfDis[is.na(JstDisaster$NoOfDis)] <- 0 #same as above
#adding strong data
JstDisaster <- merge(JstDisaster, count_YearAboveM, by=c("ISO","Yr"), all.x = TRUE) #MERGING
JstDisaster$StrongDisDummy[is.na(JstDisaster$StrongDisDummy)] <- 0 #creating 0 1 dummy 
JstDisaster$NoOfStrongDis[is.na(JstDisaster$NoOfStrongDis)] <- 0 #same as above
#adding regions and income groups: 
JstDisaster <- merge(JstDisaster, countrymeta, by=c("ISO"), all.x = TRUE) #MERGING
#Now encoding it as panel data using pdata.frame:
JstDisaster <- pdata.frame(JstDisaster, index=c("ISO","Yr"), drop.index=FALSE, row.names=TRUE)

###### Creating log change real credit ##### 
JstDisaster$rloans<-log(JstDisaster$tloans/JstDisaster$cpi)
for (i in 1:6) {
  JstDisaster[,paste('creditL',i-1,sep="")]<- lag(JstDisaster[,paste('rloans')], i-1)-lag(JstDisaster[,paste('rloans')], i)
}

#Change in log real credit, from t-j-1 to t-j

####### Creating lags for Disaster Dummies #####
for (i in 1:4) {
  JstDisaster[,paste('DisDummy','L',i,sep="")] <- lag(JstDisaster[,paste('DisDummy')], i)
}
for (i in 1:4) {
  JstDisaster[,paste('StrongDisDummy','L',i,sep="")] <- lag(JstDisaster[,paste('StrongDisDummy')], i)
}

######## Printing REGRESSIONS ######
allcrisesOLS <- lm(crisisJST ~ DisDummy + DisDummyL1 + DisDummyL2 + factor(country), JstDisaster)
summary(allcrisesOLS)

allcrisesLOGIT <- glm(crisisJST ~ DisDummy + DisDummyL1 + DisDummyL2  
                         + factor(country), family = binomial(), JstDisaster)
summary(allcrisesLOGIT)

strongcrisesOLS <- lm(crisisJST ~ StrongDisDummy +StrongDisDummyL1 +StrongDisDummyL2
                         + factor(country), JstDisaster) 
summary(strongcrisesOLS)

strongcrisesLOGIT <- glm(crisisJST ~ StrongDisDummy +StrongDisDummyL1 +StrongDisDummyL2
                             + factor(country), family = binomial(), JstDisaster) 
summary(strongcrisesLOGIT)

credit_all <- glm(crisisJST~ DisDummy + DisDummyL1 + DisDummyL2+ 
                    creditL1 + creditL2 + creditL3 + creditL4 + creditL5
                     + factor(country), family = binomial(), JstDisaster)
summary(credit_all)

credit_strong <- glm(crisisJST~ StrongDisDummy + StrongDisDummyL1 + StrongDisDummyL2+ 
                       creditL1 + creditL2 + creditL3 + creditL4 + creditL5
                  + factor(country), family = binomial(), JstDisaster)
summary(credit_strong)


credit <- glm(crisisJST~ creditL1 + creditL2 + creditL3 + creditL4 + creditL5
                     + factor(country), family = binomial(), JstDisaster)
summary(credit)

ex <- JstDisaster$StrongDisDummyL1*JstDisaster$creditL2


stargazer(allcrisesOLS,
          allcrisesLOGIT,
          strongcrisesOLS,
          strongcrisesLOGIT,
          credit_all,
          credit_strong,
          align = TRUE, 
          title = 'Estimated effect of natural disaster on JST crisis odds',
          label='JSTcrisis',
          type = 'html', 
          keep=c('DisDummy','DisDummyL1','DisDummyL2',
                 'StrongDisDummy', 'StrongDisDummyL1', 'StrongDisDummyL2',
                 'creditL1', 'creditL2', 'creditL3', 'creditL4', 'creditL5', 'creditL6'),
          covariate.labels=c("$Disaster_{t}$",
                             "$Disaster_{t-1}$",
                             "$Disaster_{t-2}$",
                             "$StrongDisaster_{t}$",
                             "$StrongDisaster_{t-1}$",
                             "$StrongDisaster_{t-2}$",
                             "$Credit_{t-1}$",
                             "$Credit_{t-2}$",
                             "$Credit_{t-3}$",
                             "$Credit_{t-4}$",
                             "$Credit_{t-5}$",
                             "$Credit_{t-6}$"),
          dep.var.labels = "JST crisis",
          out = 'tables/JSTcrisis.html',
          keep.stat = c("n","adj.rsq"),
          no.space = TRUE, # to remove the spaces after each line of coefficients
          column.sep.width = "2pt", # to reduce column width
          #font.size = "small",
          add.lines = list(c("Country fixed effects", "Yes", "Yes", "Yes", "Yes","Yes","Yes")),
          notes = "Estimates are coefficients and standard errors are shown in parantheses."
)

