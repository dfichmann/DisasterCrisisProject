######## INTRODUCTION ##########
#   to run this script you can change the working directory to this folder:
#setwd("C:\\Users\\settm\\Dropbox\\Disaster_and_Crisis_Project\\DanielsProposal\\") #Set working directory
setwd("~/Library/CloudStorage/Dropbox/MY_files/School/SciencesPo/Semester_4/Thesis/R")######## PACKAGES ###########
# Loading packages the necessary installations are commented out:
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
EmDat$TotalAdjDamage = EmDat$TotalAdjDamage/1000
median(EmDat$TotalAdjDamage, na.rm=TRUE)
#Loading the crisis data: BVX
BVX <- read_dta("data/BVX_annual_regdata.dta") #Crisis data in the folder
BVX <- subset(BVX, year >= 1900) #subsetting data for post 1900 to match with disaster data
BVX$ISO <- BVX$ISO3 #renaming ISO code for merging
BVX$Yr <- ymd(BVX$year, truncated = 2L) #encoding year as a date

year <- 1900:2016
basepanel <-NULL
for (i in c(unique(BVX$ISO))) {
  ISO<-rep(i,each=length(year)) 
  Yr<- year
  new <- cbind(ISO, Yr)
  basepanel <- rbind(basepanel, new)
}
basepanel<- as.data.frame(basepanel)
basepanel$Yr <- ymd(basepanel$Yr, truncated = 2L) #encoding year as a date
BVX <- merge(basepanel, BVX, by=c("ISO","Yr"), all.x = TRUE) #MERGING



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

total_dam_yr <- subset(EmDat, 
                       `Disaster Type` %in% disasterlist  #subsetting to the disaster list
                       ) %>% 
  group_by(Yr, ISO) %>% # grouping over year and country
  summarize(total_dam_yr = sum(TotalAdjDamage)) %>% #number of disasters in each country in each year
  ungroup()

total_dam_yr_strong <- subset(EmDat, 
                       `Disaster Type` %in% disasterlist  #subsetting to the disaster list
                        & TotalAdjDamage > median(EmDat$TotalAdjDamage, na.rm=TRUE) # only above median damage disasters
                       ) %>% 
  group_by(Yr, ISO) %>% # grouping over year and country
  summarize(total_dam_yr_strong = sum(TotalAdjDamage)) %>% #number of disasters in each country in each year
  ungroup()


#binary encoding
count_YearALL$DisDummy <- ifelse(count_YearALL$NoOfDis >= 1, 1,0) #creating 1 0 dummy. 

#same for strong disaster dummy:
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
#adding total damage by year
JstDisaster <- merge(JstDisaster, total_dam_yr, by=c("ISO","Yr"), all.x = TRUE) #MERGING
JstDisaster$total_dam_yr[is.na(JstDisaster$total_dam_yr)] <- 0 #same as above
#adding strong only total damage by year
JstDisaster <- merge(JstDisaster, total_dam_yr_strong, by=c("ISO","Yr"), all.x = TRUE) #MERGING
JstDisaster$total_dam_yr_strong[is.na(JstDisaster$total_dam_yr_strong)] <- 0 #same as above
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

for (i in 1:4) {
  JstDisaster[,paste('total_dam_yr','L',i,sep="")] <- lag(JstDisaster[,paste('total_dam_yr')], i)
}

for (i in 1:4) {
  JstDisaster[,paste('total_dam_yr_strong','L',i,sep="")] <- lag(JstDisaster[,paste('total_dam_yr_strong')], i)
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


#### ADDING BVX Data ####
BvxDisaster <- merge(BVX, count_YearALL, by=c("ISO","Yr"), all.x = TRUE) #MERGING
BvxDisaster$DisDummy[is.na(BvxDisaster$DisDummy)] <- 0 #creating dummy (this is just because all the 0s were n\a but the disaster data coverage is not a problem here)
BvxDisaster$NoOfDis[is.na(BvxDisaster$NoOfDis)] <- 0 #same as above
#adding strong data
BvxDisaster <- merge(BvxDisaster, count_YearAboveM, by=c("ISO","Yr"), all.x = TRUE) #MERGING
BvxDisaster$StrongDisDummy[is.na(BvxDisaster$StrongDisDummy)] <- 0 #creating 0 1 dummy 
BvxDisaster$NoOfStrongDis[is.na(BvxDisaster$NoOfStrongDis)] <- 0 #same as above
#adding total damage data
BvxDisaster <- merge(BvxDisaster, total_dam_yr, by=c("ISO","Yr"), all.x = TRUE) #MERGING
BvxDisaster$total_dam_yr[is.na(BvxDisaster$total_dam_yr)] <- 0 #same as above
#adding strong only total damage by year
BvxDisaster <- merge(BvxDisaster, total_dam_yr_strong, by=c("ISO","Yr"), all.x = TRUE) #MERGING
BvxDisaster$total_dam_yr_strong[is.na(BvxDisaster$total_dam_yr_strong)] <- 0 #same as above
#adding regions and income groups: 
BvxDisaster <- merge(BvxDisaster, countrymeta, by=c("ISO"), all.x = TRUE) #MERGING
#Now encoding it as panel data using pdata.frame:
BvxDisaster <- pdata.frame(BvxDisaster, index=c("ISO","Yr"), drop.index=FALSE, row.names=TRUE)

for (i in 1:4) {
  BvxDisaster[,paste('DisDummy','L',i,sep="")] <- lag(BvxDisaster[,paste('DisDummy')], i)
}
for (i in 1:4) {
  BvxDisaster[,paste('StrongDisDummy','L',i,sep="")] <- lag(BvxDisaster[,paste('StrongDisDummy')], i)
}

for (i in 1:4) {
  BvxDisaster[,paste('total_dam_yr','L',i,sep="")] <- lag(BvxDisaster[,paste('total_dam_yr')], i)
}

for (i in 1:4) {
  BvxDisaster[,paste('total_dam_yr_strong','L',i,sep="")] <- lag(BvxDisaster[,paste('total_dam_yr_strong')], i)
}

#removing three observations for spain to compare with BVX data!!!
comprJstDis <- JstDisaster[!(row.names(JstDisaster) %in% c("ESP-1936-01-01","ESP-1937-01-01","ESP-1938-01-01")),]
#removing some irish observations to compare with JST data!!!
comprBvxDis <- BvxDisaster[!(row.names(BvxDisaster) %in% c("IRL-1900-01-01",
                                                           "IRL-1901-01-01",
                                                           "IRL-1902-01-01",
                                                           "IRL-1903-01-01",
                                                           "IRL-1904-01-01",
                                                           "IRL-1905-01-01",
                                                           "IRL-1906-01-01",
                                                           "IRL-1907-01-01",
                                                           "IRL-1908-01-01",
                                                           "IRL-1909-01-01",
                                                           "IRL-1910-01-01",
                                                           "IRL-1911-01-01",
                                                           "IRL-1912-01-01",
                                                           "IRL-1913-01-01",
                                                           "IRL-1919-01-01"
                                                           )),]


bvxyears <-c(1900:1913,1919:1938,1946:2016)

Jstv1 <- glm(crisisJST ~ DisDummy + DisDummyL1 + DisDummyL2  
                      + factor(ISO), family = binomial(), subset(comprJstDis,year %in% bvxyears))
summary(Jstv1)

Bvxv1 <- glm(RC ~ DisDummy + DisDummyL1 + DisDummyL2  
             + factor(ISO), family = binomial(), subset(comprBvxDis, ISO %in% c(unique(JST$ISO))))
summary(Bvxv1)

Jstv2 <- glm(crisisJST ~ StrongDisDummy +StrongDisDummyL1 +StrongDisDummyL2  
             + factor(ISO), family = binomial(), subset(comprJstDis,year %in% bvxyears))
summary(Jstv2)

Bvxv2 <- glm(RC ~ StrongDisDummy +StrongDisDummyL1 +StrongDisDummyL2  
             + factor(ISO), family = binomial(), subset(comprBvxDis, ISO %in% c(unique(JST$ISO))))
summary(Bvxv2) 

stargazer(Jstv1,
          Bvxv1,
          Jstv2,
          Bvxv2,
          align = TRUE, 
          title = 'Estimated effect of natural disaster on JST vs BVX crisis odds',
          label='bvxVSjst',
          type = 'html', 
          keep=c('DisDummy','DisDummyL1','DisDummyL2',
                 'StrongDisDummy', 'StrongDisDummyL1', 'StrongDisDummyL2'),
          covariate.labels=c("$Disaster_{t}$",
                             "$Disaster_{t-1}$",
                             "$Disaster_{t-2}$",
                             "$StrongDisaster_{t}$",
                             "$StrongDisaster_{t-1}$",
                             "$StrongDisaster_{t-2}$"),
          dep.var.labels = c("JST crisis","BVX crisis","JST crisis","BVX crisis"),
          out = 'tables/BVXvsJSTcrisis.html',
          keep.stat = c("n","adj.rsq"),
          no.space = TRUE, # to remove the spaces after each line of coefficients
          column.sep.width = "2pt", # to reduce column width
          #font.size = "small",
          add.lines = list(c("Country fixed effects", "Yes", "Yes", "Yes", "Yes")),
          notes = "Estimates are coefficients and standard errors are shown in parantheses."
)


###### Damage variable regressions ######
allcrisesLM <- lm(crisisJST ~ total_dam_yr + total_dam_yrL1 + total_dam_yrL2
                      + factor(country), JstDisaster)
summary(allcrisesLM)

Bvxv1 <- glm(RC ~ total_dam_yr + total_dam_yrL1 + total_dam_yrL2
             + factor(ISO), family = binomial(), BvxDisaster)
summary(Bvxv1)

Bvxv1 <- glm(RC ~ StrongDisDummy +StrongDisDummyL1 +StrongDisDummyL2  
             + factor(ISO), family = binomial(), BvxDisaster)
summary(Bvxv1)


allcrisesLOGIT <- glm(crisisJST ~ total_dam_yr + total_dam_yrL1 + total_dam_yrL2
                      + factor(country) + factor(year), family = binomial(), JstDisaster)
summary(allcrisesLOGIT)

Bvxv1 <- glm(RC ~ total_dam_yr + total_dam_yrL1 + total_dam_yrL2
             + factor(ISO) + factor(year), family = binomial(), BvxDisaster)
summary(Bvxv1)

Bvxv1 <- glm(RC ~ total_dam_yr + total_dam_yrL1 + total_dam_yrL2 + total_dam_yrL3
             + factor(ISO) + factor(year), family = binomial(), BvxDisaster)
summary(Bvxv1)

Bvxv1 <- lm(RC ~ total_dam_yr + total_dam_yrL1 + total_dam_yrL2+ total_dam_yrL3
             + factor(ISO) + factor(year), BvxDisaster)
summary(Bvxv1)

Bvxv1 <- glm(RC ~ total_dam_yr + total_dam_yr_sq2
             + total_dam_yrL1 + total_dam_yrL1_sq2
             + total_dam_yrL2 + total_dam_yrL2_sq2
             + factor(ISO) + factor(year), family = binomial(), BvxDisaster)
summary(Bvxv1)

Bvxv1 <- lm(RC ~ total_dam_yr + total_dam_yrL1 + total_dam_yrL2+ total_dam_yrL3
            + factor(ISO) + factor(year), BvxDisaster)
summary(Bvxv1)

Bvxv1 <- glm(RC ~ total_dam_yr, family = binomial(), BvxDisaster)
summary(Bvxv1)
summary(Bvxv1)

plot(Bvxv1)

fit = glm(RC ~ total_dam_yr, family = binomial(), BvxDisaster)
newdat <- data.frame(total_dam_yr=seq(min(BvxDisaster$total_dam_yr), max(BvxDisaster$total_dam_yr),len=1000))
newdat$RC = predict(fit, newdata=newdat, type="response")
plot(RC~total_dam_yr, data=BvxDisaster, col="red4")
lines(RC~total_dam_yr, newdat, col="green4", lwd=2)

BvxDisaster$total_dam_aboveM <- BvxDisaster$total_dam_yr 


####DamageVariableRegressions:####
JstDisaster$total_dam_yr_sq = (JstDisaster$total_dam_yr)^2
JstDisaster$total_dam_yrL1_sq = (JstDisaster$total_dam_yrL1)^2
JstDisaster$total_dam_yrL2_sq = (JstDisaster$total_dam_yrL2)^2
BvxDisaster$total_dam_yr_sq = (BvxDisaster$total_dam_yr)^2
BvxDisaster$total_dam_yrL1_sq = (BvxDisaster$total_dam_yrL1)^2
BvxDisaster$total_dam_yrL2_sq = (BvxDisaster$total_dam_yrL2)^2

T3_1 <- glm(crisisJST ~ total_dam_yr + total_dam_yrL1 + total_dam_yrL2
                  + factor(country),family = binomial(), JstDisaster)
summary(T3_1)

T3_2 <- glm(RC ~ total_dam_yr + total_dam_yrL1 + total_dam_yrL2
             + factor(ISO), family = binomial(), BvxDisaster)
summary(T3_2)

T3_3 <- lm(crisisJST ~ total_dam_yr + total_dam_yr_sq
                      + total_dam_yrL1 + total_dam_yrL1_sq
                      + total_dam_yrL2 + total_dam_yrL2_sq
                      + factor(country), JstDisaster)
summary(T3_3)

T3_4 <- lm(RC ~ 
             total_dam_yr + total_dam_yr_sq +
             total_dam_yrL1 + total_dam_yrL1_sq +
              total_dam_yrL2 + total_dam_yrL2_sq +
            factor(country), BvxDisaster)
summary(T3_4)

stargazer(T3_1,
          T3_2,
          T3_3,
          T3_4,
          align = TRUE, 
          title = 'Estimated effect of natural disaster damages crisis odds',
          label='damages',
          type = 'html', 
          keep=c('total_dam_yr', 'total_dam_yrL1', 'total_dam_yrL2',
                 'total_dam_yr_strong', 'total_dam_yr_strongL1', 'total_dam_yr_strongL2'),
          covariate.labels=c("$DisasterDam_{t}$",
                             "$DisasterDam_{t-1}$",
                             "$DisasterDam_{t-2}$",
                             "$StrongDisasterDam_{t}$",
                             "$StrongDisasterDam_{t-1}$",
                             "$StrongDisasterDam_{t-2}$"),
          dep.var.labels = c("JST crisis","BVX crisis","JST crisis","BVX crisis"),
          out = 'tables/damages.html',
          keep.stat = c("n","adj.rsq"),
          no.space = TRUE, # to remove the spaces after each line of coefficients
          column.sep.width = "2pt", # to reduce column width
          #font.size = "small",
          add.lines = list(c("Country fixed effects", "Yes", "Yes", "Yes", "Yes"),
                           c("Year fixed effects", "No", "No", "No", "no")),
          notes = "Estimates are coefficients and standard errors are shown in parantheses."
)

#BvxDisaster$total_dam_yr[(BvxDisaster$total_dam_yr > median(EmDat$TotalAdjDamage))] <- 0 #same as above

T4_1 <- glm(crisisJST ~ total_dam_yr_strong + total_dam_yr_strongL1 + total_dam_yr_strongL2
            + factor(country),family = binomial(), JstDisaster)
summary(T4_1)

T4_2 <- glm(RC ~ total_dam_yr_strong + total_dam_yr_strongL1 + total_dam_yr_strongL2
            + factor(ISO), family = binomial(), BvxDisaster)
summary(T4_2)

T4_3 <- lm(crisisJST ~ total_dam_yr_strong + total_dam_yr_strongL1 + total_dam_yr_strongL2
            + factor(country)+ factor(year), JstDisaster)
summary(T4_5)

T4_4 <- lm(RC ~ total_dam_yr_strong + total_dam_yr_strongL1 + total_dam_yr_strongL2
            + factor(ISO)+ factor(year), BvxDisaster)
summary(T4_6)

stargazer(T4_1,
          T4_2,
          T4_3,
          T4_4,
          align = TRUE, 
          title = 'Estimated effect of strong natural disaster damages crisis odds',
          label='damages_strong',
          type = 'html', 
          keep=c('total_dam_yr', 'total_dam_yrL1', 'total_dam_yrL2',
                 'total_dam_yr_strong', 'total_dam_yr_strongL1', 'total_dam_yr_strongL2'),
          covariate.labels=c("$DisasterDam_{t}$",
                             "$DisasterDam_{t-1}$",
                             "$DisasterDam_{t-2}$",
                             "$StrongDisasterDam_{t}$",
                             "$StrongDisasterDam_{t-1}$",
                             "$StrongDisasterDam_{t-2}$"),
          dep.var.labels = c("JST crisis","BVX crisis","JST crisis","BVX crisis"),
          out = 'tables/damagesstrong.html',
          keep.stat = c("n","adj.rsq"),
          no.space = TRUE, # to remove the spaces after each line of coefficients
          column.sep.width = "2pt", # to reduce column width
          #font.size = "small",
          add.lines = list(c("Country fixed effects", "Yes", "Yes", "Yes", "Yes"),
                           c("Year fixed effects", "No", "No", "Yes", "Yes")),
          notes = "Estimates are coefficients and standard errors are shown in parantheses."
)

T5_1 <- glm(crisisJST ~ StrongDisDummy +StrongDisDummyL1 +StrongDisDummyL2 
             + factor(ISO), family = binomial(), JstDisaster)
summary(T5_1)

T5_2 <- glm(crisisJST ~ StrongDisDummy +StrongDisDummyL1 +StrongDisDummyL2 + rgdpbarro
            + factor(ISO), family = binomial(), JstDisaster)
summary(T5_2)

T5_3 <- lm(crisisJST ~ total_dam_yr_strong + total_dam_yr_strongL1 + total_dam_yr_strongL2 
           + rgdpbarro + creditL1 + creditL2 + creditL3
           + factor(country)+ factor(year), JstDisaster)
summary(T5_3)


T5_3 <- lm(crisisJST ~ total_dam_yr_strong + total_dam_yr_strongL1 + total_dam_yr_strongL2 
           + rgdpbarro + creditL1 + creditL2 + creditL3 + lev 
           + factor(country)+ factor(year), JstDisaster)
summary(T5_3)

stargazer(T5_1,
          T5_2,
          T5_3,
          align = TRUE, 
          title = 'experimenting with controls',
          label='controls',
          type = 'html', 
          keep=c('DisDummy','DisDummyL1','DisDummyL2',
                 'StrongDisDummy', 'StrongDisDummyL1', 'StrongDisDummyL2',
                 'total_dam_yr', 'total_dam_yrL1', 'total_dam_yrL2',
                 'total_dam_yr_strong', 'total_dam_yr_strongL1', 'total_dam_yr_strongL2',
                 'rgdpbarro' , 'creditL1' , 'creditL2' , 'creditL3' , 'lev'),
          covariate.labels=c("$StrongDisaster_{t}$",
                             "$StrongDisaster_{t-1}$",
                             "$StrongDisaster_{t-2}$",
                             "$StrongDisasterDam_{t}$",
                             "$StrongDisasterDam_{t-1}$",
                             "$StrongDisasterDam_{t-2}$",
                             "GDPpercapita",
                             "creditL1",
                             "creditL2",
                             "creditL3",
                             "banklev"),
          dep.var.labels = c("JST crisis","BVX crisis","JST crisis","BVX crisis"),
          out = 'tables/controls.html',
          keep.stat = c("n","adj.rsq"),
          no.space = TRUE, # to remove the spaces after each line of coefficients
          column.sep.width = "2pt", # to reduce column width
          #font.size = "small",
          add.lines = list(c("Country fixed effects", "Yes", "Yes", "Yes", "Yes"),
                           c("Year fixed effects", "No", "No", "Yes", "Yes")),
          notes = "Estimates are coefficients and standard errors are shown in parantheses."
)




