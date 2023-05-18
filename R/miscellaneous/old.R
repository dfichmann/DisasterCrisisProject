# Main Script

######## INTRODUCTION ##########
#   to run this script you can change the working directory to this folder:
setwd("C:\\Users\\settm\\Dropbox\\Disaster_and_Crisis_Project\\DanielsProposal\\") #Set working directory
'setwd("")'

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

######## LOADING DATA ##########
#Loading the disaster data: EmDat
EmDat <- read_excel("data/EmDatPublic202211.xlsx") #EmDat data in the folder
EmDat$Yr <- ymd(EmDat$Year, truncated = 2L) #encoding year as a date 
colnames(EmDat)[45] = "TotalAdjDamage"
median(EmDat$TotalAdjDamage, na.rm=TRUE)
#Loading the crisis data: BVX
BVX <- read_dta("data/BVX_annual_regdata.dta") #Crisis data in the folder
BVX <- subset(BVX, year >= 1900) #subsetting data for post 1900 to match with disaster data
BVX$ISO <- BVX$ISO3 #renaming ISO code for mergin
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

#Seecting the list of disasters i want to look at
disasterlist = c("Drought", "Earthquake", "Epidemic", 
                 "Extreme temperature", "Flood", 
                 "Storm", "Volcanic activity", "Wildfire")
#Creating the dummy
count_YearALL <- subset(EmDat, 
                        `Disaster Type` %in% disasterlist) %>% #subsetting to the disaster list
  group_by(Yr, ISO) %>% # grouping over year and country
  summarize(NoOfDis = n()) %>% #number of disasters in each country in each year
  ungroup()

count_YearALL$DisDummy <- ifelse(count_YearALL$NoOfDis >= 1, 1,0) #creating 1 0 dummy. 

#Creating strong disaster dummy:
colnames(EmDat)[45] = "TotalAdjDamage"
median(EmDat$TotalAdjDamage, na.rm=TRUE)

count_YearAboveM <- subset(EmDat, 
                        `Disaster Type` %in% disasterlist & #subsetting to the disaster list
                          TotalAdjDamage > median(EmDat$TotalAdjDamage, na.rm=TRUE)) %>% # only above median damage disasters
  group_by(Yr, ISO) %>% # grouping over year and country
  summarize(NoOfStrongDis = n()) %>% #number of disasters in each country in each year
  ungroup()

count_YearAboveM$StrongDisDummy <- ifelse(count_YearAboveM$NoOfStrongDis >= 1, 1,0) #creating 1 0 dummy.

#Creating very strong disaster dummy:
colnames(EmDat)[45] = "TotalAdjDamage"
quantile(EmDat$TotalAdjDamage, 0.75, na.rm=TRUE)

count_YearAboveQ3 <- subset(EmDat, 
                           `Disaster Type` %in% disasterlist & #subsetting to the disaster list
                             TotalAdjDamage > quantile(EmDat$TotalAdjDamage, 0.75,na.rm=TRUE)) %>% # only above median damage disasters
  group_by(Yr, ISO) %>% # grouping over year and country
  summarize(NoOfvStrongDis = n()) %>% #number of disasters in each country in each year
  ungroup()

count_YearAboveQ3$vStrongDisDummy <- ifelse(count_YearAboveQ3$NoOfvStrongDis >= 1, 1,0) #creating 1 0 dummy.

#Creating death disaster dummy:

count_YearDeath <- subset(EmDat, 
                           `Disaster Type` %in% disasterlist & #subsetting to the disaster list
                            `Total Deaths` > 20) %>% # only above median damage disasters
  group_by(Yr, ISO) %>% # grouping over year and country
  summarize(NoOfDeathDis = n()) %>% #number of disasters in each country in each year
  ungroup()

count_YearDeath$DeathDisDummy <- ifelse(count_YearDeath$NoOfDeathDis >= 1, 1,0) #creating 1 0 dummy.



########## MERGING DATA ##########
BvxDisaster <- merge(BVX, count_YearALL, by=c("ISO","Yr"), all.x = TRUE) #MERGING
BvxDisaster$DisDummy[is.na(BvxDisaster$DisDummy)] <- 0 #creating dummy (this is just because all the 0s were n\a but the disaster data coverage is not a problem here)
BvxDisaster$NoOfDis[is.na(BvxDisaster$NoOfDis)] <- 0 #same as above
#adding strong data
BvxDisaster <- merge(BvxDisaster, count_YearAboveM, by=c("ISO","Yr"), all.x = TRUE) #MERGING
BvxDisaster$StrongDisDummy[is.na(BvxDisaster$StrongDisDummy)] <- 0 #creating 0 1 dummy 
BvxDisaster$NoOfStrongDis[is.na(BvxDisaster$NoOfStrongDis)] <- 0 #same as above
#adding very strong data
BvxDisaster <- merge(BvxDisaster, count_YearAboveQ3, by=c("ISO","Yr"), all.x = TRUE) #MERGING
BvxDisaster$vStrongDisDummy[is.na(BvxDisaster$vStrongDisDummy)] <- 0 #creating 0 1 dummy 
BvxDisaster$NoOfvStrongDis[is.na(BvxDisaster$NoOfvStrongDis)] <- 0 #same as above
#adding Death data
BvxDisaster <- merge(BvxDisaster, count_YearDeath, by=c("ISO","Yr"), all.x = TRUE) #MERGING
BvxDisaster$DeathDisDummy[is.na(BvxDisaster$DeathDisDummy)] <- 0 #creating 0 1 dummy 
#adding regions and income groups: 
BvxDisaster <- merge(BvxDisaster, countrymeta, by=c("ISO"), all.x = TRUE) #MERGING
#Now encoding it as panel data using pdata.frame:
BvxDisaster <- pdata.frame(BvxDisaster, index=c("ISO","Yr"), drop.index=FALSE, row.names=TRUE)


######## MAKING PLOTS ######
#Plotting Natural Disasters: 
#plotting disasters over time for BVX countries
countdisasterbvx <- subset(EmDat, 
                     `Disaster Type` %in% disasterlist 
                     & ISO %in% BVX$ISO) %>%
  group_by(Year) %>%
  summarize(number = n()) %>%
  ungroup()
countdisasterbvx$Year <- ymd(countdisasterbvx$Year, truncated = 2L) #coding year as a data

Disaster_Plot <- ggplot(data = countdisasterbvx, aes(x = Year, y = number))

p1 <- Disaster_Plot +
  geom_point(alpha = 0.6, size = 1)+
  xlab("Year") + ylab("No. of Disasters")+
  theme_bw()+
  stat_smooth(se = FALSE, color = "red")+
  scale_color_brewer(palette="Set1")
p1
#Plotting Natural Disasters by category:
DisasterTypePlot <- ggplot(
  data = subset(EmDat, 
                `Disaster Type` %in% 
                  c("Storm", "Extreme temperature", "Epidemic", "Wildfire") 
                & ISO %in% BVX$ISO),
  aes(x = Yr, color = `Disaster Type`)) 

p2<- DisasterTypePlot + 
  geom_point(stat = "count", alpha = 0.6, size = 1)+
  xlab("Year") + ylab("No. of Disasters")+
  theme_bw()+
  theme(legend.title = element_blank())+
  scale_color_brewer(palette="Set1")

#printing side by side etc.
DisasterPlots <- ggarrange(p1, p2,
                           ncol=2, nrow=1, 
                           common.legend = TRUE, legend="bottom")
ggsave(filename = "plots/BvxDisasterPlots.png", 
       DisasterPlots, 
       width = 6, height = 4)

#Plotting BVX Crises: 

count_YBVX <- subset(BvxDisaster, RC %in% 1) %>%
  group_by(year) %>%
  summarize(number = n()) %>%
  ungroup()

count_YBVX$Yr <- ymd(count_YBVX$year, truncated = 2L) #encoding year as a date again


crisisplot <- ggplot(data = count_YBVX, aes(x = Yr, y = number))

p3 <- crisisplot +
  geom_point(alpha = 0.6, size = 1) + 
  labs(title = "Crises 1900 - 2016") +
  xlab("Year") + ylab("No. of crises")+
  theme_bw()+
  stat_smooth(se = FALSE, color = "red")+
  scale_color_brewer(palette="Set1")

ggsave(filename = "plots/BVXCrisisPlot.png", 
       p3, 
       width = 6, height = 4)

p1 <- p1 +  labs(title = "Disasters 1900 - 2016")
DisasterCrisisPlots <- ggarrange(p1, p3,
                           ncol=1, nrow=2, 
                           common.legend = TRUE, legend="bottom")
ggsave(filename = "plots/DisastervsCrisisPlots.png", 
       DisasterCrisisPlots, 
       width = 6, height = 4)

#plotting strong disasters: 
countstrongdisasterbvx <- subset(EmDat, 
                           `Disaster Type` %in% disasterlist & 
                             TotalAdjDamage > median(EmDat$TotalAdjDamage, na.rm=TRUE) & 
                             ISO %in% BVX$ISO) %>% 
  group_by(Yr) %>% 
  summarize(NoOfStrongDis = n()) %>% 
  ungroup()

countstrongdisasterbvx$Year <- ymd(countstrongdisasterbvx$Yr, truncated = 2L) #coding year as a data

Strongdisaster_Plot <- ggplot(data = countstrongdisasterbvx, aes(x = Year, y = NoOfStrongDis))

p1Strong <- Strongdisaster_Plot +
  geom_point(alpha = 0.6, size = 1)+
  xlab("Year") + ylab("No. of Strong Disasters")+
  theme_minimal()+
  stat_smooth(se = FALSE, color = "red")+
  scale_color_brewer(palette="Set1")

#plotting death disasters: 
countdeathdisasterbvx <- subset(EmDat, 
                                 `Disaster Type` %in% disasterlist & 
                                  `Total Deaths` > 20 & 
                                   ISO %in% BVX$ISO) %>% 
  group_by(Yr) %>% 
  summarize(NoOfStrongDis = n()) %>% 
  ungroup()

countdeathdisasterbvx$Year <- ymd(countdeathdisasterbvx$Yr, truncated = 2L) #coding year as a data

Deathdisaster_Plot <- ggplot(data = countdeathdisasterbvx, aes(x = Year, y = NoOfStrongDis))

p1death <- Deathdisaster_Plot +
  geom_point(alpha = 0.6, size = 1)+
  xlab("Year") + ylab("No. of Strong Disasters")+
  theme_minimal()+
  stat_smooth(se = FALSE, color = "red")+
  scale_color_brewer(palette="Set1")

#Plotting Natural Disasters by category:
  StrongDisasterTypePlot <- ggplot(
    data = subset(EmDat, 
                  `Disaster Type` %in% 
                    c("Storm", "Extreme temperature", "Epidemic", "Wildfire") 
                  & TotalAdjDamage > median(EmDat$TotalAdjDamage, na.rm=TRUE) 
                  & ISO %in% BVX$ISO),
    aes(x = Yr, color = `Disaster Type`)) 

p2Strong<- StrongDisasterTypePlot + 
  geom_point(stat = "count", alpha = 0.6, size = 1)+
  xlab("Year") + ylab("No. of Strong Disasters")+
  theme_minimal()+
  theme(legend.title = element_blank())+
  scale_color_brewer(palette="Set1")

#plotting disasters and crises by BVX countries
countrydisasterbvx <- subset(EmDat, 
                           `Disaster Type` %in% disasterlist 
                           & ISO %in% BVX$ISO) %>%
  group_by(ISO) %>%
  summarize(number = n()) %>%
  ungroup()

countrydisasterbvx <- merge(countrydisasterbvx, countrymeta, by=c("ISO"), all.x = TRUE) #MERGING

DisasterCountry_Plot <- ggplot(data = countrydisasterbvx, aes(x = reorder(ISO, -number), y = number, fill=income_group))

bycountry <- DisasterCountry_Plot +
  geom_bar(stat="identity")+
  xlab("Country") + ylab("No. of Disasters")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45), legend.position="bottom",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="")) +
  scale_color_brewer(palette="Set1")
bycountry

ggsave(filename = "plots/disasterpercountry.png", 
       bycountry, 
       width = 6, height = 4)


countrycrisisno <- subset(BVX, 
                             RC == 1) %>%
  group_by(ISO) %>%
  summarize(crisis = n()) %>%
  ungroup()

countrycrisisplot <- merge(countrydisasterbvx, countrycrisisno, by=c("ISO"), all.x = TRUE) #MERGING
DisasterCrisis_Plot <- ggplot(data = countrycrisisplot, aes(x = reorder(ISO, -crisis), y = crisis, fill=income_group))

bycountrycrisis <- DisasterCrisis_Plot +
  geom_bar(stat="identity")+
  xlab("Country") + ylab("No. of Disasters")+
  theme(axis.text.x = element_text(angle = 45))+
  scale_color_brewer(palette="Set1")



######## LAGGING VARIABLES ######
BvxDisaster$DisDummyL1 <- lag(BvxDisaster$DisDummy, 1)
BvxDisaster$DisDummyL2 <- lag(BvxDisaster$DisDummy, 2)
BvxDisaster$DisDummyL3 <- lag(BvxDisaster$DisDummy, 3)
BvxDisaster$DisDummyL4 <- lag(BvxDisaster$DisDummy, 4)

BvxDisaster$StrongDisDummyL1 <- lag(BvxDisaster$StrongDisDummy, 1)
BvxDisaster$StrongDisDummyL2 <- lag(BvxDisaster$StrongDisDummy, 2)
BvxDisaster$StrongDisDummyL3 <- lag(BvxDisaster$StrongDisDummy, 3)
BvxDisaster$StrongDisDummyL4 <- lag(BvxDisaster$StrongDisDummy, 4)

BvxDisaster$vStrongDisDummyL1 <- lag(BvxDisaster$vStrongDisDummy, 1)
BvxDisaster$vStrongDisDummyL2 <- lag(BvxDisaster$vStrongDisDummy, 2)
BvxDisaster$vStrongDisDummyL3 <- lag(BvxDisaster$vStrongDisDummy, 3)
BvxDisaster$vStrongDisDummyL4 <- lag(BvxDisaster$vStrongDisDummy, 4)

BvxDisaster$DeathDisDummyL1 <- lag(BvxDisaster$DeathDisDummy, 1)
BvxDisaster$DeathDisDummyL2 <- lag(BvxDisaster$DeathDisDummy, 2)
BvxDisaster$DeathDisDummyL3 <- lag(BvxDisaster$DeathDisDummy, 3)
BvxDisaster$DeathDisDummyL4 <- lag(BvxDisaster$DeathDisDummy, 4)

BvxDisaster$C_B30L1 <- lag(BvxDisaster$C_B30, 1)
BvxDisaster$C_B30L2 <- lag(BvxDisaster$C_B30, 2)
BvxDisaster$C_B30L3 <- lag(BvxDisaster$C_B30, 3)
BvxDisaster$C_B30L4 <- lag(BvxDisaster$C_B30, 4)

BvxDisaster$dCredit <- (BvxDisaster$credit_to_gdp - lag(BvxDisaster$credit_to_gdp, 1))
BvxDisaster$dCreditL1 <- lag(BvxDisaster$dCredit, 1)
BvxDisaster$dCreditL2 <- lag(BvxDisaster$dCredit, 2)
BvxDisaster$dCreditL3 <- lag(BvxDisaster$dCredit, 3)
BvxDisaster$dCreditL4 <- lag(BvxDisaster$dCredit, 4)

BvxDisaster$rgdp_grL1 <- lag(BvxDisaster$rgdp_gr, 1)
BvxDisaster$rgdp_grL2 <- lag(BvxDisaster$rgdp_gr, 2)
BvxDisaster$rgdp_grL3 <- lag(BvxDisaster$rgdp_gr, 3)
BvxDisaster$rgdp_grL4 <- lag(BvxDisaster$rgdp_gr, 4)


BvxDisaster$DisDummyL0to2 <- BvxDisaster$DisDummy + BvxDisaster$DisDummyL1 +BvxDisaster$DisDummyL2
BvxDisaster$DisDummyL0to2 <- ifelse(BvxDisaster$DisDummyL0to2 >= 1, 1,0) #creating 1 0 dummy. 

BvxDisaster$bankeqdecline[is.na(BvxDisaster$bankeqdecline)] <- 0 #make dummy for bank equity decline


######## Printing REGRESSIONS ######
OLSSimpleALL <- lm(RC ~ DisDummy + DisDummyL1 + DisDummyL2 + factor(country), BvxDisaster)
OLSSimpleALL_fe <- lm(RC ~ DisDummy + DisDummyL1 + DisDummyL2, BvxDisaster)
LogitSimpleALL <- glm(RC ~ DisDummy + DisDummyL1 + DisDummyL2, 
                  family = binomial(), BvxDisaster)
LogitSimpleALL_fe <- glm(RC ~ DisDummy + DisDummyL1 + DisDummyL2  
                     + factor(country), family = binomial(), BvxDisaster)
LogitStimpleSTRONG_fe <- glm(RC ~ StrongDisDummy +StrongDisDummyL1 +StrongDisDummyL2
                     + factor(country), family = binomial(), BvxDisaster) #this is a nice one...
LogitStimpleSTRONG_lowincome_fe <- glm(RC ~ StrongDisDummy +StrongDisDummyL1 +StrongDisDummyL2
                   + factor(country), family = binomial(), 
                   subset(BvxDisaster, income_group %in% c("Lower middle income", 
                                                           "Upper middle income", "Low income"))) #works really well
LogitStimpleSTRONG_highincome_fe <- glm(RC ~ StrongDisDummy +StrongDisDummyL1 +StrongDisDummyL2
                                       + factor(country), family = binomial(), 
                                       subset(BvxDisaster, income_group %in% c("High income")))
LogitSTRONG_fe <- glm(RC ~ StrongDisDummy +StrongDisDummyL1 +StrongDisDummyL2
                             +dCreditL1+dCreditL2
                             + factor(country), family = binomial(), BvxDisaster) 
LogitPlusSTRONG_fe <- glm(RC ~ StrongDisDummy +StrongDisDummyL1 +StrongDisDummyL2
                      +dCreditL1+dCreditL2 +rgdp_grL1+rgdp_grL2
                      + factor(country), family = binomial(), BvxDisaster) 
summary(LogitPlusSTRONG_fe)

BvxDisaster$DisAndCredit<- BvxDisaster$dCreditL1*BvxDisaster$DisDummyL0to2
LogitcombSTRONG_fe <- glm(RC ~  DisDummyL0to2
                          +dCreditL1 
                          + factor(country), family = binomial(), BvxDisaster) 

LogitInteractSTRONG_fe <- glm(RC ~  DisDummyL0to2
                              +dCreditL1 +DisAndCredit
                              + factor(country), family = binomial(), BvxDisaster) 
summary(LogitInteractSTRONG_fe)

stargazer(OLSSimpleALL,
          OLSSimpleALL_fe,
          LogitSimpleALL,
          LogitSimpleALL_fe,
          align = TRUE, 
          title = 'Estimated effect of natural disaster on crisis odds',
          label='crisis',
          type = 'latex', 
          keep=c('DisDummy','DisDummyL1','DisDummyL2','DisDummyL3',
                 'StrongDisDummy', 'StrongDisDummyL1', 'StrongDisDummyL2',
                  'dCreditL1', 'dCreditL2', 'rgdp_grL1', 'rgdp_grL2',
                 'DisDummyL0to2', 'DisAndCredit'),
          covariate.labels=c("$Disaster_{t}$",
                             "$Disaster_{t-1}$",
                             "$Disaster_{t-2}$"),
          dep.var.labels = "Binary crisis variable",
          out = 'tables/crisis.tex',
          keep.stat = c("n","adj.rsq"),
          no.space = TRUE, # to remove the spaces after each line of coefficients
          column.sep.width = "2pt", # to reduce column width
          #font.size = "small",
          add.lines = list(c("Country fixed effects", "No", "Yes", "No", "Yes")),
          notes = "Estimates are coefficients and standard errors are shown in parantheses."
          )

stargazer(LogitStimpleSTRONG_fe,
          LogitStimpleSTRONG_lowincome_fe,
          LogitStimpleSTRONG_highincome_fe,
          LogitSTRONG_fe,
          LogitPlusSTRONG_fe,
          align = TRUE, 
          title = 'Estimated logit model of effect of a strong natural disaster on crisis odds',
          label='strongcrisis',
          type = 'latex', 
          keep=c('DisDummy','DisDummyL1','DisDummyL2','DisDummyL3',
                 'StrongDisDummy', 'StrongDisDummyL1', 'StrongDisDummyL2',
                 'dCreditL1', 'dCreditL2', 'rgdp_grL1', 'rgdp_grL2',
                 'DisDummyL0to2', 'DisAndCredit'),
          covariate.labels=c("$StrongDisaster_{t}$",
                             "$StrongDisaster_{t-1}$",
                             "$StrongDisaster_{t-2}$",
                             "$\\Delta Credit_{t-1}$",
                             "$\\Delta Credit_{t-2}$",
                             "$\\Delta ln(rGDP)_{t-1}$",
                             "$\\Delta ln(rGDP)_{t-2}$"),
          dep.var.labels = "Binary crisis variable",
          out = 'tables/strongcrisis.tex',
          column.labels=c('Baseline', 'Selected ',
                          'Selected','Controls', 'Controls'),
          omit.stat = "aic",
          no.space = TRUE, # to remove the spaces after each line of coefficients
          column.sep.width = "2pt", # to reduce column width
          font.size = "small",
          add.lines = list(c("Country fixed effects","Yes","Yes","Yes","Yes","Yes")),
          notes = "Estimates are coefficients and standard errors are shown in parantheses."
)

#now looking at panics:
logitPanicCrisis_fe <- glm(PANIC_ind ~ StrongDisDummy +StrongDisDummyL1 +StrongDisDummyL2
                     + factor(country), family = binomial(), BvxDisaster) #VERY STRONG? --> COOOL

logitPanicYr_fe <- glm(PANIC_year ~ StrongDisDummy +StrongDisDummyL1 +StrongDisDummyL2
                     + factor(country), family = binomial(), BvxDisaster) 

logitPanicControl_fe <- glm(PANIC_year ~ StrongDisDummy +StrongDisDummyL1 +StrongDisDummyL2
                     + dCreditL1+dCreditL2 +rgdp_grL1+rgdp_grL2
                     + factor(country), family = binomial(), BvxDisaster) 

logitBeq_fe <- glm(C_B30 ~ StrongDisDummy +StrongDisDummyL1 +StrongDisDummyL2
                   + dCreditL1+dCreditL2 +rgdp_grL1+rgdp_grL2  
                   + factor(country), family = binomial(), BvxDisaster)  #ohh great predictor of eq decline

logitNfeq_fe <- glm(C_N30 ~ StrongDisDummy +StrongDisDummyL1 +StrongDisDummyL2
                    + dCreditL1+dCreditL2 +rgdp_grL1+rgdp_grL2 
                    + factor(country), family = binomial(), BvxDisaster) 

stargazer(logitPanicCrisis_fe,
          logitPanicYr_fe,
          logitPanicControl_fe,
          logitBeq_fe,
          align = TRUE, 
          title = 'Estimated effect of natural disasters on panics',
          label='panics',
          type = 'latex', 
          keep=c('StrongDisDummy', 'StrongDisDummyL1', 'StrongDisDummyL2',
                 'dCreditL1', 'dCreditL2', 'rgdp_grL1', 'rgdp_grL2',
                 'DisDummyL0to2', 'DisAndCredit'),
          covariate.labels=c("$StrongDisaster_{t}$",
                             "$StrongDisaster_{t-1}$",
                             "$StrongDisaster_{t-2}$",
                             "$\\Delta Credit_{t-1}$",
                             "$\\Delta Credit_{t-2}$",
                             "$\\Delta ln(rGDP)_{t-1}$",
                             "$\\Delta ln(rGDP)_{t-2}$"),
          out = 'tables/panic.tex',
          omit.stat = "aic",
          no.space = TRUE, # to remove the spaces after each line of coefficients
          column.sep.width = "2pt", # to reduce column width
          font.size = "small",
          add.lines = list(c("Country fixed effects","Yes","Yes","Yes","Yes")),
          notes = "Estimates are coefficients and standard errors are shown in parantheses."
)
#appendix for year fixed effects: 
lm_yrfe <- lm(RC ~ StrongDisDummy +StrongDisDummyL1 +StrongDisDummyL2
               + factor(country) +factor(year), BvxDisaster)

lmlow_yrfe <- lm(RC ~ StrongDisDummy +StrongDisDummyL1 +StrongDisDummyL2
               + factor(country) +factor(year), 
               subset(BvxDisaster, income_group %in% c("Lower middle income", 
                                                       "Upper middle income", "Low income")))

LmPANIC_yrfe <- lm(PANIC_year ~ StrongDisDummy +StrongDisDummyL1 +StrongDisDummyL2
                   + factor(country) +factor(year), BvxDisaster)

LmlowPANIC_yrfe <- lm(PANIC_year ~ StrongDisDummy +StrongDisDummyL1 +StrongDisDummyL2
                   + factor(country) +factor(year), 
                   subset(BvxDisaster, income_group %in% c("Lower middle income", 
                                                           "Upper middle income", 
                                                           "Low income"))) #here the year fixed effects work. 

stargazer(lm_yrfe,
          lmlow_yrfe,
          LmPANIC_yrfe,
          LmlowPANIC_yrfe,
          align = TRUE, 
          title = 'Including year fixed effects',
          label='yrfe',
          type = 'latex', 
          keep=c('StrongDisDummy', 'StrongDisDummyL1', 'StrongDisDummyL2',
                 'dCreditL1', 'dCreditL2', 'rgdp_grL1', 'rgdp_grL2',
                 'DisDummyL0to2', 'DisAndCredit'),
          covariate.labels=c("$StrongDisaster_{t}$",
                             "$StrongDisaster_{t-1}$",
                             "$StrongDisaster_{t-2}$",
                             "$\\Delta Credit_{t-1}$",
                             "$\\Delta Credit_{t-2}$",
                             "$\\Delta ln(rGDP)_{t-1}$",
                             "$\\Delta ln(rGDP)_{t-2}$"),
          out = 'tables/yrfe.tex',
          keep.stat = c("n","adj.rsq"),
          no.space = TRUE, # to remove the spaces after each line of coefficients
          column.sep.width = "2pt", # to reduce column width
          font.size = "small",
          add.lines = list(c("Country fixed effects","Yes","Yes","Yes","Yes"),
                           c("Year fixed effects", "Yes", "Yes","Yes","Yes")),
          notes = "Estimates are coefficients and standard errors are shown in parantheses.")






######## Printing Projections ######
vars <- list(BvxDisaster$Fd1y,BvxDisaster$Fd2y,BvxDisaster$Fd3y,BvxDisaster$Fd4y,BvxDisaster$Fd5y,BvxDisaster$Fd6y,BvxDisaster$Fd7y)
#List to store regressions
reglist<-list()
for(i in 1:length(vars)){
  x<-vars[[i]]
  reglist[[i]]<- lm(x ~ StrongDisDummy
                    +factor(country),
                    BvxDisaster)
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


#nextlp:
vars <- list(BvxDisaster$Fd1credit_to_gdp,BvxDisaster$Fd2credit_to_gdp,BvxDisaster$Fd3credit_to_gdp,BvxDisaster$Fd4credit_to_gdp,BvxDisaster$Fd5credit_to_gdp,BvxDisaster$Fd6credit_to_gdp,BvxDisaster$Fd7credit_to_gdp)
#List to store regressions
reglist<-list()
for(i in 1:length(vars)){
  x<-vars[[i]]
  reglist[[i]]<- lm(x ~ StrongDisDummy
                    +factor(country),
                    BvxDisaster)
  reglist[[i]]<-coeftest(reglist[[i]],vcov=vcovHC(reglist[[i]],cluster="ISO",type="HC2"))
  model<-tidy(reglist[[i]], conf.int = TRUE, conf.level = 0.9)%>%
    filter(term=='StrongDisDummy')%>%
    mutate(model=paste('h=',i,sep=''))
  if(i==1){
    regdf2<-model
  }else {
    regdf2<-rbind(regdf2,model)
  }
}

regdf2<-rbind(tibble(term = "StrongDisDummy", 
                     estimate = 0, 
                     std.error = 0, 
                     statistic = 0,
                     p.value = 0,
                     conf.low = 0,
                     conf.high = 0,
                     model = "h=0"),regdf2)


pd <- position_dodge(0.1)
creditlp <- ggplot(regdf2, aes(model,estimate, group=1)) +
  geom_point() +
  geom_line(linetype = "dashed",color="blue", size = .8)+
  theme_bw() +
  geom_hline(yintercept=0) + 
  scale_y_continuous("change in credit (%GDP)") +
  xlab("Years after disaster") +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.1, position=pd)

ggsave(filename = "plots/creditlp.png", 
       creditlp, 
       width = 6, height = 4)
ggsave(filename = "plots/gdplp.png", 
       gdplp, 
       width = 6, height = 4)

lps <- ggarrange(gdplp, creditlp,
                                 ncol=2, nrow=1, 
                                 common.legend = TRUE, legend="bottom")
ggsave(filename = "plots/lps.png", 
       lps, 
       width = 6, height = 3)

