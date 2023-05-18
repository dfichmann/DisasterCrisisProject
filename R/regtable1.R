#Regression Table 1

#using standardized strong disaster variables
MyData <- lagvar(stnDAM_strong, 4, MyData) #lagged up to 4 years

T1M1 <- glm(crisisJST ~ stnDAM_strong +stnDAM_strong_L1 + stnDAM_strong_L2 + stnDAM_strong_L3,
            family = binomial(), MyData) #no fixed effects
summary(T1M1)


T1M2 <- glm(crisisJST ~ stnDAM_strong +stnDAM_strong_L1 + stnDAM_strong_L2 + stnDAM_strong_L3 
            + factor(ISO), family = binomial(), MyData) #country fixed effects
summary(T1M2)

T1M3 <- glm(crisisJST ~ stnDAM_strong +stnDAM_strong_L1 + stnDAM_strong_L2 + stnDAM_strong_L3 
            + factor(ISO), family = binomial(), filter(MyData, year >= 1950)) #restricted sample (1950-2020)
summary(T1M3)

T1M4 <- lm(crisisJST ~ stnDAM_strong +stnDAM_strong_L1 + stnDAM_strong_L2 + stnDAM_strong_L3
           , MyData) #OLS and no fixed effects
summary(T1M4)

T1M5 <- lm(crisisJST ~ stnDAM_strong +stnDAM_strong_L1 + stnDAM_strong_L2 + stnDAM_strong_L3
           + factor(ISO) + factor(year), MyData) #OLS with country and year fixed effects
summary(T1M5)


# Calculate robust standard errors for all specifications
# Cluster standard errors at the country level for each model
cluster_se_T1M1 <- sqrt(diag(cluster.vcov(T1M1, MyData$ISO))) #clustered at the country level
cluster_se_T1M2 <- sqrt(diag(cluster.vcov(T1M2, MyData$ISO))) #clustered at the country level
cluster_se_T1M3 <- sqrt(diag(cluster.vcov(T1M3, filter(MyData, year >= 1950)$ISO))) #clustered at the country level
cluster_se_T1M4 <- sqrt(diag(cluster.vcov(T1M4, MyData$ISO))) #clustered at the country level
suppressWarnings({
cluster_se_T1M5 <- sqrt(diag(cluster.vcov(T1M5, MyData$ISO))) #clustered at the country level (warning message is ok)
})

#checking for multicol...
MyData_subset <- MyData[, c("crisisJST","stnDAM_strong", "stnDAM_strong_L1", "stnDAM_strong_L2", "stnDAM_strong_L3")]
MyData_subset <- na.omit(MyData_subset)
cor_matrix <- cor(MyData_subset)
print(cor_matrix)
cor_df <- as.data.frame(cor_matrix)
# Print correlation matrix as a table
cor_table <- xtable(cor_df)
# Print the table
print(cor_table)



stargazer(T1M1,T1M2,T1M3,T1M4,T1M5, 
          title = 'Effect of a strong natural disaster on crisis risk (1900 - 2020)', 
          label='T1', type = 'latex',
          out = 'tables/T1.tex',
          
          font.size = "footnotesize", no.space = TRUE, multicolumn = TRUE,
          align = TRUE, df = FALSE, omit.stat = c("rsq", "aic"), notes.align = "l",
          column.sep.width = "-5pt",
          
          
          dep.var.caption = "Dependent Variable: Crisis in $year = t$ ",
          dep.var.labels = "",
          omit.table.layout = "d",
          
          keep=c('stnDAM_strong', 'stnDAM_strong_L1', 'stnDAM_strong_L2'),
          se = list(cluster_se_T1M1, cluster_se_T1M2,cluster_se_T1M3,cluster_se_T1M4,cluster_se_T1M5),
          
          covariate.labels=c("$Damages (pc GDP)_{t}$",
                             "$Damages (pc GDP)_{t-1}$",
                             "$Damages (pc GDP)_{t-2}$",
                             "$Damages (pc GDP)_{t-3}$"),
          
          add.lines = list(c("Country fixed effects","No","Yes","Yes","No","Yes"),
                           c("Year fixed effects", "No", "No","No","No","Yes"),
                           c("Restricted sample", "No", "No"," Yes","No","No")),
          
          notes = "Cluster-robust standard errors are shown in parentheses."

)

# Table (Annex) with heteroskedasticity-robust standard errors
robust_se_T1M1 <- sqrt(diag(vcovHC(T1M1, type = "HC1"))) #robust standard errors
robust_se_T1M2 <- sqrt(diag(vcovHC(T1M2, type = "HC1"))) #robust standard errors
robust_se_T1M3 <- sqrt(diag(vcovHC(T1M3, type = "HC1"))) #robust standard errors
robust_se_T1M4 <- sqrt(diag(vcovHC(T1M4, type = "HC1"))) #robust standard errors
robust_se_T1M5 <- sqrt(diag(vcovHC(T1M5, type = "HC1"))) #robust standard errors

stargazer(T1M1,T1M2,T1M3,T1M4,T1M5, 
          title = 'ANNEX: Effect of a strong natural disaster on crisis risk (1900 - 2020)',
          label='TA1_1', type = 'latex', 
          out = 'tables/TA1_1.tex',
          
          font.size = "footnotesize", no.space = TRUE, multicolumn = TRUE,
          align = TRUE, df = FALSE, omit.stat = c("rsq", "aic"), notes.align = "l",
          column.sep.width = "-5pt",
          
          
          dep.var.caption = "Dependent Variable: Crisis in $year = t$ ",
          dep.var.labels = "",
          omit.table.layout = "d",
          
          omit=c('factor'),
          se = list(robust_se_T1M1, robust_se_T1M2, robust_se_T1M3, robust_se_T1M4, robust_se_T1M5),

          covariate.labels=c("$Damages (pc GDP)_{t}$",
                             "$Damages (pc GDP)_{t-1}$",
                             "$Damages (pc GDP)_{t-2}$",
                             "$Damages (pc GDP)_{t-3}$"),
          
          add.lines = list(c("Country fixed effects","No","Yes","Yes","No","Yes"),
                           c("Year fixed effects", "No", "No","No","No","Yes"),
                           c("Restricted sample", "No", "No"," Yes","No","No")),
          
          notes = "Heteroskedasticity-robust standard errors are shown in parentheses."
          
)
# Table : Effect of a strong UNWEIGHTED natural disaster on crisis risk (1900 - 2020)

MyData <- lagvar(stnDAM_strong_uw, 4, MyData) #create lagged variables

T1M1 <- glm(crisisJST ~ stnDAM_strong_uw +stnDAM_strong_uw_L1 + stnDAM_strong_uw_L2 + stnDAM_strong_uw_L3,
            family = binomial(), MyData) #no fixed effects
summary(T1M1)

T1M2 <- glm(crisisJST ~ stnDAM_strong_uw +stnDAM_strong_uw_L1 + stnDAM_strong_uw_L2 + stnDAM_strong_uw_L3 
            + factor(ISO), family = binomial(), MyData) #country fixed effects
summary(T1M2)

T1M3 <- glm(crisisJST ~ stnDAM_strong_uw +stnDAM_strong_uw_L1 + stnDAM_strong_uw_L2 + stnDAM_strong_uw_L3 
            + factor(ISO), family = binomial(), filter(MyData, year >= 1950)) #restricted sample (1950-2020)
summary(T1M3)

T1M4 <- lm(crisisJST ~ stnDAM_strong_uw +stnDAM_strong_uw_L1 + stnDAM_strong_uw_L2 + stnDAM_strong_uw_L3
           , MyData)
summary(T1M4)

T1M5 <- lm(crisisJST ~ stnDAM_strong_uw +stnDAM_strong_uw_L1 + stnDAM_strong_uw_L2 + stnDAM_strong_uw_L3
           + factor(ISO) + factor(year), MyData)
summary(T1M5)

cluster_se_T1M1 <- sqrt(diag(cluster.vcov(T1M1, MyData$ISO))) #cluster-robust standard errors
cluster_se_T1M2 <- sqrt(diag(cluster.vcov(T1M2, MyData$ISO)))
cluster_se_T1M3 <- sqrt(diag(cluster.vcov(T1M3, filter(MyData, year >= 1950)$ISO)))
cluster_se_T1M4 <- sqrt(diag(cluster.vcov(T1M4, MyData$ISO)))
cluster_se_T1M5 <- sqrt(diag(cluster.vcov(T1M5, MyData$ISO)))

stargazer(T1M1,T1M2,T1M3,T1M4,T1M5,
          title = 'ANNEX Effect of a strong natural disaster (unweighted) on crisis risk (1900 - 2020)',
          label='TA1_2', type = 'latex', out = 'tables/TA1_2.tex',
          
          font.size = "footnotesize", no.space = TRUE, multicolumn = TRUE,
          align = TRUE, df = FALSE, omit.stat = c("rsq", "aic"), notes.align = "l",
          column.sep.width = "-5pt",
          
          
          dep.var.caption = "Dependent Variable: Crisis in $year = t$ ",
          dep.var.labels = "",
          omit.table.layout = "d",
          
          omit=c('factor'),
          se = list(cluster_se_T1M1, cluster_se_T1M2,cluster_se_T1M3,cluster_se_T1M4,cluster_se_T1M5),
          
          covariate.labels=c("$Damages (pc GDP)_{t}$",
                             "$Damages (pc GDP)_{t-1}$",
                             "$Damages (pc GDP)_{t-2}$",
                             "$Damages (pc GDP)_{t-3}$"),
          
          add.lines = list(c("Country fixed effects","No","Yes","Yes","No","Yes"),
                           c("Year fixed effects", "No", "No","No","No","Yes"),
                           c("Restricted sample", "No", "No"," Yes","No","No")),
          
          notes = "Cluster-robust standard errors are shown in parentheses."
          
          #apply.coef = exp,
)

# Table Effect of a strong natural disaster (weighted) but not limited to strong on crisis risk (1900 - 2020)

MyData <- lagvar(stnDAM, 4, MyData) #create lagged variables

T1M1 <- glm(crisisJST ~ stnDAM +stnDAM_L1 + stnDAM_L2 + stnDAM_L3,
            family = binomial(), MyData) #no fixed effects
summary(T1M1)

T1M2 <- glm(crisisJST ~ stnDAM +stnDAM_L1 + stnDAM_L2 + stnDAM_L3
            + factor(ISO), family = binomial(), MyData)
summary(T1M2)

T1M3 <- glm(crisisJST ~ stnDAM +stnDAM_L1 + stnDAM_L2 + stnDAM_L3
            + factor(ISO), family = binomial(), filter(MyData, year >= 1950))
summary(T1M3)

T1M4 <- lm(crisisJST ~ stnDAM +stnDAM_L1 + stnDAM_L2 + stnDAM_L3, MyData)
summary(T1M4)

T1M5 <- lm(crisisJST ~ stnDAM +stnDAM_L1 + stnDAM_L2 + stnDAM_L3
           + factor(ISO) + factor(year), MyData)
summary(T1M5)

cluster_se_T1M1 <- sqrt(diag(cluster.vcov(T1M1, MyData$ISO)))
cluster_se_T1M2 <- sqrt(diag(cluster.vcov(T1M2, MyData$ISO)))
cluster_se_T1M3 <- sqrt(diag(cluster.vcov(T1M3, filter(MyData, year >= 1950)$ISO)))
cluster_se_T1M4 <- sqrt(diag(cluster.vcov(T1M4, MyData$ISO)))
cluster_se_T1M5 <- sqrt(diag(cluster.vcov(T1M5, MyData$ISO)))

stargazer(T1M1,T1M2,T1M3,T1M4,T1M5,
          title = 'ANNEX Effect of a natural disaster on crisis risk (1900 - 2020)',
          label='TA1_3', type = 'latex', out = 'tables/TA1_3.tex',
          
          font.size = "footnotesize", no.space = TRUE, multicolumn = TRUE,
          align = TRUE, df = FALSE, omit.stat = c("rsq", "aic"), notes.align = "l",
          column.sep.width = "-5pt",
          
          
          dep.var.caption = "Dependent Variable: Crisis in $year = t$ ",
          dep.var.labels = "",
          omit.table.layout = "d",
          
          omit=c('factor'),
          se = list(cluster_se_T1M1, cluster_se_T1M2,cluster_se_T1M3,cluster_se_T1M4,cluster_se_T1M5),
          
          covariate.labels=c("$Damages (pc GDP)_{t}$",
                             "$Damages (pc GDP)_{t-1}$",
                             "$Damages (pc GDP)_{t-2}$",
                             "$Damages (pc GDP)_{t-3}$"),
          
          add.lines = list(c("Country fixed effects","No","Yes","Yes","No","Yes"),
                           c("Year fixed effects", "No", "No","No","No","Yes"),
                           c("Restricted sample", "No", "No"," Yes","No","No")),
          
          notes = "Cluster-robust standard errors are shown in parentheses."
          
          #apply.coef = exp,
)

## WITH BVX DATA ###################################################################

#standardize using world bank gdp data from 1960 to 2020: 
MyData <- lagvar(GDP_nominal_000US, 1, MyData) #create lagged variables so that GDP is unaffected by disaster
MyData$stnDAM_strong2 <- (MyData$w_strong_DAM_000US / MyData$GDP_nominal_000US_L1)*1000 #standardize by GDP

MyData <- lagvar(stnDAM_strong2, 4, MyData) #create lagged variables
MyData <- lagvar(strong_DAM_000US, 4, MyData) #create lagged variables

#regressions

T1M1 <- glm(RC ~ stnDAM_strong2 +stnDAM_strong2_L1 + stnDAM_strong2_L2 + stnDAM_strong2_L3,
            family = binomial(), MyData) #no fixed effects

T1M2 <- glm(RC ~ stnDAM_strong2 +stnDAM_strong2_L1 + stnDAM_strong2_L2 + stnDAM_strong2_L3
            + factor(ISO), family = binomial(), MyData) #country fixed effects
#show regression results
summary(T1M2)

cluster_se_T1M1 <- sqrt(diag(cluster.vcov(T1M1, MyData$ISO))) #clustered at the country level
cluster_se_T1M2 <- sqrt(diag(cluster.vcov(T1M2, MyData$ISO))) #clustered at the country level

 #Table 1 with BVX Data instead:

stargazer(T1M1,T1M2, 
          title = 'Effect of a strong natural disaster on crisis risk (with additional countries) (1960 - 2020)', 
          label='T1_bvx', type = 'latex',
          out = 'tables/T1_bvx.tex',
          
          font.size = "footnotesize", no.space = TRUE, multicolumn = TRUE,
          align = TRUE, df = FALSE, omit.stat = c("rsq", "aic"), notes.align = "l",
          column.sep.width = "-5pt",
          
          dep.var.caption = "Dependent Variable: Crisis in $year = t$ ",
          dep.var.labels = "",
          omit.table.layout = "d",
          omit=c('factor'),
          se = list(cluster_se_T1M1, cluster_se_T1M2),
          
          covariate.labels=c("$Damages (pc GDP)_{t}$",
                             "$Damages (pc GDP)_{t-1}$",
                             "$Damages (pc GDP)_{t-2}$",
                             "$Damages (pc GDP)_{t-3}$"),
          
          add.lines = list(c("Country fixed effects","No","Yes")),
          
          notes = "Cluster-robust standard errors are shown in parentheses."

)

# Table 1s using dummy variables for disaster and using JST and BVX data

#coding a strong disasters_dummy:
MyData <- lagvar(StrongDisDummy, 4, MyData)  #lagging the dummy
MyData <- lagvar(NoOfDis, 4, MyData) #lagging the number of disasters
MyData <- lagvar(DisDummy, 4, MyData) #lagging the dummy

#coding a weighted strong disaster_dummy
MyData$w_DisDummy <- MyData$W_NoOfDis/MyData$NoOfDis #weighting the dummy
MyData$w_StrongDisDummy <- MyData$w_NoOfStrongDisaster / MyData$NoOfStrongDisaster #weighting the dummy

MyData <- lagvar(w_StrongDisDummy, 4, MyData) #lagging the dummy
MyData <- lagvar(w_DisDummy, 4, MyData) #lagging the dummy

MyData$strong_DAM_100MUS <- MyData$strong_DAM_000US/ 1000000 #scale
MyData$strong_DAM_100MUS_L1 <- MyData$strong_DAM_000US_L1/1000000 #scale
MyData$strong_DAM_100MUS_L2 <- MyData$strong_DAM_000US_L2/1000000 #scale
MyData$strong_DAM_100MUS_L3 <- MyData$strong_DAM_000US_L3/1000000 #scale

# Subset the data to include only observations where both dependent variables are not NA
data_subset <- subset(MyData, !is.na(RC) & !is.na(crisisJST) & !is.na(stnDAM_strong)) 

T1M1 <- glm(RC ~ stnDAM_strong +stnDAM_strong_L1 + stnDAM_strong_L2 + stnDAM_strong_L3 
            + factor(ISO), family = binomial(), data_subset) #country fixed effects
T1M2 <- glm(crisisJST ~ DisDummy + DisDummy_L1 + DisDummy_L2 + DisDummy_L3 
            + factor(ISO), family = binomial(), data_subset) #using JST
T1M3 <- glm(crisisJST ~ NoOfDis + NoOfDis_L1 + NoOfDis_L2 + NoOfDis_L3 
            + factor(ISO), family = binomial(), data_subset) #using JST
T1M4 <- glm(crisisJST ~ strong_DAM_100MUS + strong_DAM_100MUS_L1 + strong_DAM_100MUS_L2 + strong_DAM_100MUS_L3 
            + factor(ISO), family = binomial(), data_subset) #using JST


cluster_se_T1M1 <- sqrt(diag(cluster.vcov(T1M1, data_subset$ISO))) #clustered at the country level
cluster_se_T1M2 <- sqrt(diag(cluster.vcov(T1M2, data_subset$ISO))) #clustered at the country level
cluster_se_T1M3 <- sqrt(diag(cluster.vcov(T1M3, data_subset$ISO))) #clustered at the country level
cluster_se_T1M4 <- sqrt(diag(cluster.vcov(T1M4, data_subset$ISO))) #clustered at the country level
stargazer(T1M1,T1M2,T1M3,T1M4, 
          title = 'ANNEX: Additional Robustness checks (1900 - 2020)', 
          label='TA1_4', type = 'latex',
          out = 'tables/TA1_4.tex',
          
          font.size = "footnotesize", no.space = TRUE, multicolumn = TRUE,
          align = TRUE, df = FALSE, omit.stat = c("rsq", "aic"), notes.align = "l",
          column.sep.width = "-5pt",
          dep.var.labels.include = TRUE,
          dep.var.caption = "Dependent Variable: Crisis in $year = t$ ",
          dep.var.labels = c("BVX","JST", "JST", "JST"),
          column.separate = c(1,1,1,1),
          #omit.table.layout = "d",
          omit=c('factor'),
          se = list(cluster_se_T1M1, cluster_se_T1M2,cluster_se_T1M3,cluster_se_T1M4),
          
          covariate.labels=c("$Damages (pc GDP)_{t}$",
                             "$Damages (pc GDP)_{t-1}$",
                             "$Damages (pc GDP)_{t-2}$",
                             "$Damages (pc GDP)_{t-3}$",
                             "$Disaster_{t}$",
                             "$Disaster_{t-1}$",
                             "$Disaster_{t-2}$",
                             "$Disaster_{t-3}$",
                             "$No.ofDis_{t}$",
                             "$No.ofDis_{t-1}$",
                             "$No.ofDis_{t-2}$",
                             "$No.ofDis_{t-3}$",
                             "$DamB(US)_{t}$",
                             "$DamB(US)_{t-1}$",
                             "$DamB(US)_{t-2}$",
                             "$DamB(US)_{t-3}$"),
          
          add.lines = list(c("Country fixed effects","Yes","Yes","Yes","Yes")),
          
          notes = "Cluster-robust standard errors are shown in parentheses."

)


# Table for disaster dummies by countries

T1M1 <- glm(RC ~ DisDummy +DisDummy_L1 + DisDummy_L2 + DisDummy_L3
            + factor(ISO), family = binomial(), MyData) #using BVX
T1M2 <- glm(RC ~ StrongDisDummy +StrongDisDummy_L1 + StrongDisDummy_L2 + StrongDisDummy_L3 
            + factor(ISO), family = binomial(), MyData) #using BVX
T1M3 <- glm(RC ~ StrongDisDummy + StrongDisDummy_L1 + StrongDisDummy_L2 + StrongDisDummy_L3
            + factor(ISO), family = binomial(), subset(MyData, income_group %in% c("High income")))
T1M4 <- glm(RC ~ StrongDisDummy + StrongDisDummy_L1 + StrongDisDummy_L2 + StrongDisDummy_L3
            + factor(ISO), family = binomial(), subset(MyData, income_group %in% c("Lower middle income", 
                                                           "Upper middle income", "Low income")))
cluster_se_T1M1 <- sqrt(diag(cluster.vcov(T1M1, MyData$ISO))) #clustered at the country level
cluster_se_T1M2 <- sqrt(diag(cluster.vcov(T1M2, MyData$ISO))) #clustered at the country level
cluster_se_T1M3 <- sqrt(diag(cluster.vcov(T1M3, subset(MyData, income_group %in% c("High income"))$ISO))) 
cluster_se_T1M4 <- sqrt(diag(cluster.vcov(T1M4, subset(MyData, income_group %in% c("Lower middle income", 
                                                           "Upper middle income", "Low income"))$ISO))) 

stargazer(T1M1,T1M2,T1M3,T1M4, 
          title = 'Disaster Dummies (1900 - 2020)', 
          label='T1_dummy', type = 'latex',
          out = 'tables/T1_dummy.tex',
          
          font.size = "footnotesize", no.space = TRUE, multicolumn = TRUE,
          align = TRUE, df = FALSE, omit.stat = c("rsq", "aic"), notes.align = "l",
          column.sep.width = "-5pt",
          
          dep.var.caption = "Dependent Variable: Crisis in $year = t$ ",
          dep.var.labels = c("JST","BVX","JST","BVX"),
          omit.table.layout = "d",
          omit=c('factor'),
          se = list(cluster_se_T1M1, cluster_se_T1M2, cluster_se_T1M3, cluster_se_T1M4),
          
          covariate.labels=c("$Disaster_{t}$",
                             "$Disaster_{t-1}$",
                             "$Disaster_{t-2}$",
                             "$Disaster_{t-3}$",
                             "$Strong Disaster_{t}$",
                             "$Strong Disaster_{t-1}$",
                             "$Strong Disaster_{t-2}$",
                             "$Strong Disaster_{t-3}$"),
          
          add.lines = list(c("Country fixed effects","Yes","Yes","Yes","Yes"),
                           c("Income group","All","All","High","Low")),  
          
          notes = "Cluster-robust standard errors are shown in parentheses."

)







