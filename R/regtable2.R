###### Creating log change real credit ####
MyData$rloans<-log(MyData$tloans/MyData$cpi)
for (i in 0:6) {
  MyData[,paste('creditL',i-1,sep="")]<- lag(MyData[,paste('rloans')], i-1)-lag(MyData[,paste('rloans')], i)
}

MyData <- lagvar(creditMA5, 1, MyData) #lag moving average to make so that it is only calculated using past observations
#View(subset(MyData, select = c(Yr, ISO, creditD, creditMA5, creditMA5_L1)))


MyData$stnDAM_strong_MA5 <- (MyData$stnDAM_strong_L1 + MyData$stnDAM_strong_L2 + MyData$stnDAM_strong_L3)/3

T2M1 <- glm(crisisJST ~ stnDAM_strong +stnDAM_strong_L1 + stnDAM_strong_L2 + stnDAM_strong_L3
            + creditL1 + creditL2 + creditL3 + creditL4 
            + factor(ISO), family = binomial(), MyData)
summary(T2M1)

T2M2 <- glm(crisisJST ~ stnDAM_strong +stnDAM_strong_L1 + stnDAM_strong_L2 + stnDAM_strong_L3 
            + creditL1 + creditL2 + creditL3 + creditL4 
            + stnDAM_strong_L2*creditL2
            + factor(ISO), family = binomial(), MyData)
summary(T2M2)

T2M3 <- glm(crisisJST ~ stnDAM_strong_MA5 + creditMA5_L1
            + factor(ISO), family = binomial(), MyData)
summary(T2M3)

T2M4 <- glm(crisisJST ~ stnDAM_strong_MA5 + creditMA5_L1 
            + stnDAM_strong_MA5*creditMA5_L1
            + factor(ISO), family = binomial(), MyData)
summary(T2M4)

T2M5 <- lm(crisisJST ~ stnDAM_strong_MA5 + creditMA5_L1 
           + factor(ISO)+ factor(year), MyData)
summary(T2M5)

T2M6 <- lm(crisisJST ~ stnDAM_strong_MA5 + creditMA5_L1 
            + stnDAM_strong_MA5*creditMA5_L1 
            + factor(ISO)+ factor(year), MyData)
summary(T2M6)

# Cluster robust standard errors at the country cluster level for each model
cluster_se_T2M1 <- sqrt(diag(cluster.vcov(T2M1, MyData$ISO)))
cluster_se_T2M2 <- sqrt(diag(cluster.vcov(T2M2, MyData$ISO)))
cluster_se_T2M3 <- sqrt(diag(cluster.vcov(T2M3, MyData$ISO)))
cluster_se_T2M4 <- sqrt(diag(cluster.vcov(T2M4, MyData$ISO)))
cluster_se_T2M5 <- sqrt(diag(cluster.vcov(T2M5, MyData$ISO)))
cluster_se_T2M6 <- sqrt(diag(cluster.vcov(T2M6, MyData$ISO)))


stargazer(T2M1,T2M2,T2M3,T2M4,T2M5,T2M6,
          title = 'Interaction credit expansions and natural disasters (1900 - 2020)',
          label='T2', type = 'latex', 
          out = 'tables/T2.tex',

          font.size = "footnotesize", no.space = TRUE, 
          #stargazer_style_list = "aer", 
          multicolumn = TRUE,
          align = TRUE, df = FALSE, omit.stat = c("rsq", "aic"), notes.align = "l",
          #column.sep.width = "",
          
          
          dep.var.caption = "Dependent Variable: Crisis in $year = t$ ",
          dep.var.labels = "",
          omit.table.layout = "d",
          
          omit=c("factor","gdp"),
          se = list(cluster_se_T2M1,cluster_se_T2M2,cluster_se_T2M3,cluster_se_T2M4,cluster_se_T2M5,cluster_se_T2M6),
          
          covariate.labels=c("$Damages (pct GDP)_{t}$",
                             "$Damages (pct GDP)_{t-1}$",
                             "$Damages (pct GDP)_{t-2}$",
                             "$Damages (pct GDP)_{t-3}$",
                             "$\\Delta Credit_{t-1}$",
                             "$\\Delta Credit_{t-2}$",
                             "$\\Delta Credit_{t-3}$",
                             "$\\Delta Credit_{t-4}$",
                             "Damages 5yr ma",
                             "Credit 5yr ma",
                             "$Damages_{t-2} \\times \\Delta Credit_{t-2}$",
                             "Damages ma $\\times \\Delta Credit$ ma"),
          
          add.lines = list(c("Country fixed effects","Yes","Yes","Yes","Yes","Yes","Yes"),
                           c("Year fixed effects", "No", "No","No", "No","Yes","Yes")),
          
          notes = "Cluster-robust standard errors are shown in parantheses."
          
          #apply.coef = exp,
)

#Test for heteroskedasticity
bptest(T2M1)
bptest(T2M2)
bptest(T2M3)
bptest(T2M4)
bptest(T2M5)
bptest(T2M6)


ncvTest(T2M5)
ncvTest(T2M6)

# Calculate robust standard errors for all specifications
robust_se_T2M1 <- sqrt(diag(vcovHC(T2M1, type = "HC1")))
robust_se_T2M2 <- sqrt(diag(vcovHC(T2M2, type = "HC1")))
robust_se_T2M3 <- sqrt(diag(vcovHC(T2M3, type = "HC1")))
robust_se_T2M4 <- sqrt(diag(vcovHC(T2M4, type = "HC1")))
robust_se_T2M5 <- sqrt(diag(vcovHC(T2M5, type = "HC1")))
robust_se_T2M6 <- sqrt(diag(vcovHC(T2M6, type = "HC1")))

stargazer(T2M1,T2M2,T2M3,T2M4,T2M5,T2M6,
          title = 'Interaction credit expansions and natural disasters (1900 - 2020)',
          label='TA2_1', type = 'latex', 
          out = 'tables/TA2_1.tex',
          
          font.size = "footnotesize", no.space = TRUE, 
          #stargazer_style_list = "aer", 
          multicolumn = TRUE,
          align = TRUE, df = FALSE, omit.stat = c("rsq", "aic"), notes.align = "l",
          #column.sep.width = "",
          
          
          dep.var.caption = "Dependent Variable: Crisis in $year = t$ ",
          dep.var.labels = "",
          omit.table.layout = "d",
          
          omit=c("factor","gdp"),
          se = 
            list(robust_se_T2M1, robust_se_T2M2, robust_se_T2M3, robust_se_T2M4, robust_se_T2M5,robust_se_T2M6),

          covariate.labels=c("$Damages (pct GDP)_{t}$",
                             "$Damages (pct GDP)_{t-1}$",
                             "$Damages (pct GDP)_{t-2}$",
                             "$Damages (pct GDP)_{t-3}$",
                             "$\\Delta Credit_{t-1}$",
                             "$\\Delta Credit_{t-2}$",
                             "$\\Delta Credit_{t-3}$",
                             "$\\Delta Credit_{t-4}$",
                             "Damages 5yr ma",
                             "Credit 5yr ma",
                             "$Damages_{t-2} \\times \\Delta Credit_{t-2}$",
                             "Damages ma $\\times \\Delta Credit$ ma"),
          
          add.lines = list(c("Country fixed effects","Yes","Yes","Yes","Yes","Yes","Yes"),
                           c("Year fixed effects", "No", "No","No", "No","Yes","Yes")),
          
          notes = "Heteroskedasticity-robust standard errors are shown in parentheses."
          
          #apply.coef = exp,
)

MyData$rlrevenue<-log(MyData$revenue/MyData$cpi)
for (i in 0:6) {
  MyData[,paste('revL',i-1,sep="")]<- lag(MyData[,paste('rlrevenue')], i-1)-lag(MyData[,paste('rlrevenue')], i)
}

MyData$rlgdp<-log(MyData$gdp/MyData$cpi)
for (i in -3:6) {
  MyData[,paste('gdpL',i-1,sep="")]<- lag(MyData[,paste('rlgdp')], i-1)-lag(MyData[,paste('rlgdp')], i)
}

MyData$rGDP<-log(MyData$revenue/MyData$cpi)
for (i in 0:6) {
  MyData[,paste('revL',i-1,sep="")]<- lag(MyData[,paste('rlrevenue')], i-1)-lag(MyData[,paste('rlrevenue')], i)
}

MyData <- lagvar(iy, 5, MyData)
MyData <- lagvar(revenueMA5, 1,MyData)


MyData$r_eq_capgain <- MyData$eq_capgain /MyData$cpi

MyData$gdpMA <- (MyData$gdpL1 + MyData$gdpL2 + MyData$gdpL3 + MyData$gdpL4 + MyData$gdpL5)/5

MyData$gdpMA_center <- (MyData$`gdpL-2`+ MyData$`gdpL-1` + MyData$gdpL0 + MyData$gdpL1 + MyData$gdpL2)/5

colnames(MyData)

MyData <- lagvar(revenueMA5, 1, MyData)
MyData <- lagvar(capgainMA5, 1, MyData)
MyData <- lagvar(expMA5, 1, MyData)





T2M1 <- glm(crisisJST ~ stnDAM_strong +stnDAM_strong_L1 + stnDAM_strong_L2 + stnDAM_strong_L3
            + creditL1 + creditL2 + creditL3 + creditL4 
            + gdpL1 + gdpL2 + gdpL3 + gdpL4 
            + factor(ISO), family = binomial(), MyData)
summary(T2M1)

T2M2 <- glm(crisisJST ~ stnDAM_strong +stnDAM_strong_L1 + stnDAM_strong_L2 + stnDAM_strong_L3 
            + creditL1 + creditL2 + creditL3 + creditL4 + gdpL1 + gdpL2 + gdpL3 + gdpL4 
            + stnDAM_strong_L2*creditL2
            + factor(ISO), family = binomial(), MyData)
summary(T2M2)

T2M3 <- glm(crisisJST ~ stnDAM_strong_MA5 + creditMA5_L1 
            +gdpMA
            + factor(ISO), family = binomial(), MyData)
summary(T2M3)

T2M4 <- glm(crisisJST ~ stnDAM_strong_MA5 + creditMA5_L1 
            + stnDAM_strong_MA5*creditMA5_L1 
            +gdpMA
            + factor(ISO), family = binomial(), MyData)
summary(T2M4)

T2M5 <- lm(crisisJST ~ stnDAM_strong_MA5 + creditMA5_L1 +gdpMA
           + factor(ISO)+ factor(year), MyData)
summary(T2M5)

T2M6 <- lm(crisisJST ~ stnDAM_strong_MA5 + creditMA5_L1 +gdpMA
           + stnDAM_strong_MA5*creditMA5_L1 
           + factor(ISO)+ factor(year), MyData)
summary(T2M6)


# Cluster standard errors at the country level for each model
cluster_se_T2M1 <- sqrt(diag(cluster.vcov(T2M1, MyData$ISO)))
cluster_se_T2M2 <- sqrt(diag(cluster.vcov(T2M2, MyData$ISO)))
cluster_se_T2M3 <- sqrt(diag(cluster.vcov(T2M3, MyData$ISO)))
cluster_se_T2M4 <- sqrt(diag(cluster.vcov(T2M4, MyData$ISO)))
cluster_se_T2M5 <- sqrt(diag(cluster.vcov(T2M5, MyData$ISO)))
cluster_se_T2M6 <- sqrt(diag(cluster.vcov(T2M6, MyData$ISO)))

# Calculate robust standard errors for all specifications
robust_se_T2M1 <- sqrt(diag(vcovHC(T2M1, type = "HC1")))
robust_se_T2M2 <- sqrt(diag(vcovHC(T2M2, type = "HC1")))
robust_se_T2M3 <- sqrt(diag(vcovHC(T2M3, type = "HC1")))
robust_se_T2M4 <- sqrt(diag(vcovHC(T2M4, type = "HC1")))
robust_se_T2M5 <- sqrt(diag(vcovHC(T2M5, type = "HC1")))
robust_se_T2M6 <- sqrt(diag(vcovHC(T2M6, type = "HC1")))

bptest(T2M1)
bptest(T2M2)
bptest(T2M3)
bptest(T2M4)
bptest(T2M5)
bptest(T2M6)


ncvTest(T2M5)
ncvTest(T2M6)

stargazer(T2M1,T2M2,T2M3,T2M4,T2M5,T2M6,
          title = 'ANNEX: Interaction credit expansions and natural disasters with GDP controls (1900 - 2020)',
          label='TA2_2', type = 'latex',
          out = 'tables/TA2_2.tex',
          
          font.size = "footnotesize", no.space = TRUE,  
          multicolumn = TRUE,
          align = TRUE, df = FALSE, omit.stat = c("rsq", "aic"), notes.align = "l",
          #column.sep.width = "",
          
          
          dep.var.caption = "Dependent Variable: Crisis in $year = t$ ",
          dep.var.labels = "",
          omit.table.layout = "d",
          
          omit=c("factor"),
          se = 
            list(cluster_se_T2M1,cluster_se_T2M2,cluster_se_T2M3,cluster_se_T2M4,cluster_se_T2M5,cluster_se_T2M6),
          

          covariate.labels=c("$Damages (pct GDP)_{t}$",
                             "$Damages (pct GDP)_{t-1}$",
                             "$Damages (pct GDP)_{t-2}$",
                             "$Damages (pct GDP)_{t-3}$",
                             "$\\Delta Credit_{t-1}$",
                             "$\\Delta Credit_{t-2}$",
                             "$\\Delta Credit_{t-3}$",
                             "$\\Delta Credit_{t-4}$",
                             "$\\Delta GDP_{t-1}$",
                             "$\\Delta GDP_{t-2}$",
                             "$\\Delta GDP_{t-3}$",
                             "$\\Delta GDP_{t-4}$",
                             "Damages 5yr ma",
                             "$\\Delta$ Credit 5yr ma",
                             "$\\Delta$ GDP 5yr ma",
                             "$Damages_{t-2} \\times \\Delta Credit_{t-2}$",
                             "Damages ma $\\times \\Delta Credit$ ma"),
          
          add.lines = list(c("Country fixed effects","Yes","Yes","Yes","Yes","Yes","Yes"),
                           c("Year fixed effects", "No", "No","No", "No","Yes","Yes")),
          
          notes = "Cluster-robust standard errors are shown in parentheses."
          
          #apply.coef = exp,
)

## Looking at the inclusion of channel of GDP ##

T2_2M1 <- glm(crisisJST ~ stnDAM_strong_MA5 + creditMA5_L1 
            +gdpMA
            + factor(ISO), family = binomial(), MyData)
summary(T2_2M1)

T2_2M2 <- glm(crisisJST ~ stnDAM_strong_MA5 + creditMA5_L1 
            + stnDAM_strong_MA5*creditMA5_L1 
            +gdpMA
            + factor(ISO), family = binomial(), MyData)
summary(T2_2M2)

T2_2M3 <- lm(gdpMA_center ~ stnDAM_strong_MA5 
            + factor(ISO) + factor(year), MyData)
summary(T2_2M3)

# Cluster robust standard errors at the country cluster level for each model
cluster_se_T2_2M1 <- sqrt(diag(cluster.vcov(T2_2M1, MyData$ISO)))
cluster_se_T2_2M2 <- sqrt(diag(cluster.vcov(T2_2M2, MyData$ISO)))
cluster_se_T2_2M3 <- sqrt(diag(cluster.vcov(T2_2M3, MyData$ISO)))

stargazer(T2_2M1,T2_2M2,T2_2M3,
          title = 'Controlling for economic growth',
          label='T2_gdp', type = 'latex', 
          out = 'tables/T2_gdp.tex',
          
          font.size = "footnotesize", no.space = TRUE, 
          multicolumn = TRUE,
          align = TRUE, df = FALSE, omit.stat = c("rsq", "aic"), notes.align = "l",
          
          
          #dep.var.caption = "",
          dep.var.labels = c("Crisis in $year = t","Crisis in $year = t", "$\\Delta$ GDP 5yr ma"),
          omit.table.layout = "d",
          
          omit=c("factor"),
          se = list(cluster_se_T2_2M1, cluster_se_T2_2M2, cluster_se_T2_2M3),
          
          covariate.labels=c("Damages 5yr ma",
                             "Credit 5yr ma",
                             "$\\Delta$ GDP ma",
                             "Damages ma $\\times \\Delta Credit$ ma"),
          
          add.lines = list(c("Country fixed effects","Yes","Yes","Yes"),
                           c("Year fixed effects", "No", "No","Yes")),
          
          notes = "Cluster-robust standard errors are shown in parentheses."
          
          #apply.coef = exp,
)

