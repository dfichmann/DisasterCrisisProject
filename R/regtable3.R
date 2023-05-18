#coding a crisis "danger zone"
MyData$crisisJST1 <- lead(MyData$crisisJST, 1) #coding dangerzone   
MyData$crisisJST2 <- lead(MyData$crisisJST, 2) #coding dangerzone
MyData$crisisJST3 <- lead(MyData$crisisJST, 3) #coding dangerzone

MyData$dangerzoneJST <- MyData$crisisJST + MyData$crisisJST1 + MyData$crisisJST2 #coding dangerzone

#regressing with "danger zone" 

T3M1 <- glm(dangerzoneJST ~ stnDAM_strong_L1 + creditL1 + gdpL1
            + factor(ISO), family = binomial(), filter(MyData, year >= 1900)) 
summary(T3M1)


T3M2 <- glm(dangerzoneJST ~ stnDAM_strong_L1 + creditL1 + gdpL1
            + stnDAM_strong_L1*creditL1 
            + factor(ISO), family = binomial(), filter(MyData, year >= 1900))
summary(T3M2)

T3M3 <- lm(dangerzoneJST ~ stnDAM_strong_L1 + creditL1 + gdpL1
           + factor(ISO) + factor(year), filter(MyData, year >= 1900))
summary(T3M3)

T3M4 <- lm(dangerzoneJST ~ stnDAM_strong_L1 + creditL1 + gdpL1
           + stnDAM_strong_L1*creditL1 
           + factor(ISO) + factor(year), filter(MyData, year >= 1900))
summary(T3M4)


# Calculate robust standard errors for all specifications
# Cluster standard errors at the country level for each model
cluster_se_T3M1 <- sqrt(diag(cluster.vcov(T3M1, filter(MyData, year >= 1900)$ISO)))
cluster_se_T3M2 <- sqrt(diag(cluster.vcov(T3M2, filter(MyData, year >= 1900)$ISO)))
cluster_se_T3M3 <- sqrt(diag(cluster.vcov(T3M3, filter(MyData, year >= 1900)$ISO)))
cluster_se_T3M4 <- sqrt(diag(cluster.vcov(T3M4, filter(MyData, year >= 1900)$ISO)))

robust_se_T3M1 <- sqrt(diag(vcovHC(T3M1, type = "HC1")))
robust_se_T3M2 <- sqrt(diag(vcovHC(T3M2, type = "HC1")))
robust_se_T3M3 <- sqrt(diag(vcovHC(T3M3, type = "HC1")))
robust_se_T3M4 <- sqrt(diag(vcovHC(T3M4, type = "HC1")))

stargazer(T3M1,T3M2,T3M3,T3M4,
          title = 'Effect on a Crisis \"Danger Zone\" (1900 - 2020)',
          label='T3', type = 'latex',
          out = 'tables/T3.tex',
          
          font.size = "footnotesize", no.space = TRUE, multicolumn = TRUE,
          align = TRUE, df = FALSE, omit.stat = c("rsq", "aic"), notes.align = "l",
          column.sep.width = "-5pt",
          
          
          dep.var.caption = "Dependent Variable: Crisis Danger Zone ",
          dep.var.labels = "",
          omit.table.layout = "d",
          
          omit=c("factor"),
          se = list(cluster_se_T3M1,cluster_se_T3M2,cluster_se_T3M3,cluster_se_T3M4),
          
          covariate.labels=c("$Damages (pc GDP)_{t-1}$",
                             "$\\Delta Credit_{t-1}$",
                             "$\\Delta GDP_{t-1}$",
                             "$Damages_{t-1} \\times \\Delta Credit_{t-1}$"),
          
          add.lines = list(c("Country fixed effects","Yes","Yes","Yes","Yes"),
                           c("Year fixed effects", "No", "No","Yes","Yes"),
                           c("Restricted sample", "No", "No"," No","No")),
          
          notes = "Cluster-robust standard errors are shown in parentheses."
          
          #apply.coef = exp,
)

#Table with heteroskedasticity-robust standard errors

stargazer(T3M1,T3M2,T3M3,T3M4,
          title = 'Robust: Effect on a Crisis \"Danger Zone\" (1900 - 2020)',
          label='TA3_1', type = 'latex',
          out = 'tables/TA3_1.tex',
          
          font.size = "footnotesize", no.space = TRUE, multicolumn = TRUE,
          align = TRUE, df = FALSE, omit.stat = c("rsq", "aic"), notes.align = "l",
          column.sep.width = "-5pt",
          
          
          dep.var.caption = "Dependent Variable: Crisis Danger Zone ",
          dep.var.labels = "",
          omit.table.layout = "d",
          
          omit=c("factor"),
          se = list(robust_se_T3M1, robust_se_T3M2,robust_se_T3M3,robust_se_T3M4),          
          covariate.labels=c("$Damages (pc GDP)_{t-1}$",
                             "$\\Delta Credit_{t-1}$",
                             "$\\Delta GDP Credit_{t-1}$",
                             "$Damages_{t-1} \\times \\Delta Credit_{t-1}$"),
          
          add.lines = list(c("Country fixed effects","Yes","Yes","Yes","Yes"),
                           c("Year fixed effects", "No", "No","Yes","Yes"),
                           c("Restricted sample", "No", "No"," No","No")),
          
          notes = "Heteroskedasticity-robust standard errors are shown in parentheses."
          )


# ####### OLD CODE
# 
# 
# #regressing with "danger zone" 
# 
# 3T3M1 <- glm(dangerzoneJST ~ stnDAM_strong +stnDAM_strong_L1 + stnDAM_strong_L2 + stnDAM_strong_L3 
#            + creditMA5_L1 +gdpMA
#             + factor(ISO), family = binomial(), filter(MyData, year >= 1900))
# summary(T3M1)
# 
# T3M3 <- lm(dangerzoneJST ~ stnDAM_strong +stnDAM_strong_L1 + stnDAM_strong_L2 + stnDAM_strong_L3
#            #+ creditL1 + creditL2 + creditL3 + creditL4 
#            + creditMA5_L1 + gdpMA
#            + factor(ISO) + factor(year), filter(MyData, year >= 1900))
# summary(T3M3)
# 
# T3M4 <- glm(dangerzoneJST ~ stnDAM_strong_MA5 + creditMA5_L1 + gdpMA
#             + factor(ISO), family = binomial(), filter(MyData, year >= 1900))
# summary(T3M4)
# 
# 
# T3M5 <- glm(dangerzoneJST ~ stnDAM_strong_MA5 + creditMA5_L1 + gdpMA
#             + stnDAM_strong_MA5*creditMA5_L1 
#             + factor(ISO), family = binomial(), filter(MyData, year >= 1900))
# summary(T3M5)
# 
# T3M6 <- lm(dangerzoneJST ~ stnDAM_strong_MA5 + creditMA5_L1 + gdpMA
#            + factor(ISO) + factor(year), filter(MyData, year >= 1900))
# summary(T3M6)
# 
# T3M7 <- lm(dangerzoneJST ~ stnDAM_strong_MA5 + creditMA5_L1 + gdpMA
#            + stnDAM_strong_MA5*creditMA5_L1 
#            + factor(ISO) + factor(year), filter(MyData, year >= 1900))
# summary(T3M7)
# 
# 
# # Calculate robust standard errors for all specifications
# # Cluster standard errors at the country level for each model
# cluster_se_T3M1 <- sqrt(diag(cluster.vcov(T3M1, filter(MyData, year >= 1900)$ISO)))
# 
# cluster_se_T3M3 <- sqrt(diag(cluster.vcov(T3M3, filter(MyData, year >= 1900)$ISO)))
# cluster_se_T3M4 <- sqrt(diag(cluster.vcov(T3M4, filter(MyData, year >= 1900)$ISO)))
# cluster_se_T3M5 <- sqrt(diag(cluster.vcov(T3M5, filter(MyData, year >= 1900)$ISO)))
# cluster_se_T3M6 <- sqrt(diag(cluster.vcov(T3M6, filter(MyData, year >= 1900)$ISO)))
# cluster_se_T3M7 <- sqrt(diag(cluster.vcov(T3M7, filter(MyData, year >= 1900)$ISO)))
# 
# 
# stargazer(T3M1,T3M3,T3M4,T3M5,T3M6,T3M7,
#           title = 'Dangerzone (1900 - 2020)',
#           label='T3', type = 'latex',
#           out = 'tables/T3.tex',
#           
#           font.size = "footnotesize", no.space = TRUE, multicolumn = TRUE,
#           align = TRUE, df = FALSE, omit.stat = c("rsq", "aic"), notes.align = "l",
#           column.sep.width = "-5pt",
#           
#           
#           dep.var.caption = "Dependent Variable: Crisis Danger Zone ",
#           dep.var.labels = "",
#           omit.table.layout = "d",
#           
#           omit=c("factor"),
#           se = list(cluster_se_T3M1,cluster_se_T3M3,cluster_se_T3M4,cluster_se_T3M5,cluster_se_T3M6,cluster_se_T3M7),
#           
#           covariate.labels=c("$Damages (pc GDP)_{t}$",
#                              "$Damages (pc GDP)_{t-1}$",
#                              "$Damages (pc GDP)_{t-2}$",
#                              "$Damages (pc GDP)_{t-3}$",
#                              "Damages 5yr ma",
#                              "$\\Delta$ Credit 5yr ma",
#                              "$\\Delta$ GDP 5yr ma",
#                              "Damages ma $\\times \\Delta Credit$ ma"),
#           
#           add.lines = list(c("Country fixed effects","Yes","Yes","Yes","No","Yes","Yes"),
#                            c("Year fixed effects", "No", "Yes","No","No","Yes","Yes"),
#                            c("Restricted sample", "Yes", "Yes"," Yes","Yes","Yes","Yes")),
#           
#           notes = "Cluster-robust standard errors are shown in parentheses."
#           
#           #apply.coef = exp,
# )
# 
# 
# #robustness check
# #calculating white standard errors
# robust_se_T3M1 <- sqrt(diag(vcovHC(T3M1, type = "HC1")))
# 
# robust_se_T3M3 <- sqrt(diag(vcovHC(T3M3, type = "HC1")))
# robust_se_T3M4 <- sqrt(diag(vcovHC(T3M4, type = "HC1")))
# robust_se_T3M5 <- sqrt(diag(vcovHC(T3M5, type = "HC1")))
# robust_se_T3M6 <- sqrt(diag(vcovHC(T3M6, type = "HC1")))
# robust_se_T3M7 <- sqrt(diag(vcovHC(T3M7, type = "HC1")))
# 
# stargazer(T3M1,T3M3,T3M4,T3M5,T3M6,T3M7,
#           title = 'Dangerzone (1950 - 2020)',
#           label='TA3_1', type = 'latex',
#           out = 'tables/TA3_1.tex',
#           
#           font.size = "footnotesize", no.space = TRUE, multicolumn = TRUE,
#           align = TRUE, df = FALSE, omit.stat = c("rsq", "aic"), notes.align = "l",
#           column.sep.width = "-5pt",
#           
#           
#           dep.var.caption = "Dependent Variable: Crisis Danger Zone ",
#           dep.var.labels = "",
#           omit.table.layout = "d",
#           
#           omit=c("factor"),
#           se = list(robust_se_T3M1, robust_se_T3M3, robust_se_T3M4, robust_se_T3M5,robust_se_T3M6,robust_se_T3M7),
#           
#           covariate.labels=c("$Damages (pc GDP)_{t}$",
#                              "$Damages (pc GDP)_{t-1}$",
#                              "$Damages (pc GDP)_{t-2}$",
#                              "$Damages (pc GDP)_{t-3}$",
#                              "Damages 5yr ma",
#                              "$\\Delta$ Credit 5yr ma",
#                              "$\\Delta$ GDP 5yr ma",
#                              "Damages ma $\\times \\Delta Credit$ ma"),
#           
#           add.lines = list(c("Country fixed effects","Yes","Yes","Yes","No","Yes","Yes"),
#                            c("Year fixed effects", "No", "Yes","No","No","Yes","Yes"),
#                            c("Restricted sample", "Yes", "Yes"," Yes","Yes","Yes","Yes")),
#           
#           notes = "Cluster-robust standard errors are shown in parentheses."
# )
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
