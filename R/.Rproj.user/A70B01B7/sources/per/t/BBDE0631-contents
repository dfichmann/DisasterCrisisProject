MyData_robustcheck <- lagvar(stnDAM_strong, 4, MyData_robustcheck) #lagged up to 4 years

T1M2 <- glm(crisisJST ~ stnDAM_strong +stnDAM_strong_L1 + stnDAM_strong_L2 + stnDAM_strong_L3 
            + factor(ISO), family = binomial(), MyData_robustcheck) #country fixed effects
summary(T1M2)

T1M3 <- glm(crisisJST ~ stnDAM_strong +stnDAM_strong_L1 + stnDAM_strong_L2 + stnDAM_strong_L3 
            + factor(ISO), family = binomial(), filter(MyData_robustcheck, year >= 1950)) #restricted sample (1950-2020)
summary(T1M3)

cluster_se_T1M2 <- sqrt(diag(cluster.vcov(T1M2, MyData$ISO))) #clustered at the country level
cluster_se_T1M3 <- sqrt(diag(cluster.vcov(T1M3, filter(MyData, year >= 1950)$ISO))) #clustered at the country level


###### Creating log change real credit ####
MyData_robustcheck$rloans<-log(MyData_robustcheck$tloans/MyData_robustcheck$cpi)
for (i in 0:6) {
  MyData_robustcheck[,paste('creditL',i-1,sep="")]<- lag(MyData_robustcheck[,paste('rloans')], i-1)-lag(MyData_robustcheck[,paste('rloans')], i)
}

MyData_robustcheck <- lagvar(creditMA5, 1, MyData_robustcheck) #lag moving average to make so that it is only calculated using past observations


MyData_robustcheck$stnDAM_strong_MA5 <- (MyData_robustcheck$stnDAM_strong_L1 + MyData_robustcheck$stnDAM_strong_L2 + MyData_robustcheck$stnDAM_strong_L3)/3


T2M3 <- glm(crisisJST ~ stnDAM_strong_MA5 + creditMA5_L1
            + factor(ISO), family = binomial(), MyData_robustcheck)
summary(T2M3)

T2M4 <- glm(crisisJST ~ stnDAM_strong_MA5 + creditMA5_L1 
            + stnDAM_strong_MA5*creditMA5_L1
            + factor(ISO), family = binomial(), MyData_robustcheck)
summary(T2M4)

# Cluster robust standard errors at the country cluster level for each model
cluster_se_T2M3 <- sqrt(diag(cluster.vcov(T2M3, MyData$ISO)))
cluster_se_T2M4 <- sqrt(diag(cluster.vcov(T2M4, MyData$ISO)))

stargazer(T1M2,T1M3,T2M3,T2M4,
          title = 'Main results including other types of disasters',
          label='TA_alltypes.tex', type = 'latex', 
          out = 'tables/TA_alltypes.tex',
          
          font.size = "footnotesize", no.space = TRUE, 
          #stargazer_style_list = "aer", 
          multicolumn = TRUE,
          align = TRUE, df = FALSE, omit.stat = c("rsq", "aic"), notes.align = "l",
          #column.sep.width = "",
          
          
          dep.var.caption = "Dependent Variable: Crisis in $year = t$ ",
          dep.var.labels = "",
          omit.table.layout = "d",
          
          omit=c("factor","gdp"),
          se = list(cluster_se_T1M2,cluster_se_T1M3,cluster_se_T2M3,cluster_se_T2M4),
          
          covariate.labels=c("$Damages (pct GDP)_{t}$",
                             "$Damages (pct GDP)_{t-1}$",
                             "$Damages (pct GDP)_{t-2}$",
                             "$Damages (pct GDP)_{t-3}$",
                             "Damages 5yr ma",
                             "Credit 5yr ma",
                             "Damages ma $\\times \\Delta Credit$ ma"),
          
          add.lines = list(c("Country fixed effects","Yes","Yes","Yes","Yes"),
                           c("Year fixed effects", "No", "No","No", "No"),
                           c("Restricted sample", "No", "Yes"," No","No")),
          
          notes = "Cluster-robust standard errors are shown in parantheses."
          
          #apply.coef = exp,
)
