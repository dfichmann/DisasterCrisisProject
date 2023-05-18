# SUM STATS TABLE ##########

# Select the relevant columns from the MyData dataframe
MyData_summarySTAT <- MyData %>%
  dplyr::select(crisisJST, stnDAM, stnDAM_strong, stnDAM_strong_uw, adj_DAM_000US, adj_strong_DAM_000US, creditL0, stnDAM_strong_MA5, creditMA5)

MyData_summarySTAT$Strong_Dis_ind <- ifelse(MyData_summarySTAT$adj_strong_DAM_000US > 0, 1, MyData_summarySTAT$adj_strong_DAM_000US)
MyData_summarySTAT$Strong_Dis_ind <- ifelse(is.na(MyData_summarySTAT$crisisJST) == FALSE, MyData_summarySTAT$Strong_Dis_ind, NA)

MyData_summarySTAT$Dis_ind <- ifelse(MyData_summarySTAT$adj_DAM_000US > 0, 1, MyData_summarySTAT$adj_DAM_000US)
MyData_summarySTAT$Dis_ind <- ifelse(is.na(MyData_summarySTAT$crisisJST) == FALSE, MyData_summarySTAT$Dis_ind, NA)

MyData_summarySTAT$stnDAM_strong <- ifelse(MyData_summarySTAT$stnDAM_strong == 0, NA, MyData_summarySTAT$stnDAM_strong/10)
MyData_summarySTAT$stnDAM <- ifelse(MyData_summarySTAT$stnDAM == 0, NA, MyData_summarySTAT$stnDAM/10)
MyData_summarySTAT$stnDAM_strong_uw <- ifelse(MyData_summarySTAT$stnDAM_strong_uw == 0, NA, MyData_summarySTAT$stnDAM_strong_uw/10)

MyData_summarySTAT$adj_DAM_000US <- ifelse(MyData_summarySTAT$adj_DAM_000US == 0 | is.na(MyData_summarySTAT$stnDAM_strong) == FALSE, NA, MyData_summarySTAT$adj_DAM_000US/1000)
MyData_summarySTAT$adj_strong_DAM_000US <- ifelse(MyData_summarySTAT$adj_strong_DAM_000US == 0 | is.na(MyData_summarySTAT$stnDAM_strong) == FALSE, NA, MyData_summarySTAT$adj_strong_DAM_000US/1000)

colnames(MyData_summarySTAT)

# Reorder variables
MyData_summarySTAT <- MyData_summarySTAT %>%
  dplyr::select(crisisJST, Dis_ind, Strong_Dis_ind, Strong_Dis_ind, adj_DAM_000US, adj_strong_DAM_000US, stnDAM, stnDAM_strong, stnDAM_strong_uw, stnDAM_strong_MA5, creditL0, creditMA5)

names(MyData_summarySTAT) <- c("Crisis Indicator (JST)", "Disaster Indicator", "Strong Disaster Indicator", "Damages (in millions US)", "Strong Damages (in millions US)", "Weighted Damages (pc GDP)" , "Disaster Measure (pc GDP)",       
                               "Strng Disaster Measure (pc GDP)", "Strng Disaster Measure 5yr ma (pc GDP) ", "Log change in credit", "Log change in credit 5yr ma")

# Generate summary statistics and a LaTeX table using stargazer
stargazer(MyData_summarySTAT, type = "latex",
          summary.stat = c("n", "mean", "median","p75", "min", "max", "sd"),
          title = "Summary Statistics",
          header = FALSE, out = 'tables/T0_summarystat1.tex',label='sumtab', font.size = "footnotesize")
