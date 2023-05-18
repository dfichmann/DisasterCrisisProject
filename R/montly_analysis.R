#Monthly Analysis!! 

# Loading the crisis data: BVX
BVXmonthly <- read_dta("data/master_data_monthly.dta") # Crisis data in the folder
# Transform into year and month variables
BVXmonthly$year_variable <- as.integer(substring(BVXmonthly$date, 1, 4))
BVXmonthly$month_variable <- as.integer(substring(BVXmonthly$date, 6))
# Transform into date variable
BVXmonthly$date <- as.Date(paste(BVXmonthly$year_variable, BVXmonthly$month_variable, "1", sep = "-"), format = "%Y-%m-%d")
BVXmonthly <- BVXmonthly %>%
  mutate(country = ifelse(country == "Czech", "Czech Republic", country))
# Convert to ISO codes
BVXmonthly$ISO <- countrycode(BVXmonthly$country, "country.name", "iso3c")
BVXmonthly <- subset(BVXmonthly, select = c("sB", "sC", "rN", "rB", "date", "year_variable", "month_variable", "ISO"))
# Convert date to year and month format
BVXmonthly$year_month <- format(BVXmonthly$date, "%Y-%m")

#Base Panel
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
panel_data <- panel_data[, c("ISO", "year_month", "Yr", "Mo")]

# Merge with Base Panel
MyMonthlyData <- merge(panel_data, BVXmonthly, by = c("ISO", "year_month"), all.x = TRUE)
# Rename specific columns in MyMonthlyData
colnames(MyMonthlyData) <- c("ISO", "year_month", "Yr", "Mo", "bank_credit_spread", "corporate_credit_spread",
                             "Nonfinancial_eq_return", "bank_eq_return", "date", "year_variable", "month_variable")



# Calculating the country specific median
EmDat <- EmDat %>%
  group_by(ISO) %>%
  mutate(median_damage = median(TotalDamagesAdj_000US, na.rm = TRUE)) %>%
  ungroup()


### Collapsing / Summarizing


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
    filtervar,
    threshold,
    var, # the variable that is actually being summed
    value # the value to return if the filter variable is less than the median (this is set to 0 in the loop below)
  ) {
    ifelse(
      filtervar > threshold, # if the filter variable is greater than the country specific median
      var, # return the variable
      value # else return the value (0)
    )
  }
  
  # summarizing
  DisasterDF_monthly <- subset(EmDat, `DisasterType` %in% c("Earthquake", "Storm", "Volcanic activity", "Wildfire")) %>%
    group_by(year_month, ISO) %>%
    summarize(
      adj_DAM_000US = sum_if_not_all_na(TotalDamagesAdj_000US),
      DAM_000US = sum_if_not_all_na(TotalDamages_000US),
      DEATH = sum_if_not_all_na(TotalDeath),
      w_adj_DAM_000US = sum_if_not_all_na(weighted_TotalDamagesAdj_000US),
      w_DAM_000US = sum_if_not_all_na(weighted_TotalDamages_000US),
      w_DEATH = sum_if_not_all_na(weighted_TotalDeath),
      w_strong_DAM_000US = sum_if_not_all_na(calculate_strong_metric(filtervar = TotalDamagesAdj_000US, threshold = median_damage, var = weighted_TotalDamages_000US, value = 0)),
      adj_strong_DAM_000US = sum_if_not_all_na(calculate_strong_metric(filtervar = TotalDamagesAdj_000US, threshold = median_damage, var = TotalDamagesAdj_000US, value = 0)),
      strong_DAM_000US = sum_if_not_all_na(calculate_strong_metric(filtervar = TotalDamagesAdj_000US, threshold = median_damage, var = TotalDamages_000US, value = 0)),
      strong_DEATH = sum_if_not_all_na(calculate_strong_metric(filtervar = TotalDamagesAdj_000US, threshold = median_damage, var = TotalDeath, value = 0)),
      w_adj_strong_DAM_000US = sum_if_not_all_na(calculate_strong_metric(filtervar = TotalDamagesAdj_000US, threshold = median_damage, var = weighted_TotalDamages_000US, value = 0)),
      w_strong_DEATH = sum_if_not_all_na(calculate_strong_metric(filtervar = TotalDamagesAdj_000US, threshold = median_damage, var = weighted_TotalDeath, value = 0)),
      NoOfStrongDisaster = sum_if_not_all_na(calculate_strong_metric(filtervar = TotalDamagesAdj_000US, threshold = median_damage, var = 1, value = 0)),
      w_NoOfStrongDisaster = sum_if_not_all_na(calculate_strong_metric(filtervar = TotalDamagesAdj_000US, threshold = median_damage, var = mWeight, value = 0)),
      W_NoOfDis = sum_if_not_all_na(mWeight),
      NoOfDis = n(),
      .groups = "drop"
    )
  
  
  ### Merging datasets together
  MyMonthlyData <- merge(MyMonthlyData, DisasterDF_monthly, by = c("ISO", "year_month"), all.x = TRUE) # merging BVX with disaster data
  
  gdp_data2 <- gdp_data
  
  gdp_data2$Yr <- format(gdp_data2$Yr, "%Y")
  gdp_data2$Yr <- as.numeric(gdp_data2$Yr)
  
  MyMonthlyData <- merge(MyMonthlyData, gdp_data2, by = c("ISO", "Yr"), all.x = TRUE) # adding GDP data
  
  ### Making indicators:
  MyMonthlyData$DisDummy <- ifelse(MyMonthlyData$NoOfDis >= 1, 1, 0) # everything over 1 is coded as 1
  MyMonthlyData$StrongDisDummy <- ifelse(MyMonthlyData$NoOfStrongDisaster >= 1, 1, 0) # everything over 1 is coded as 1
  
  for (varname in c(
    "DisDummy",
    "StrongDisDummy",
    "w_NoOfStrongDisaster", "NoOfStrongDisaster",
    "W_NoOfDis", "NoOfDis",
    "w_adj_DAM_000US", "w_DAM_000US", "w_DEATH", "w_adj_strong_DAM_000US",
    "w_strong_DAM_000US", "w_strong_DEATH", "adj_DAM_000US", "DAM_000US", "DEATH", "adj_strong_DAM_000US",
    "strong_DAM_000US", "strong_DEATH"
  )) {
    MyMonthlyData[, paste(varname)][is.na(MyMonthlyData[, paste(varname)])] <- 0 # coding NA as 0 damage (since EmDat is a list of observations)
  }
  
  MyMonthlyData$w_strong_DAM_000US <- replace(MyMonthlyData$w_strong_DAM_000US, MyMonthlyData$w_strong_DAM_000US < 0, NA)
  
  
  #loading BVX_Monthly_List_of_Crises.xlsx
  
  BVX_Monthly_List_of_Crises <- read_excel("data/BVX_Monthly_List_of_Crises.xlsx")
  BVX_Monthly_List_of_Crises$year_month <- format(parse_date_time(BVX_Monthly_List_of_Crises$Panic_date, orders = "%B %Y"), "%Y-%m")
  BVX_Monthly_List_of_Crises$ISO <- countrycode(BVX_Monthly_List_of_Crises$Country, "country.name", "iso3c")
  BVX_Monthly_List_of_Crises <- BVX_Monthly_List_of_Crises[, c("ISO", "year_month", "Panic")]
  
  MyMonthlyData <- merge(MyMonthlyData, BVX_Monthly_List_of_Crises, by = c("ISO", "year_month"), all.x = TRUE) # adding GDP data
  
  
  # Creating Panel Data
  MyMonthlyData <- pdata.frame(MyMonthlyData, index = c("ISO", "year_month"), drop.index = FALSE, row.names = TRUE) # make panel data
  
  
  #standardize using world bank gdp data from 1960 to 2020: 
  MyMonthlyData <- lagvar(GDP_nominal_000US, 1, MyMonthlyData) #create lagged variables so that GDP is unaffected by disaster
  MyMonthlyData$stnDAM_strong2 <- (MyMonthlyData$w_strong_DAM_000US / MyMonthlyData$GDP_nominal_000US_L1)*1000 #standardize by GDP
  
  
  MyMonthlyData$Panic[is.na(MyMonthlyData$Panic)] <- 0 # 
  
  MyMonthlyData <- lagvar(stnDAM_strong2, 10, MyMonthlyData) #create lagged variables
  MyMonthlyData <- lagvar(strong_DAM_000US, 10, MyMonthlyData) #create lagged variables
  MyMonthlyData <- lagvar(DisDummy, 24, MyMonthlyData) #create lagged variables
  MyMonthlyData <- lagvar(StrongDisDummy, 24, MyMonthlyData) #create lagged variables
  
  #regressions
  
  T1M1 <- lm(Panic ~ stnDAM_strong2 +stnDAM_strong2_L1 + stnDAM_strong2_L2 + stnDAM_strong2_L3
             +stnDAM_strong2_L4 + stnDAM_strong2_L5 + stnDAM_strong2_L6
             +stnDAM_strong2_L7 + stnDAM_strong2_L8 + stnDAM_strong2_L9 + stnDAM_strong2_L10,
              MyMonthlyData) #no fixed effects
  
  summary(T1M1)
  
  
  T1M2 <- glm(Panic ~ stnDAM_strong2 +stnDAM_strong2_L1 + stnDAM_strong2_L2 + stnDAM_strong2_L3
              +stnDAM_strong2_L4 + stnDAM_strong2_L5 + stnDAM_strong2_L6
              +stnDAM_strong2_L7 + stnDAM_strong2_L8 + stnDAM_strong2_L9 + stnDAM_strong2_L10
              + factor(ISO), family = binomial(), MyMonthlyData) #country fixed effects
  
  T1M2 <- glm(Panic ~ DisDummy +
                DisDummy_L1 + DisDummy_L2 + DisDummy_L3 + DisDummy_L4 +
                DisDummy_L5 + DisDummy_L6 + DisDummy_L7 + DisDummy_L8 +
                DisDummy_L9 + DisDummy_L10 + DisDummy_L11 + DisDummy_L12 +
                DisDummy_L13 + DisDummy_L14 + DisDummy_L15 + DisDummy_L16 +
                DisDummy_L17 + DisDummy_L18 + DisDummy_L19 + DisDummy_L20 +
                DisDummy_L21 + DisDummy_L22 + DisDummy_L23 + DisDummy_L24 +
                factor(ISO), family = binomial(), MyMonthlyData)
  
  
  T1M2 <- glm(Panic ~ StrongDisDummy +
                StrongDisDummy_L1 + StrongDisDummy_L2 + StrongDisDummy_L3 + StrongDisDummy_L4 +
                StrongDisDummy_L5 + StrongDisDummy_L6 + StrongDisDummy_L7 + StrongDisDummy_L8 +
                StrongDisDummy_L9 + StrongDisDummy_L10 + StrongDisDummy_L11 + StrongDisDummy_L12 +
                StrongDisDummy_L13 + StrongDisDummy_L14 + StrongDisDummy_L15 + StrongDisDummy_L16 +
                StrongDisDummy_L17 + StrongDisDummy_L18 + StrongDisDummy_L19 + StrongDisDummy_L20 +
                StrongDisDummy_L21 + StrongDisDummy_L22 + StrongDisDummy_L23 + StrongDisDummy_L24 +
                factor(ISO), family = binomial(), MyMonthlyData)
  
  summary(T1M2)
  
  
  
  
  
  
MyData_subset <- MyData[, c("stnDAM_strong", "stnDAM_strong_L1", "stnDAM_strong_L2", "stnDAM_strong_L3")]
  
  
MyData_subset <- na.omit(MyData_subset)  # Replace "your_data" with the name of your data frame
  
  # Compute the correlation matrix
  cor_matrix <- cor(MyData_subset)
  
  # Print the correlation matrix
  print(cor_matrix)
  
  
  
  
  
  
  
  
  
  lagged_vars <- paste0("StrongDisDummy_L", 1:24)
  lagged_vars <- paste0("DisDummy_L", 1:24)
  
  # Regression formula with lagged variables
  formula <- as.formula(paste("corporate_credit_spread ~ StrongDisDummy +", paste(lagged_vars, collapse = " + "), "+ factor(ISO) + factor(Yr)"))
  # Fit the linear regression model
  T1M1 <- lm(formula, MyMonthlyData)
  summary(T1M1)
  
  # Regression formula with lagged variables
  formula <- as.formula(paste("Nonfinancial_eq_return ~ DisDummy +", paste(lagged_vars, collapse = " + "), "+ factor(ISO) + factor(Yr)"))
  # Fit the linear regression model
  T1M1 <- lm(formula, MyMonthlyData)
  summary(T1M1)
  
  MyMonthlyData <- lagvar(DisDummy, 24, MyMonthlyData) #create lagged variables
  
  
  T1M1 <- lm(corporate_credit_spread ~ StrongDisDummy +StrongDisDummy_L1 + StrongDisDummy_L2 + StrongDisDummy_L3 + ... + StrongDisDummy_L24
             + factor(ISO)+ factor(Yr), MyMonthlyData) 
  summary(T1M1)
  
  "bank_credit_spread"      "corporate_credit_spread" "Nonfinancial_eq_return"  "bank_eq_return"
  
  
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
  
  
  
  
  
  
  
  
  
  
  
  MyMonthlyData <- lagvar(stnDAM_strong2, 4, MyMonthlyData) #create lagged variables
  MyMonthlyData <- lagvar(strong_DAM_000US, 4, MyMonthlyData) #create lagged variables
  
  #regressions
  
  T1M1 <- glm(RC ~ stnDAM_strong2 +stnDAM_strong2_L1 + stnDAM_strong2_L2 + stnDAM_strong2_L3,
              family = binomial(), MyMonthlyData) #no fixed effects
  
  T1M2 <- glm(RC ~ stnDAM_strong2 +stnDAM_strong2_L1 + stnDAM_strong2_L2 + stnDAM_strong2_L3
              + factor(ISO), family = binomial(), MyMonthlyData) #country fixed effects
  
  
  
  

