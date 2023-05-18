#cool functions 


SumDisData <- function(data, group_vars, summarize_vars, summarize_type = "sum") {
  
  if (!is.data.frame(data)) {
    stop("Input data is not a data frame.")
  }
  
  if (!is.character(group_vars)) {
    stop("Group variables must be specified as a character vector.")
  }
  
  if (!is.character(summarize_vars)) {
    stop("Summarize variables must be specified as a character vector.")
  }
  
  if (!is.character(summarize_type) | !summarize_type %in% c("sum", "count")) {
    stop("Summarize type must be either 'sum' or 'count'.")
  }
  
  if (summarize_type == "sum") {
    summarizing_function <- sum
  } else if (summarize_type == "count") {
    summarizing_function <- n
  }
  
  summarized_data <- data %>% 
    group_by(across(all_of(group_vars))) %>%
    summarize(across(all_of(summarize_vars), summarizing_function))
  
  return(summarized_data)
}

test1 <- SumDisData(data = subset(EmDat, `DisasterType` %in% disasterlist),
                    group_vars = c("year_month", "ISO"),
                    summarize_vars = c("TotalDamagesAdj_000US", "TotalDamages_000US", "TotalDeath", "weighted_TotalDamagesAdj_000US", "weighted_TotalDamages_000US", "weighted_TotalDeath"),
                    summarize_type = "sum")





#DisasterDF <- subset(EmDat,`DisasterType` %in% d_types) %>% 
#  group_by(Yr,ISO) %>%  # grouping over year and country
#  summarize(adj_DAM_000US = sum(TotalDamagesAdj_000US, na.rm = TRUE),
#            DAM_000US = sum(TotalDamages_000US, na.rm = TRUE),
#            DEATH = sum(TotalDeath, na.rm = TRUE),
#            w_adj_DAM_000US = sum(weighted_TotalDamagesAdj_000US, na.rm = TRUE),
#            w_DAM_000US = sum(weighted_TotalDamages_000US, na.rm = TRUE),
#            w_DEATH = sum(weighted_TotalDeath, na.rm = TRUE),
#            NoOfDis = n(),
#            .groups ='drop')

#DisasterDF <- subset(EmDat,`DisasterType` %in% d_types) %>% 
#  group_by(Yr,ISO) %>%  
#  summarize(adj_DAM_000US = ifelse(all(is.na(TotalDamagesAdj_000US)), NA, sum(TotalDamagesAdj_000US, na.rm = TRUE)),
#            DAM_000US = ifelse(all(is.na(TotalDamages_000US)), NA, sum(TotalDamages_000US, na.rm = TRUE)),
#            DEATH = ifelse(all(is.na(TotalDeath)), NA, sum(TotalDeath, na.rm = TRUE)),
#            w_adj_DAM_000US = ifelse(all(is.na(weighted_TotalDamagesAdj_000US)), NA, sum(weighted_TotalDamagesAdj_000US, na.rm = TRUE)),
#            w_DAM_000US = ifelse(all(is.na(weighted_TotalDamages_000US)), NA, sum(weighted_TotalDamages_000US, na.rm = TRUE)),
#            w_DEATH = ifelse(all(is.na(weighted_TotalDeath)), NA, sum(weighted_TotalDeath, na.rm = TRUE)),
#            w_strong_DAM_000US = ifelse(all(is.na(weighted_TotalDamages_000US)), NA,
#                                        sum(ifelse(TotalDamagesAdj_000US > quantile(TotalDamagesAdj_000US, 0.5, na.rm=TRUE), weighted_TotalDamages_000US,0), na.rm = TRUE )),
#            adj_strong_DAM_000US = ifelse(all(is.na(weighted_TotalDamages_000US)), NA, 
#                                        sum(ifelse(TotalDamagesAdj_000US > quantile(TotalDamagesAdj_000US, 0.5, na.rm=TRUE), TotalDamagesAdj_000US,0), na.rm = TRUE )),
#            strong_DAM_000US = ifelse(all(is.na(weighted_TotalDamages_000US)), NA, 
#                                        sum(ifelse(TotalDamagesAdj_000US > quantile(TotalDamagesAdj_000US, 0.5, na.rm=TRUE), TotalDamages_000US,0), na.rm = TRUE )),
#            strong_DEATH = ifelse(all(is.na(weighted_TotalDamages_000US)), NA, 
#                                        sum(ifelse(TotalDamagesAdj_000US > quantile(TotalDamagesAdj_000US, 0.5, na.rm=TRUE), TotalDeath,0), na.rm = TRUE )),
#            w_adj_strong_DAM_000US = ifelse(all(is.na(weighted_TotalDamages_000US)), NA,
#                                        sum(ifelse(TotalDamagesAdj_000US > quantile(TotalDamagesAdj_000US, 0.5, na.rm=TRUE), weighted_TotalDamages_000US,0), na.rm = TRUE )),
#            w_strong_DEATH = ifelse(all(is.na(weighted_TotalDamages_000US)), NA,
#                                        sum(ifelse(TotalDamagesAdj_000US > quantile(TotalDamagesAdj_000US, 0.5, na.rm=TRUE), weighted_TotalDeath,0), na.rm = TRUE )),
#            
#            NoOfDis = n(),
#            .groups ='drop')




#addstrong <- subset(EmDat_Strong,`DisasterType` %in% d_types) %>%  
#  group_by(Yr, ISO) %>%
#  summarize(adj_strong_DAM_000US = sum(TotalDamagesAdj_000US, na.rm = TRUE),
#            strong_DAM_000US = sum(TotalDamages_000US, na.rm = TRUE),
#            strong_DEATH = sum(TotalDeath, na.rm = TRUE),
#            w_adj_strong_DAM_000US = sum(weighted_TotalDamagesAdj_000US, na.rm = TRUE),
#            w_strong_DAM_000US = sum(weighted_TotalDamages_000US, na.rm = TRUE),
#            w_strong_DEATH = sum(weighted_TotalDeath, na.rm = TRUE),
#            NoOfStrongDis = n(),
#            .groups ='drop')

#addstrong <- subset(EmDat_Strong,`DisasterType` %in% d_types) %>%  
#  group_by(Yr, ISO) %>%
#  summarize(NoOfStrongDis = n(),
#            adj_strong_DAM_000US = ifelse(all(is.na(TotalDamagesAdj_000US)), NA, sum(TotalDamagesAdj_000US, na.rm = TRUE)),
#            strong_DAM_000US = ifelse(all(is.na(TotalDamages_000US)), NA, sum(TotalDamages_000US, na.rm = TRUE)),
#            strong_DEATH = ifelse(all(is.na(TotalDeath)), NA, sum(TotalDeath, na.rm = TRUE)),
#            w_adj_strong_DAM_000US = ifelse(all(is.na(weighted_TotalDamagesAdj_000US)), NA, sum(weighted_TotalDamagesAdj_000US, na.rm = TRUE)),
#            w_strong_DAM_000US = ifelse(all(is.na(weighted_TotalDamages_000US)), NA, sum(weighted_TotalDamages_000US, na.rm = TRUE)),
#            w_strong_DEATH = ifelse(all(is.na(weighted_TotalDeath)), NA, sum(weighted_TotalDeath, na.rm = TRUE)),
#            .groups ='drop')
