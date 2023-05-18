# Making LP graphs for GDP, credit, and return # 

# For GDP ----------------------------------------------

MyData <- lagvar(g0y,2,MyData) # lag g0y (gdp growth) by 2 years

# Scale by 100 to get pct
for (i in 0:10) {
  MyData[[paste0("g",i,"y")]] <- MyData[[paste0("g",i,"y")]] * 100
}
# Load all the outcome variables
vars <- list()
for (i in 0:5) {
  vars[[i+1]] <- MyData[[paste0("g",i,"y")]]
}

#Empty List to store regressions
reglist<-list()
#Run an LP for each outcome var and store it
for(i in 1:length(vars)){
  y<-vars[[i]]
  reglist[[i]]<-lm(y ~ stnDAM_strong + factor(ISO) + factor(as.factor(year_var)),
                   MyData)
  #Redo significance tests with clustered standard errors
  reglist[[i]]<-coeftest(reglist[[i]],
                         vcov = vcovCL(reglist[[i]],cluster = MyData$ISO)
                         #vcov=vcovHC(reglist[[i]],cluster="ISO",type="HC1")
                         #vcov = NeweyWest
                         )
  #Collect relevant coefficients for plotting.
  model <- tidy(reglist[[i]], conf.int = TRUE, conf.level = 0.9) %>%
    filter(term == "stnDAM_strong") %>%
    mutate(model = paste("h=", i-1, sep = ""))
  if (i == 1) {
    regdf <- model
  } else {
    regdf <- rbind(regdf, model)
  }
}

regdf$lp = 1 

pd <- position_dodge(0.1)
gdplp <- ggplot(regdf, aes(model, estimate, group = lp)) +
  geom_point(color = "#4B4B4B", size = 2) +
  geom_line(linetype = "solid", color = "#1D487A", size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "#377EB8", alpha = 0.2, position = pd) +
  theme_bw() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 1, color = "darkred", linetype = "dashed") +  # Change color and linetype
  annotate("text", x = 1.41, y = -0.37, label = "Disaster shock", color = "darkred", size = 4) +  # Add label
  xlab("Years after disaster") +
  ylab("Percent")+
  scale_y_continuous(labels = scales::percent_format(scale = 1), expand = c(0.05, 0.05), limits = c(-0.4, 0.1))


ggsave("plots/lp_GDP.png", plot = gdplp, width = 9, height = 5)


#-------------------------------------------------------------------------------
# For Credit to GDP ----------------------------------------------

MyData = MyData %>%
  group_by(ISO) %>%
  mutate(bvx_cred0 = 100*(dplyr::lead(credit_to_gdp,0)-dplyr::lag(credit_to_gdp,1))) %>%
  mutate(bvx_cred1 = 100*(dplyr::lead(credit_to_gdp,1)-dplyr::lag(credit_to_gdp,1))) %>%
  mutate(bvx_cred2 = 100*(dplyr::lead(credit_to_gdp,2)-dplyr::lag(credit_to_gdp,1))) %>%
  mutate(bvx_cred3 = 100*(dplyr::lead(credit_to_gdp,3)-dplyr::lag(credit_to_gdp,1))) %>%
  mutate(bvx_cred4 = 100*(dplyr::lead(credit_to_gdp,4)-dplyr::lag(credit_to_gdp,1))) %>%
  mutate(bvx_cred5 = 100*(dplyr::lead(credit_to_gdp,5)-dplyr::lag(credit_to_gdp,1))) %>%
  mutate(bvx_cred6 = 100*(dplyr::lead(credit_to_gdp,6)-dplyr::lag(credit_to_gdp,1))) %>%
  mutate(bvx_cred7 = 100*(dplyr::lead(credit_to_gdp,7)-dplyr::lag(credit_to_gdp,1))) %>%
  ungroup()

# Load all the outcome variables
vars <- list(MyData$bvx_cred0,MyData$bvx_cred1,MyData$bvx_cred2,MyData$bvx_cred3,MyData$bvx_cred4,MyData$bvx_cred5)
#List to store regressions
reglist<-list()
#Run an LP for each outcome var and store it
for(i in 1:length(vars)){
  y<-vars[[i]]
  reglist[[i]]<-lm(y ~ stnDAM_strong + g0y_L1 + factor(ISO) + factor(as.factor(year_var)),
                   MyData)
  #Redo significance tests with clustered standard errors
  reglist[[i]]<-coeftest(reglist[[i]],
                         vcov = vcovCL(reglist[[i]],cluster = MyData$ISO)
                         #vcov=vcovHC(reglist[[i]],cluster="ISO",type="HC1")
                         #vcov = NeweyWest
                         )
  #Collect relevant coefficients for plotting.
  model <- tidy(reglist[[i]], conf.int = TRUE, conf.level = 0.9) %>%
    filter(term == "stnDAM_strong") %>%
    mutate(model = paste("h=", i-1, sep = ""))
  if (i == 1) {
    regdf <- model
  } else {
    regdf <- rbind(regdf, model)
  }
}

regdf$lp = 1
# Plot
pd <- position_dodge(0.1)
gdplp <- ggplot(regdf, aes(model, estimate, group = lp)) +
  geom_point(color = "#4B4B4B", size = 2) +
  geom_line(linetype = "solid", color = "#1D487A", size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "#377EB8", alpha = 0.2, position = pd) +
  theme_bw() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 1, color = "darkred", linetype = "dashed") +  # Change color and linetype
  annotate("text", x = 1.41, y = -0.55, label = "Disaster shock", color = "darkred", size = 4) +  # Add label
  xlab("Years after disaster") +
  ylab("Percent of GDP")

ggsave("plots/lp_credit.png", plot = gdplp, width = 9, height = 4) #save it

#-------------------------------------------------------------------------------

# For Return on bank equ ----------------------------------------------
vars <- list()
for (i in 0:5) {
  vars[[i+1]] <- MyData[[paste0("g",i,"Rtot_real_w")]]
}
#List to store regressions
reglist<-list()
#Run an LP for each outcome var and store it
for(i in 1:length(vars)){
  y<-vars[[i]]
  reglist[[i]]<-lm(y ~ stnDAM_strong + g0y_L1 + factor(ISO) + factor(as.factor(year_var)),
                   MyData)
  #Redo significance tests with clustered standard errors
  reglist[[i]]<-coeftest(reglist[[i]],
                         vcov = vcovCL(reglist[[i]],cluster = MyData$ISO)
                         #vcov=vcovHC(reglist[[i]],cluster="ISO",type="HC1")
                         #vcov = NeweyWest
  )
  #Collect relevant coefficients for plotting.
  model <- tidy(reglist[[i]], conf.int = TRUE, conf.level = 0.9) %>%
    filter(term == "stnDAM_strong") %>%
    mutate(model = paste("h=", i-1, sep = ""))
  if (i == 1) {
    regdf <- model
  } else {
    regdf <- rbind(regdf, model)
  }
}

regdf$lp = 1
# Plot
pd <- position_dodge(0.1)
gdplp <- ggplot(regdf, aes(model, estimate, group = lp)) +
  geom_point(color = "#4B4B4B", size = 2) +
  geom_line(linetype = "solid", color = "#1D487A", size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "#377EB8", alpha = 0.2, position = pd) +
  theme_bw() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 1, color = "darkred", linetype = "dashed") +  # Change color and linetype
  xlab("Years after disaster") +
  ylab("Percentage Point")+
  annotate("text", x = 1.41, y = -0.023, label = "Disaster shock", color = "darkred", size = 4) +  # Add label
  scale_y_continuous(labels = scales::percent_format(scale = 100))

ggsave("plots/lp_return.png", plot = gdplp, width = 9, height = 4)


# Third, standard errors are estimated using a clustered-robust covariance matrix estimator. 
# This option allows for a completely unrestricted specification of the covariance matrix 
# of the residuals in the time series dimension by taking advantage of the cross-section. 
# This conveniently takes care of serial correlation in the residuals induced by the local
# projection setup.






#Old code for GDP

#  for (i in 0:7) {
#   MyData <- MyData %>% 
#     group_by(ISO) %>%
#     mutate(!!paste0("bvx_y", i) := 100*(dplyr::lead(log(GDP_nominal_000US), i) - dplyr::lag(log(GDP_nominal_000US), 1))) %>%
#     ungroup()
# }

# load data in var list
# vars <- list()
# for (i in 0:5) {
#   vars[[i+1]] <- MyData[[paste0("bvx_y", i)]]
# }

# vars <- list(MyData$bvx_y0,MyData$bvx_y1,MyData$bvx_y2,MyData$bvx_y3,MyData$bvx_y4,MyData$bvx_y5)


# MyData = MyData %>%
#   group_by(ISO) %>%
#   mutate(y0 = 100*(dplyr::lead(log(rGDP),0)-dplyr::lag(log(rGDP),1))) %>%
#   mutate(y1 = 100*(dplyr::lead(log(rGDP),1)-dplyr::lag(log(rGDP),1))) %>%
#   mutate(y2 = 100*(dplyr::lead(log(rGDP),2)-dplyr::lag(log(rGDP),1))) %>%
#   mutate(y3 = 100*(dplyr::lead(log(rGDP),3)-dplyr::lag(log(rGDP),1))) %>%
#   mutate(y4 = 100*(dplyr::lead(log(rGDP),4)-dplyr::lag(log(rGDP),1))) %>%
#   mutate(y5 = 100*(dplyr::lead(log(rGDP),5)-dplyr::lag(log(rGDP),1))) %>%
#   mutate(y6 = 100*(dplyr::lead(log(rGDP),6)-dplyr::lag(log(rGDP),1))) %>%
#   mutate(y7 = 100*(dplyr::lead(log(rGDP),7)-dplyr::lag(log(rGDP),1))) %>%
#   ungroup()
# 
# MyData<- lagvar(pop, 1, MyData)
# MyData$decade <- floor(MyData$year_var/10) * 10
# 
# #Run LP-IVs for each output horizon
# #List of all LHS variables
# vars <- list(MyData$y0,MyData$y1,MyData$y2,MyData$y3,MyData$y4,MyData$y5)
# #List to store regressions
# reglist<-list()
# 
# #Run an LP-IV for each LHS variable, store it
# for(i in 1:length(vars)){
#   y<-vars[[i]]
#   reglist[[i]]<-lm(y ~ stnDAM_strong +  factor(ISO) + factor(as.factor(year_var)),
#                    MyData)
#   #Redo significance tests with clustered standard errors
#   reglist[[i]]<-coeftest(reglist[[i]],vcov=vcovHC(reglist[[i]],cluster="ISO",type="HC1"))
#   #Collect relevant coefficients for plotting.
#   model <- tidy(reglist[[i]], conf.int = TRUE, conf.level = 0.50) %>%
#     filter(term == "stnDAM_strong") %>%
#     mutate(model = paste("h=", i-1, sep = ""))
#   if (i == 1) {
#     regdf <- model
#   } else {
#     regdf <- rbind(regdf, model)
#   }
# }
# 
# regdf$lp = 1
# 
# pd <- position_dodge(0.1)
# gdplp<- ggplot(regdf, aes(model,estimate, group=lp)) +
#   geom_point() +
#   geom_line(linetype = "dashed", color = "blue", size = .8) +
#   theme_bw() +
#   geom_hline(yintercept = 0) +
#   scale_y_continuous("log change in real GDP") +
#   xlab("Years after disaster") +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1, position = pd)
# 
# gdplp
# 
# ggsave("plots/lp_test1.png", plot = gdplp, width = 8, height = 5, units = "in", dpi = 300)

MyData = MyData %>%
  group_by(ISO) %>%
  mutate(bvx_y0 = 100*(dplyr::lead(log(rloans),0)-dplyr::lag(log(rloans),1))) %>%
  mutate(bvx_y1 = 100*(dplyr::lead(log(rloans),1)-dplyr::lag(log(rloans),1))) %>%
  mutate(bvx_y2 = 100*(dplyr::lead(log(rloans),2)-dplyr::lag(log(rloans),1))) %>%
  mutate(bvx_y3 = 100*(dplyr::lead(log(rloans),3)-dplyr::lag(log(rloans),1))) %>%
  mutate(bvx_y4 = 100*(dplyr::lead(log(rloans),4)-dplyr::lag(log(rloans),1))) %>%
  mutate(bvx_y5 = 100*(dplyr::lead(log(rloans),5)-dplyr::lag(log(rloans),1))) %>%
  mutate(bvx_y6 = 100*(dplyr::lead(log(rloans),6)-dplyr::lag(log(rloans),1))) %>%
  mutate(bvx_y7 = 100*(dplyr::lead(log(rloans),7)-dplyr::lag(log(rloans),1))) %>%
  ungroup()
#-------------------------------------------------------------------------------





