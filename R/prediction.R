MyData_subset <- subset(MyData, select = c(crisisJST, stnDAM_strong, stnDAM_strong_L1, stnDAM_strong_L2, stnDAM_strong_L3, creditL1, creditL2, creditL3, creditL4, ISO, year_var))

MyData_subset$crisisJST_binary <- as.numeric(MyData_subset$crisisJST)
MyData_subset$year_var <- as.numeric(MyData_subset$year_var)
MyData_subset <- subset(MyData_subset, !is.na(crisisJST_binary) & !is.na(stnDAM_strong) & !is.na(stnDAM_strong_L1)
                          & !is.na(stnDAM_strong_L2) & !is.na(stnDAM_strong_L3) & !is.na(creditL1)
                          & !is.na(creditL2) & !is.na(creditL3) & !is.na(creditL4))

T2M1 <- glm(crisisJST_binary ~ creditL1 + creditL2 + creditL3 + creditL4 + factor(ISO), 
            family = binomial(), data = MyData_subset)
summary(T2M1)

T2M2 <- glm(crisisJST_binary ~ creditL1 + creditL2 + creditL3 + creditL4 + 
            stnDAM_strong + stnDAM_strong_L1 + stnDAM_strong_L2 + stnDAM_strong_L3 +
            factor(ISO), family = binomial(), data = MyData_subset)
summary(T2M2)

T2M3 <- glm(crisisJST_binary ~ stnDAM_strong +stnDAM_strong_L1 + stnDAM_strong_L2 + stnDAM_strong_L3 
            + factor(ISO), family = binomial(), MyData_subset)
summary(T2M3)

# Predicted probabilities
T2M1_probs <- predict(T2M1, newdata = MyData_subset, type = "response")
T2M2_probs <- predict(T2M2, newdata = MyData_subset, type = "response")
T2M3_probs <- predict(T2M3, newdata = MyData_subset, type = "response")

# Compute ROC curves for both models
ROC1_1 <- pROC::roc(MyData_subset$crisisJST_binary, T2M1_probs)
ROC1_2 <- pROC::roc(MyData_subset$crisisJST_binary, T2M2_probs)
ROC1_3 <- pROC::roc(MyData_subset$crisisJST_binary, T2M3_probs)
ROC1_1
ROC1_2
ROC1_3
# Plot ROC curves for both models

par(mar = c(4, 4, 2, 2) + 0.1)
png("plots/ROC_plot.png", width = 480, height = 480)

# Plot ROC curves for both models
plot(ROC1_2, col = "red", auc.polygon = TRUE, grid=c(0.1, 0.9), auc.polygon.col = "#f4cbcb38")
plot(ROC1_1, col = "blue", add = TRUE, auc.polygon = TRUE, grid=c(0.1, 0.9), auc.polygon.col = "#d0cef651", max.auc.polygon = TRUE, max.auc.polygon.col = "#ebe9e939")
plot(ROC1_3, col = "green", add = TRUE, auc.polygon = TRUE, grid=c(0.1, 0.9), auc.polygon.col = "#eafbb135")  

# Add diagonal line
lines(x = c(1, 0), y = c(0, 1), col = "black", lty = 2, lwd = 2)
# Add legend
legend("bottomright", legend = c("Model 1 (Both) AUC = 0.74", "Model 2 (only Credit) AUC = 0.73", "Model 3 (only Disaster) AUC = 0.66"), 
       col = c("red", "blue","green"), lwd = 2, cex = 0.8)

dev.off()
#--------------------------------------------------------------
# Set the training period end year
training_end_year <- 1995 #1995 
#--------------------------------------------------------------
# Split data into training and out-of-sample datasets
MyData_subset_train <- MyData_subset[MyData_subset$year_var <= training_end_year, ]
MyData_subset_out_of_sample <- MyData_subset[MyData_subset$year_var > training_end_year, ]

#keep only some variables in the dataset 

T2M1 <- glm(crisisJST_binary ~ creditL1 + creditL2 + creditL3 + creditL4 + factor(ISO), 
            family = binomial(), data = MyData_subset_train)
summary(T2M1)

T2M2 <- glm(crisisJST_binary ~ creditL1 + creditL2 + creditL3 + creditL4 + 
            stnDAM_strong + stnDAM_strong_L1 + stnDAM_strong_L2 + stnDAM_strong_L3 +
            factor(ISO), family = binomial(), data = MyData_subset_train)
summary(T2M2)

T2M3 <- glm(crisisJST_binary ~ stnDAM_strong +stnDAM_strong_L1 + stnDAM_strong_L2 + stnDAM_strong_L3 
            + factor(ISO), family = binomial(), MyData_subset_train)
summary(T2M3)

# Predicted probabilities
T2M1_probs <- predict(T2M1, newdata = MyData_subset_out_of_sample, type = "response")
T2M2_probs <- predict(T2M2, newdata = MyData_subset_out_of_sample, type = "response")
T2M3_probs <- predict(T2M3, newdata = MyData_subset_out_of_sample, type = "response")

# Compute ROC curves for both models
ROC2_1 <- pROC::roc(MyData_subset_out_of_sample$crisisJST_binary, T2M1_probs)
ROC2_2 <- pROC::roc(MyData_subset_out_of_sample$crisisJST_binary, T2M2_probs)
ROC2_3 <- pROC::roc(MyData_subset_out_of_sample$crisisJST_binary, T2M3_probs)
ROC2_1
ROC2_2
ROC2_3
# Plot ROC curves for both models

par(mar = c(4, 4, 2, 2) + 0.1)
png("plots/ROC_plot2.png", width = 480, height = 480)

# Plot ROC curves for both models
plot(ROC2_2, col = "red", auc.polygon = TRUE, grid=c(0.1, 0.9), auc.polygon.col = "#f4cbcb38")
plot(ROC2_1, col = "blue", add = TRUE, auc.polygon = TRUE, grid=c(0.1, 0.9), auc.polygon.col = "#d0cef651", max.auc.polygon = TRUE, max.auc.polygon.col = "#ebe9e939")
plot(ROC2_3, col = "green", add = TRUE, auc.polygon = TRUE, grid=c(0.1, 0.9), auc.polygon.col = "#eafbb135")  

# Add diagonal line
lines(x = c(1, 0), y = c(0, 1), col = "black", lty = 2, lwd = 2)
# Add legend
legend("bottomright", legend = c("Model 1 (Both) AUC = 0.54", "Model 2 (only Credit) AUC = 0.60", "Model 3 (only Disaster) AUC = 0.60"), 
       col = c("red", "blue","green"), lwd = 2, cex = 0.8)

dev.off()
#--------------------------------------------------------------

# Set up the plotting parameters
par(mar = c(4, 4, 2, 2) + 0.1)
png("plots/ROC_plot_combined.png", width = 960, height = 480)

# Split the plotting area into two columns
par(mfrow = c(1, 2))

# Plot ROC curves for both models
plot(ROC1_2, col = "red", auc.polygon = TRUE, grid=c(0.1, 0.9), auc.polygon.col = "#f4cbcb38")
plot(ROC1_1, col = "blue", add = TRUE, auc.polygon = TRUE, grid=c(0.1, 0.9), auc.polygon.col = "#d0cef651", max.auc.polygon = TRUE, max.auc.polygon.col = "#ebe9e939")
plot(ROC1_3, col = "green", add = TRUE, auc.polygon = TRUE, grid=c(0.1, 0.9), auc.polygon.col = "#eafbb135")  
lines(x = c(1, 0), y = c(0, 1), col = "black", lty = 2, lwd = 2)
legend("bottomright", legend = c("Model 1 (Both) AUC = 0.74", "Model 2 (only Credit) AUC = 0.73", "Model 3 (only Disaster) AUC = 0.65"), 
       col = c("red", "blue","green"), lwd = 2, cex = 0.8)
title("A")

# Plot ROC curves for both models
plot(ROC2_2, col = "red", auc.polygon = TRUE, grid=c(0.1, 0.9), auc.polygon.col = "#f4cbcb38")
plot(ROC2_1, col = "blue", add = TRUE, auc.polygon = TRUE, grid=c(0.1, 0.9), auc.polygon.col = "#d0cef651", max.auc.polygon = TRUE, max.auc.polygon.col = "#ebe9e939")
plot(ROC2_3, col = "green", add = TRUE, auc.polygon = TRUE, grid=c(0.1, 0.9), auc.polygon.col = "#eafbb135")  
lines(x = c(1, 0), y = c(0, 1), col = "black", lty = 2, lwd = 2)
legend("bottomright", legend = c("Model 1 (Both) AUC = 0.54", "Model 2 (only Credit) AUC = 0.60", "Model 3 (only Disaster) AUC = 0.60"), 
       col = c("red", "blue","green"), lwd = 2, cex = 0.8)
title("B")
# Save the plot
dev.off()










#other code: 

# #--------------------------------------------------------------
# MyData_subset <- subset(MyData, select = c(crisisJST,RC, NoOfDis , NoOfDis_L1 , NoOfDis_L2 , NoOfDis_L3, creditL1, creditL2, creditL3, creditL4, ISO, year_var))
# 
# MyData_subset$crisisJST_binary <- as.numeric(MyData_subset$crisisJST)
# MyData_subset$RC_binary <- as.numeric(MyData_subset$RC)
# 
# MyData_subset <- subset(MyData_subset, !is.na(RC_binary) & !is.na(NoOfDis) & !is.na(NoOfDis_L1)
#                           & !is.na(NoOfDis_L2) & !is.na(NoOfDis_L3) 
#                            & !is.na(creditL1) & !is.na(creditL2) & !is.na(creditL3) & !is.na(creditL4)
#                           )
# 
# # Set the training period end year
# training_end_year <- 1980
# 
# # Split data into training and out-of-sample datasets
# MyData_subset_train <- MyData_subset[MyData_subset$year_var <= training_end_year, ]
# MyData_subset_out_of_sample <- MyData_subset[MyData_subset$year_var > training_end_year, ]
# 
# #keep only some variables in the dataset 
# T1M1 <- glm(RC_binary ~ NoOfDis + NoOfDis_L1 + NoOfDis_L2 + NoOfDis_L3 
#             + factor(ISO), family = binomial(), MyData_subset_train)
# 
# T1M2 <- glm(RC_binary ~ DisDummy + DisDummy_L1 + DisDummy_L2 + DisDummy_L3
#             + factor(ISO), 
#             family = binomial(), MyData_subset_train)
# 
# T1M1_full <- glm(RC_binary ~ NoOfDis + NoOfDis_L1 + NoOfDis_L2 + NoOfDis_L3 
#             + factor(ISO), family = binomial(), MyData_subset)
# 
# T1M2_full <- glm(RC_binary ~ DisDummy + DisDummy_L1 + DisDummy_L2 + DisDummy_L3
#             + factor(ISO), 
#             family = binomial(), MyData_subset)
# 
# T1M1_probs <- predict(T1M1, newdata = MyData_subset_out_of_sample, type = "response")
# T1M2_probs <- predict(T1M2, newdata = MyData_subset_out_of_sample, type = "response")
# T1M1_ROC <- pROC::roc(MyData_subset_out_of_sample$crisisJST_binary, T1M1_probs)
# T1M2_ROC <- pROC::roc(MyData_subset_out_of_sample$crisisJST_binary, T1M2_probs)
# T1M1_ROC
# T1M2_ROC
# # Plot ROC curves for both models
# T1M1_full_probs <- predict(T1M1_full, newdata = MyData_subset, type = "response")
# T1M2_full_probs <- predict(T1M2_full, newdata = MyData_subset, type = "response")
# T1M1_full_ROC <- pROC::roc(MyData_subset$crisisJST_binary, T1M1_full_probs)
# T1M2_full_ROC <- pROC::roc(MyData_subset$crisisJST_binary, T1M2_full_probs)
# T1M1_full_ROC
# T1M2_full_ROC
# # Plot ROC curves for both models
# plot(T1M1_full_ROC, col = "blue", auc.polygon = TRUE, grid=c(0.1, 0.9), auc.polygon.col = "#aaa6a68e", max.auc.polygon = TRUE, max.auc.polygon.col = "#ebe9e939")
# plot(T1M2_full_ROC, col = "red", add = TRUE, auc.polygon = TRUE, grid=c(0.1, 0.9), auc.polygon.col = "#d3d3d36a")
# 
# 
# 
# 
# plot(T1M1_ROC, col = "blue", auc.polygon = TRUE, grid=c(0.1, 0.9), auc.polygon.col = "#aaa6a68e", max.auc.polygon = TRUE, max.auc.polygon.col = "#ebe9e939")
# plot(T1M2_ROC, col = "red", add = TRUE, auc.polygon = TRUE, grid=c(0.1, 0.9), auc.polygon.col = "#d3d3d36a")
# 
# #--------------------------------------------------------------
# MyData_subset <- subset(MyData, select = c(crisisJST, stnDAM_strong_MA5, creditMA5_L1, ISO, year_var))
# 
# MyData_subset$crisisJST_binary <- as.numeric(MyData_subset$crisisJST)
# 
# MyData_subset <- subset(MyData_subset, !is.na(crisisJST_binary) & !is.na(stnDAM_strong_MA5) & !is.na(creditMA5_L1)
#                           & !is.na(stnDAM_strong_MA5*creditMA5_L1))
# 
# # Set the training period end year
# training_end_year <- 1980
# 
# # Split data into training and out-of-sample datasets
# MyData_subset_train <- MyData_subset[MyData_subset$year_var <= training_end_year, ]
# MyData_subset_out_of_sample <- MyData_subset[MyData_subset$year_var > training_end_year, ]
# 
# 
# T2M1 <- glm(crisisJST_binary ~  creditMA5_L1 
#             + factor(ISO), family = binomial(), MyData_subset)
# summary(T2M1)
# 
# # Predicted probabilities
# T2M1_probs <- predict(T2M1, newdata = MyData_subset, type = "response")
# 
# # Compute ROC curves for both models
# T2M1_ROC <- pROC::roc(MyData_subset$crisisJST_binary, T2M1_probs)
# T2M1_ROC
# # Plot ROC curves for both models
# # Plot ROC curves for both models
# plot(T2M1_ROC, col = "red", auc.polygon = TRUE, grid=c(0.1, 0.9), auc.polygon.col = "#f4cbcb38")
