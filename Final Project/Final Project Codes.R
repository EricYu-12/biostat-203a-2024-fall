# Install packages and load required libraries
install.packages("dplyr")
install.packages("kableExtra")
install.packages("janitor")
install.packages("ggplot2")
install.packages("pROC")
install.packages("labelled")
install.packages("tidyr")
install.packages("gt")
install.packages("gridExtra")
install.packages("flextable")
install.packages("stringr")

library(dplyr)
library(ggplot2)
library(pROC)
library(knitr) 
library(kableExtra) 
library(janitor)
library(labelled)
library(tidyr)
library(gt)
library(gridExtra)
library(flextable)
library(stringr)

R.Version()

# Import the dataset
Heart <- read.csv("C:/Users/zxhyu/Desktop/UCLA/Fall 2024/Biostats 203A/Final Project/Heart.csv")

# Add labels to columns
attr(Heart$Age, "label") <- "Age"
attr(Heart$Sex, "label") <- "Gender"
attr(Heart$ChestPainType, "label") <- "Chest Pain Type"
attr(Heart$RestingBP, "label") <- "Resting Blood Pressure"
attr(Heart$Cholesterol, "label") <- "Cholesterol Level"
attr(Heart$FastingBS, "label") <- "Fasting Blood Sugar"
attr(Heart$RestingECG, "label") <- "Resting Electrocardiogram Results"
attr(Heart$MaxHR, "label") <- "Maximum Heart Rate Achieved"
attr(Heart$ExerciseAngina, "label") <- "Exercise-Induced Angina"
attr(Heart$Oldpeak, "label") <- "ST"
attr(Heart$ST_Slope, "label") <- "The Slope of The Peak Exercise ST Segment"
attr(Heart$HeartDisease, "label") <- "Has Heart Disease"

# Remove duplicated rows
Heart <- Heart %>% distinct()

# Define formats as functions
Chest_Pain_Type_Group <- c(
  "ATA" = "Atypical Angina",
  "TA" = "Typical Angina",
  "NAP" = "Non-Anginal Pain",
  "ASY" = "Asymptomatic"
)

heartdisease_no_yes <- c(
  `0` = "No",
  `1` = "Yes"
)

ExerciseAngina_no_yes <- c(
  "N" = "No",
  "Y" = "Yes"
)

# Apply formats to the dataset
Heart <- Heart %>%
  mutate(
    ChestPainType = recode(ChestPainType, !!!Chest_Pain_Type_Group),
    HeartDisease = recode(as.character(HeartDisease), !!!heartdisease_no_yes),
    ExerciseAngina = recode(ExerciseAngina, !!!ExerciseAngina_no_yes)
  )

# Create summary table 1a for continuous variables
# Summary statistics 
summarized_data <- Heart %>%
  pivot_longer(cols = c(Age, RestingBP, Cholesterol, MaxHR), 
               names_to = "Variable", 
               values_to = "Value") %>%
  group_by(ChestPainType, Variable) %>%
  summarize(
    N = sum(!is.na(Value)),
    Mean = mean(Value, na.rm = TRUE),
    `Std Dev` = sd(Value, na.rm = TRUE),
    Median = median(Value, na.rm = TRUE),
    Minimum = min(Value, na.rm = TRUE),
    Maximum = max(Value, na.rm = TRUE),
    .groups = "drop"
  )

# Print summary table
title <- "Baseline Characteristics of Study Population"
cat(title, "\n")
kable(summarized_data, digits = 3, col.names = c(
  "Chest Pain Type", "Variable", "N", "Mean", "Std Dev", "Median", "Minimum", "Maximum"
))

# Create the table for plotting
generate_table <- function(data) {
  grid.table(
    data,
    rows = NULL,
    theme = ttheme_default(
      core = list(fg_params = list(fontsize = 10), bg_params = list(fill = c("white", "lightblue"), col = NA)),
      colhead = list(fg_params = list(fontsize = 12, fontface = "bold"), bg_params = list(fill = "lightgrey"))
    )
  )
}

# Plot the summary table
title <- "Baseline Characteristics of Study Population"
cat(title, "\n")
generate_table(summarized_data)

# Tabulation by groups 
# Generate summary table 
summary_table <- Heart %>%
  mutate(ExerciseAngina = recode(ExerciseAngina, "N" = "No", "Y" = "Yes")) %>%  # Mutate ExerciseAngina
  group_by(RestingECG, ExerciseAngina, ChestPainType) %>%  # Exclude HeartDisease
  summarise(Count = n(), .groups = 'drop') %>%
  ungroup() %>%
  group_by(RestingECG, ExerciseAngina) %>%  # Adjust grouping
  mutate(`Col %` = round(Count / sum(Count) * 100, 2)) %>%
  ungroup() %>%
  complete(RestingECG, ExerciseAngina, ChestPainType, fill = list(Count = 0, `Col %` = 0))  # Adjust complete

# Pivot the data for a tabular display
pivot_table <- summary_table %>%
  pivot_wider(names_from = ChestPainType, values_from = c(Count, `Col %`), names_sep = "_")

# Create a table for display
gt_table <- gt(pivot_table) %>%
  tab_header(
    title = "Summary Table",
    subtitle = "RestingECG, ExerciseAngina by Chest Pain Types"
  )

# Print the table
gt_table

# Frequency table
freq_table <- table(Heart$ChestPainType, Heart$HeartDisease)
addmargins(freq_table)

# Generate a clustered bar plot for HeartDisease grouped by ChestPainType
Heart <- Heart %>%
  mutate(
    ChestPainType = recode(ChestPainType, !!!Chest_Pain_Type_Group),
    HeartDisease = factor(recode(as.character(HeartDisease), !!!heartdisease_no_yes), 
                          levels = c("No", "Yes")),
    ExerciseAngina = recode(ExerciseAngina, !!!ExerciseAngina_no_yes)
  )

# Check levels of HeartDisease (debugging step)
levels(Heart$HeartDisease)

# Create the plot
p <- ggplot(Heart, aes(x = HeartDisease, fill = ChestPainType)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Heart Diseases Frequencies Amongst Each Chest Pain Type",
       y = "Frequency", x = "Has Heart Disease") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

print(p)

# Filter data by ChestPainType
Heart_ATA <- Heart %>% filter(ChestPainType == "Atypical Angina")
Heart_TA <- Heart %>% filter(ChestPainType == "Typical Angina")
Heart_NAP <- Heart %>% filter(ChestPainType == "Non-Anginal Pain")
Heart_ASY <- Heart %>% filter(ChestPainType == "Asymptomatic")



##############################################################################################
# ROC curve for ATA
# Logistic regression model
model <- glm(HeartDisease ~ Age + RestingBP + Cholesterol + MaxHR + RestingECG + ExerciseAngina,
             data = Heart_ATA, family = binomial(link = "logit"))

# Add predicted probabilities to the dataset
Heart_ATA$ATA_heartdisease_predicted <- predict(model, type = "response")

# Compute ROC curve
roc_data_ata <- roc(Heart_ATA$HeartDisease, Heart_ATA$ATA_heartdisease_predicted, levels = c("No", "Yes"))

# Extract thresholds, sensitivity, and specificity
roc_df_ata <- data.frame(
  threshold = roc_data_ata$thresholds,
  sensitivity = roc_data_ata$sensitivities,
  specificity = roc_data_ata$specificities
)

# Calculate the sum of sensitivity and specificity
roc_df_ata <- roc_df_ata %>%
  mutate(sum_sens_spec = sensitivity + specificity)

# Find the optimal cutoff
optimal_cutoff_ata <- roc_df_ata %>% arrange(desc(sum_sens_spec)) %>% slice(1)
print(optimal_cutoff_ata)

# Apply optimal cutoff to classify predictions
Heart_ATA <- Heart_ATA %>%
  mutate(predicted = ifelse(ATA_heartdisease_predicted >= 0.192, 1, 0),
         predicted = factor(predicted, levels = c(0, 1), labels = c("No", "Yes")))

# Generate Confusion Matrix
confusion_matrix <- table(Heart_ATA$HeartDisease, Heart_ATA$predicted)
print("Confusion Matrix of Predicted Heart Disease Occurrences Amongst ATA Group using a Logistic Regression Model")
print(confusion_matrix)

# Plot the ROC curve with AUC
plot(roc_data_ata, main = "ROC Curve for ATA", col = "blue", lwd = 2)
auc_value <- auc(roc_data_ata)
text(0.5, 0.5, labels = paste0("AUC = ", round(auc_value, 4)), col = "red", cex = 1.5)

########################################################################################################
# ROC curve for TA
# Logistic regression model
model <- glm(HeartDisease ~ Age + RestingBP + Cholesterol + MaxHR + RestingECG + ExerciseAngina,
             data = Heart_TA, family = binomial(link = "logit"))

# Add predicted probabilities to the dataset
Heart_TA$TA_heartdisease_predicted <- predict(model, type = "response")

# Compute ROC curve
roc_data_ta <- roc(Heart_TA$HeartDisease, Heart_TA$TA_heartdisease_predicted, levels = c("No", "Yes"))

# Extract thresholds, sensitivity, and specificity
roc_df_ta <- data.frame(
  threshold = roc_data_ta$thresholds,
  sensitivity = roc_data_ta$sensitivities,
  specificity = roc_data_ta$specificities
)

# Calculate the sum of sensitivity and specificity
roc_df_ta <- roc_df_ta %>%
  mutate(sum_sens_spec = sensitivity + specificity)

# Find the optimal cutoff
optimal_cutoff_ta <- roc_df_ta %>% arrange(desc(sum_sens_spec)) %>% slice(1)
print(optimal_cutoff_ta)

# Apply optimal cutoff to classify predictions
Heart_TA <- Heart_TA %>%
  mutate(predicted = ifelse(TA_heartdisease_predicted >= 0.471, 1, 0),
         predicted = factor(predicted, levels = c(0, 1), labels = c("No", "Yes")))

# Generate Confusion Matrix
confusion_matrix <- table(Heart_TA$HeartDisease, Heart_TA$predicted)
print("Confusion Matrix of Predicted Heart Disease Occurrences Amongst TA Group using a Logistic Regression Model")
print(confusion_matrix)

# Plot the ROC curve with AUC
plot(roc_data_ta, main = "ROC Curve for TA", col = "blue", lwd = 2)
auc_value <- auc(roc_data_ta)
text(0.5, 0.5, labels = paste0("AUC = ", round(auc_value, 4)), col = "red", cex = 1.5)

###########################################################################################################
# ROC curve for NAP
# Logistic regression model
model <- glm(HeartDisease ~ Age + RestingBP + Cholesterol + MaxHR + RestingECG + ExerciseAngina,
             data = Heart_NAP, family = binomial(link = "logit"))

# Add predicted probabilities to the dataset
Heart_NAP$NAP_heartdisease_predicted <- predict(model, type = "response")

# Compute ROC curve
roc_data_nap <- roc(Heart_NAP$HeartDisease, Heart_NAP$NAP_heartdisease_predicted, levels = c("No", "Yes"))

# Extract thresholds, sensitivity, and specificity
roc_df_nap <- data.frame(
  threshold = roc_data_nap$thresholds,
  sensitivity = roc_data_nap$sensitivities,
  specificity = roc_data_nap$specificities
)

# Calculate the sum of sensitivity and specificity
roc_df_nap <- roc_df_nap %>%
  mutate(sum_sens_spec = sensitivity + specificity)

# Find the optimal cutoff
optimal_cutoff_nap <- roc_df_nap %>% arrange(desc(sum_sens_spec)) %>% slice(1)
print(optimal_cutoff_nap)

# Apply optimal cutoff to classify predictions
Heart_NAP <- Heart_NAP %>%
  mutate(predicted = ifelse(NAP_heartdisease_predicted >= 0.426, 1, 0),
         predicted = factor(predicted, levels = c(0, 1), labels = c("No", "Yes")))

# Generate Confusion Matrix
confusion_matrix <- table(Heart_NAP$HeartDisease, Heart_NAP$predicted)
print("Confusion Matrix of Predicted Heart Disease Occurrences Amongst TA Group using a Logistic Regression Model")
print(confusion_matrix)

# Plot the ROC curve with AUC
plot(roc_data_nap, main = "ROC Curve for NAP", col = "blue", lwd = 2)
auc_value <- auc(roc_data_nap)
text(0.5, 0.5, labels = paste0("AUC = ", round(auc_value, 4)), col = "red", cex = 1.5)

###################################################################################################
# ROC curve for ASY
# Logistic regression model
model <- glm(HeartDisease ~ Age + RestingBP + Cholesterol + MaxHR + RestingECG + ExerciseAngina,
             data = Heart_ASY, family = binomial(link = "logit"))

# Add predicted probabilities to the dataset
Heart_ASY$ASY_heartdisease_predicted <- predict(model, type = "response")

# Compute ROC curve
roc_data_asy <- roc(Heart_ASY$HeartDisease, Heart_ASY$ASY_heartdisease_predicted, levels = c("No", "Yes"))

# Extract thresholds, sensitivity, and specificity
roc_df_asy <- data.frame(
  threshold = roc_data_asy$thresholds,
  sensitivity = roc_data_asy$sensitivities,
  specificity = roc_data_asy$specificities
)

# Calculate the sum of sensitivity and specificity
roc_df_asy <- roc_df_asy %>%
  mutate(sum_sens_spec = sensitivity + specificity)

# Find the optimal cutoff
optimal_cutoff <- roc_df_asy %>% arrange(desc(sum_sens_spec)) %>% slice(1)
print(optimal_cutoff)

# Apply optimal cutoff to classify predictions
Heart_ASY <- Heart_ASY %>%
  mutate(predicted = ifelse(ASY_heartdisease_predicted >= 0.750, 1, 0),
         predicted = factor(predicted, levels = c(0, 1), labels = c("No", "Yes")))

# Generate Confusion Matrix
confusion_matrix <- table(Heart_ASY$HeartDisease, Heart_ASY$predicted)
print("Confusion Matrix of Predicted Heart Disease Occurrences Amongst ASY Group using a Logistic Regression Model")
print(confusion_matrix)

# Plot the ROC curve with AUC
plot(roc_data_asy, main = "ROC Curve for ASY", col = "blue", lwd = 2)
auc_value <- auc(roc_data_asy)
text(0.5, 0.5, labels = paste0("AUC = ", round(auc_value, 4)), col = "red", cex = 1.5)

# Compare the four ROC curves using pairwise one-sided tests
roc_comparison_1_2 <- roc.test(roc_data_ata, roc_data_ta, alternative = "greater")
roc_comparison_1_3 <- roc.test(roc_data_ata, roc_data_nap, alternative = "greater")
roc_comparison_1_4 <- roc.test(roc_data_ata, roc_data_asy, alternative = "greater")
roc_comparison_2_3 <- roc.test(roc_data_ta, roc_data_nap, alternative = "greater")
roc_comparison_2_4 <- roc.test(roc_data_ta, roc_data_asy, alternative = "greater")
roc_comparison_3_4 <- roc.test(roc_data_nap, roc_data_asy, alternative = "greater")

# Print pairwise one-sided comparison results with significance level 0.05
cat("P-value for ROC curve comparison (ATA > TA):", roc_comparison_1_2$p.value, ifelse(roc_comparison_1_2$p.value < 0.05, "(Significant)", "(Not Significant)"), "\n")
cat("P-value for ROC curve comparison (ATA > NAP):", roc_comparison_1_3$p.value, ifelse(roc_comparison_1_3$p.value < 0.05, "(Significant)", "(Not Significant)"), "\n")
cat("P-value for ROC curve comparison (ATA > ASY):", roc_comparison_1_4$p.value, ifelse(roc_comparison_1_4$p.value < 0.05, "(Significant)", "(Not Significant)"), "\n")
cat("P-value for ROC curve comparison (TA > NAP):", roc_comparison_2_3$p.value, ifelse(roc_comparison_2_3$p.value < 0.05, "(Significant)", "(Not Significant)"), "\n")
cat("P-value for ROC curve comparison (TA > ASY):", roc_comparison_2_4$p.value, ifelse(roc_comparison_2_4$p.value < 0.05, "(Significant)", "(Not Significant)"), "\n")
cat("P-value for ROC curve comparison (NAP > ASY):", roc_comparison_3_4$p.value, ifelse(roc_comparison_3_4$p.value < 0.05, "(Significant)", "(Not Significant)"), "\n")











