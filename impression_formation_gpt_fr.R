#####################
#   GETTING READY   #
#####################

setwd("F:/LPI lessons/pscychology intern/NewProject/gpt_fr/ABOS_context/fr_version/regression")


# libraries
library(dplyr)

#Remove: Removes objects from memory
rm(list = ls())

#Garbage Collection: Frees up memory, but preserves variables created in previous steps
gc()

getwd()


### read and prepare data
set.seed(123)

ratings_gpt_fr_in_out <- read.csv("gpt_fr_in_out.csv", fileEncoding = "UTF-8")


####################
## ACTION REQUIRED #
####################


# select data and reduce
regr_data <- ratings_gpt_fr_in_out[,c(22,2,17,23,24,25,31,32,33)]

# reshape
regr_data <- reshape(regr_data, idvar = "ID_level_1", timevar = "item_term", direction = "wide")


# cat("original:", nrow(regr_data), "\n")

regr_data <- na.omit(regr_data)

# cat("after remove:", nrow(regr_data), "\n")

# reduce df
rownames(regr_data) <- regr_data$Concept

# left joing back to add the sentence
match <- ratings_gpt_fr_in_out[,c(2,3)] 
match <- distinct(match, .keep_all = FALSE)
regr_data <- left_join(regr_data, match, by = "ID_level_1")


##################### REGRESSION FOR THE SUBJECT ##################### 
# evaluation

# Calculate means of the independent variables
mean_mean_eva_out_Subject <- mean(regr_data$Mean_GPT_E_out.item_A)
mean_mean_eva_out_Behavior <- mean(regr_data$Mean_GPT_E_out.item_B)
mean_mean_eva_out_Object <- mean(regr_data$Mean_GPT_E_out.item_O)
mean_mean_eva_out_Setting <- mean(regr_data$Mean_GPT_E_out.item_S)
mean_mean_pot_out_Subject <- mean(regr_data$Mean_GPT_P_out.item_A)
mean_mean_pot_out_Behavior <- mean(regr_data$Mean_GPT_P_out.item_B)
mean_mean_pot_out_Object <- mean(regr_data$Mean_GPT_P_out.item_O)
mean_mean_pot_out_Setting <- mean(regr_data$Mean_GPT_P_out.item_S)
mean_mean_act_out_Subject <- mean(regr_data$Mean_GPT_A_out.item_A)
mean_mean_act_out_Behavior <- mean(regr_data$Mean_GPT_A_out.item_B)
mean_mean_act_out_Object <- mean(regr_data$Mean_GPT_A_out.item_O)
mean_mean_act_out_Setting <- mean(regr_data$Mean_GPT_A_out.item_S)

# Center the variables
regr_data$mean_eva_out.Subject_centered <- regr_data$Mean_GPT_E_out.item_A - mean_mean_eva_out_Subject
regr_data$mean_eva_out.Behavior_centered <- regr_data$Mean_GPT_E_out.item_B - mean_mean_eva_out_Behavior
regr_data$mean_eva_out.Object_centered <- regr_data$Mean_GPT_E_out.item_O - mean_mean_eva_out_Object
regr_data$mean_eva_out.Setting_centered <- regr_data$Mean_GPT_E_out.item_S - mean_mean_eva_out_Setting
regr_data$mean_pot_out.Subject_centered <- regr_data$Mean_GPT_P_out.item_A - mean_mean_pot_out_Subject
regr_data$mean_pot_out.Behavior_centered <- regr_data$Mean_GPT_P_out.item_B - mean_mean_pot_out_Behavior
regr_data$mean_pot_out.Object_centered <- regr_data$Mean_GPT_P_out.item_O - mean_mean_pot_out_Object
regr_data$mean_pot_out.Setting_centered <- regr_data$Mean_GPT_P_out.item_S - mean_mean_pot_out_Setting
regr_data$mean_act_out.Subject_centered <- regr_data$Mean_GPT_A_out.item_A - mean_mean_act_out_Subject
regr_data$mean_act_out.Behavior_centered <- regr_data$Mean_GPT_A_out.item_B - mean_mean_act_out_Behavior
regr_data$mean_act_out.Object_centered <- regr_data$Mean_GPT_A_out.item_O - mean_mean_act_out_Object
regr_data$mean_act_out.Setting_centered <- regr_data$Mean_GPT_A_out.item_S - mean_mean_act_out_Setting


# Run the regression with centered variables
res_subject_e_centered <- lm(Mean_GPT_E_in.item_A ~
                               mean_eva_out.Subject_centered + mean_eva_out.Behavior_centered + mean_eva_out.Object_centered + mean_eva_out.Setting_centered +
                               mean_pot_out.Subject_centered + mean_pot_out.Behavior_centered + mean_pot_out.Object_centered + mean_pot_out.Setting_centered +
                               mean_act_out.Subject_centered + mean_act_out.Behavior_centered + mean_act_out.Object_centered + mean_act_out.Setting_centered +
                               ### interaction terms with centered variables
                               mean_eva_out.Behavior_centered * mean_eva_out.Object_centered +
                               mean_act_out.Subject_centered * mean_act_out.Setting_centered +
                               mean_act_out.Behavior_centered * mean_act_out.Setting_centered +
                               mean_act_out.Subject_centered * mean_act_out.Behavior_centered * mean_act_out.Setting_centered
                             , data = regr_data)
summary(res_subject_e_centered)




res_subject_e <- lm(Mean_GPT_E_in.item_A ~
                      Mean_GPT_E_out.item_A + Mean_GPT_E_out.item_B + Mean_GPT_E_out.item_O + Mean_GPT_E_out.item_S +
                      Mean_GPT_P_out.item_A + Mean_GPT_P_out.item_B + Mean_GPT_P_out.item_O + Mean_GPT_P_out.item_S +
                      Mean_GPT_A_out.item_A + Mean_GPT_A_out.item_B + Mean_GPT_A_out.item_O + Mean_GPT_A_out.item_S +
                      ### interaction terms are defined following Smith-Lovin 1987 - SEE FORMULA 1
                      Mean_GPT_E_out.item_B * Mean_GPT_E_out.item_O +
                      Mean_GPT_A_out.item_A * Mean_GPT_A_out.item_S +
                      Mean_GPT_A_out.item_B * Mean_GPT_A_out.item_S +
                      Mean_GPT_A_out.item_A * Mean_GPT_A_out.item_B * Mean_GPT_A_out.item_S
                    , data = regr_data)
summary(res_subject_e)


# Extract summaries of both models
summary_res_subject_e <- summary(res_subject_e)
summary_res_subject_e_centered <- summary(res_subject_e_centered)

# Get coefficients and p-values
coefficients_res_subject_e <- summary_res_subject_e$coefficients
coefficients_res_subject_e_centered <- summary_res_subject_e_centered$coefficients

# Create a data frame for comparison
comparison_df_suj_E <- data.frame(
  Effect = row.names(coefficients_res_subject_e),
  Coefficient_Original = coefficients_res_subject_e[, "Estimate"],
  P_value_Original = coefficients_res_subject_e[, "Pr(>|t|)"],
  Coefficient_Centered = coefficients_res_subject_e_centered[, "Estimate"],
  P_value_Centered = coefficients_res_subject_e_centered[, "Pr(>|t|)"]
)

# Function to mark significant coefficients
mark_significance <- function(coefficient, p_value) {
  if (p_value < 0.05) {
    return(paste0(coefficient, "*"))
  } else {
    return(as.character(coefficient))
  }
}

# Apply the function to the coefficients
comparison_df_suj_E$Coefficient_Original <- mapply(mark_significance, 
                                                   comparison_df_suj_E$Coefficient_Original, 
                                                   comparison_df_suj_E$P_value_Original)

comparison_df_suj_E$Coefficient_Centered <- mapply(mark_significance, 
                                                   comparison_df_suj_E$Coefficient_Centered, 
                                                   comparison_df_suj_E$P_value_Centered)

# Remove p-values from the table as significance is now indicated
comparison_df_suj_E$P_value_Original <- NULL
comparison_df_suj_E$P_value_Centered <- NULL

# Use View to inspect the table
View(comparison_df_suj_E)



# potency
res_subject_p <- lm(Mean_GPT_P_in.item_A ~
                      Mean_GPT_E_out.item_A + Mean_GPT_E_out.item_B + Mean_GPT_E_out.item_O + Mean_GPT_E_out.item_S +
                      Mean_GPT_P_out.item_A + Mean_GPT_P_out.item_B + Mean_GPT_P_out.item_O + Mean_GPT_P_out.item_S +
                      Mean_GPT_A_out.item_A + Mean_GPT_A_out.item_B + Mean_GPT_A_out.item_O + Mean_GPT_A_out.item_S +
                      ### interaction terms are defined following Smith-Lovin 1987 - SEE FORMULA 1
                      Mean_GPT_E_out.item_B * Mean_GPT_E_out.item_O +
                      Mean_GPT_A_out.item_A * Mean_GPT_A_out.item_S +
                      Mean_GPT_A_out.item_B * Mean_GPT_A_out.item_S +
                      Mean_GPT_A_out.item_A * Mean_GPT_A_out.item_B * Mean_GPT_A_out.item_S
                    , data = regr_data)

summary(res_subject_p)

res_subject_p_centered <- lm(Mean_GPT_P_in.item_A ~
                               mean_eva_out.Subject_centered + mean_eva_out.Behavior_centered + mean_eva_out.Object_centered + mean_eva_out.Setting_centered +
                               mean_pot_out.Subject_centered + mean_pot_out.Behavior_centered + mean_pot_out.Object_centered + mean_pot_out.Setting_centered +
                               mean_act_out.Subject_centered + mean_act_out.Behavior_centered + mean_act_out.Object_centered + mean_act_out.Setting_centered +
                               ### interaction terms with centered variables
                               mean_eva_out.Behavior_centered * mean_eva_out.Object_centered +
                               mean_act_out.Subject_centered * mean_act_out.Setting_centered +
                               mean_act_out.Behavior_centered * mean_act_out.Setting_centered +
                               mean_act_out.Subject_centered * mean_act_out.Behavior_centered * mean_act_out.Setting_centered
                             , data = regr_data)


# Extract summaries of both models
summary_res_subject_p <- summary(res_subject_p)
summary_res_subject_p_centered <- summary(res_subject_p_centered)

# Get coefficients and p-values
coefficients_res_subject_p <- summary_res_subject_p$coefficients
coefficients_res_subject_p_centered <- summary_res_subject_p_centered$coefficients

# Create a data frame for comparison
comparison_df_suj_P <- data.frame(
  Effect = row.names(coefficients_res_subject_p),
  Coefficient_Original = coefficients_res_subject_p[, "Estimate"],
  P_value_Original = coefficients_res_subject_p[, "Pr(>|t|)"],
  Coefficient_Centered = coefficients_res_subject_p_centered[, "Estimate"],
  P_value_Centered = coefficients_res_subject_p_centered[, "Pr(>|t|)"]
)

# Function to mark significant coefficients
mark_significance <- function(coefficient, p_value) {
  if (p_value < 0.05) {
    return(paste0(coefficient, "*"))
  } else {
    return(as.character(coefficient))
  }
}

# Apply the function to the coefficients
comparison_df_suj_P$Coefficient_Original <- mapply(mark_significance, 
                                                   comparison_df_suj_P$Coefficient_Original, 
                                                   comparison_df_suj_P$P_value_Original)

comparison_df$Coefficient_Centered <- mapply(mark_significance, 
                                             comparison_df_suj_P$Coefficient_Centered, 
                                             comparison_df_suj_P$P_value_Centered)

# Remove p-values from the table as significance is now indicated
comparison_df_suj_P$P_value_Original <- NULL
comparison_df_suj_P$P_value_Centered <- NULL

# Use View to inspect the table
View(comparison_df_suj_P)


# activity
res_subject_a <- lm(Mean_GPT_A_in.item_A ~
                      Mean_GPT_E_out.item_A + Mean_GPT_E_out.item_B + Mean_GPT_E_out.item_O + Mean_GPT_E_out.item_S +
                      Mean_GPT_P_out.item_A + Mean_GPT_P_out.item_B + Mean_GPT_P_out.item_O + Mean_GPT_P_out.item_S +
                      Mean_GPT_A_out.item_A + Mean_GPT_A_out.item_B + Mean_GPT_A_out.item_O + Mean_GPT_A_out.item_S +
                      ### interaction terms are defined following Smith-Lovin 1987 - SEE FORMULA 1
                      Mean_GPT_E_out.item_B * Mean_GPT_E_out.item_O +
                      Mean_GPT_A_out.item_A * Mean_GPT_A_out.item_S +
                      Mean_GPT_A_out.item_B * Mean_GPT_A_out.item_S +
                      Mean_GPT_A_out.item_A * Mean_GPT_A_out.item_B * Mean_GPT_A_out.item_S
                    , data = regr_data)

summary(res_subject_a)

res_subject_a_centered <- lm(Mean_GPT_A_in.item_A ~
                               mean_eva_out.Subject_centered + mean_eva_out.Behavior_centered + mean_eva_out.Object_centered + mean_eva_out.Setting_centered +
                               mean_pot_out.Subject_centered + mean_pot_out.Behavior_centered + mean_pot_out.Object_centered + mean_pot_out.Setting_centered +
                               mean_act_out.Subject_centered + mean_act_out.Behavior_centered + mean_act_out.Object_centered + mean_act_out.Setting_centered +
                               ### interaction terms with centered variables
                               mean_eva_out.Behavior_centered * mean_eva_out.Object_centered +
                               mean_act_out.Subject_centered * mean_act_out.Setting_centered +
                               mean_act_out.Behavior_centered * mean_act_out.Setting_centered +
                               mean_act_out.Subject_centered * mean_act_out.Behavior_centered * mean_act_out.Setting_centered
                             , data = regr_data)

# Extract summaries of both models
summary_res_subject_a <- summary(res_subject_a)
summary_res_subject_a_centered <- summary(res_subject_a_centered)

# Get coefficients and p-values
coefficients_res_subject_a <- summary_res_subject_a$coefficients
coefficients_res_subject_a_centered <- summary_res_subject_a_centered$coefficients

# Create a data frame for comparison
comparison_df_suj_A <- data.frame(
  Effect = row.names(coefficients_res_subject_a),
  Coefficient_Original = coefficients_res_subject_a[, "Estimate"],
  P_value_Original = coefficients_res_subject_a[, "Pr(>|t|)"],
  Coefficient_Centered = coefficients_res_subject_a_centered[, "Estimate"],
  P_value_Centered = coefficients_res_subject_a_centered[, "Pr(>|t|)"]
)

# Function to mark significant coefficients
mark_significance <- function(coefficient, p_value) {
  if (p_value < 0.05) {
    return(paste0(coefficient, "*"))
  } else {
    return(as.character(coefficient))
  }
}

# Apply the function to the coefficients
comparison_df_suj_A$Coefficient_Original <- mapply(mark_significance, 
                                                   comparison_df_suj_A$Coefficient_Original, 
                                                   comparison_df_suj_A$P_value_Original)

comparison_df_suj_A$Coefficient_Centered <- mapply(mark_significance, 
                                                   comparison_df_suj_A$Coefficient_Centered, 
                                                   comparison_df_suj_A$P_value_Centered)

# Remove p-values from the table as significance is now indicated
comparison_df_suj_A$P_value_Original <- NULL
comparison_df_suj_A$P_value_Centered <- NULL

# Use View to inspect the table
View(comparison_df_suj_A)



##################### REGRESSION FOR THE BEHAVIOR ##################### 
# evaluation
res_behavior_e <- lm(Mean_GPT_E_in.item_B ~
                       Mean_GPT_E_out.item_A + Mean_GPT_E_out.item_B + Mean_GPT_E_out.item_O + Mean_GPT_E_out.item_S +
                       Mean_GPT_P_out.item_A + Mean_GPT_P_out.item_B + Mean_GPT_P_out.item_O + Mean_GPT_P_out.item_S +
                       Mean_GPT_A_out.item_A + Mean_GPT_A_out.item_B + Mean_GPT_A_out.item_O + Mean_GPT_A_out.item_S +
                       ### interaction terms are defined following Smith-Lovin 1987 - SEE FORMULA 2 and 3
                       Mean_GPT_E_out.item_B * Mean_GPT_E_out.item_O
                     , data = regr_data)

summary(res_behavior_e)

res_behavior_e_centered <- lm(Mean_GPT_E_in.item_B ~
                                mean_eva_out.Subject_centered + mean_eva_out.Behavior_centered + mean_eva_out.Object_centered + mean_eva_out.Setting_centered +
                                mean_pot_out.Subject_centered + mean_pot_out.Behavior_centered + mean_pot_out.Object_centered + mean_pot_out.Setting_centered +
                                mean_act_out.Subject_centered + mean_act_out.Behavior_centered + mean_act_out.Object_centered + mean_act_out.Setting_centered +
                                ### interaction terms are defined following Smith-Lovin 1987 - SEE FORMULA 2 and 3
                                mean_eva_out.Behavior_centered * mean_eva_out.Object_centered
                              , data = regr_data)

# Extract summaries of both models
summary_res_behavior_e <- summary(res_behavior_e)
summary_res_behavior_e_centered <- summary(res_behavior_e_centered)

# Get coefficients and p-values
coefficients_res_behavior_e <- summary_res_behavior_e$coefficients
coefficients_res_behavior_e_centered <- summary_res_behavior_e_centered$coefficients

# Create a data frame for comparison
comparison_df_beh_E <- data.frame(
  Effect = row.names(coefficients_res_behavior_e),
  Coefficient_Original = coefficients_res_behavior_e[, "Estimate"],
  P_value_Original = coefficients_res_behavior_e[, "Pr(>|t|)"],
  Coefficient_Centered = coefficients_res_behavior_e_centered[, "Estimate"],
  P_value_Centered = coefficients_res_behavior_e_centered[, "Pr(>|t|)"]
)

# Function to mark significant coefficients
mark_significance <- function(coefficient, p_value) {
  if (p_value < 0.05) {
    return(paste0(coefficient, "*"))
  } else {
    return(as.character(coefficient))
  }
}

# Apply the function to the coefficients
comparison_df_beh_E$Coefficient_Original <- mapply(mark_significance, 
                                                   comparison_df_beh_E$Coefficient_Original, 
                                                   comparison_df_beh_E$P_value_Original)

comparison_df_beh_E$Coefficient_Centered <- mapply(mark_significance, 
                                                   comparison_df_beh_E$Coefficient_Centered, 
                                                   comparison_df_beh_E$P_value_Centered)

# Remove p-values from the table as significance is now indicated
comparison_df_beh_E$P_value_Original <- NULL
comparison_df_beh_E$P_value_Centered <- NULL

# Use View to inspect the table
View(comparison_df_beh_E)

# potency
res_behavior_p <- lm(Mean_GPT_P_in.item_B ~
                       Mean_GPT_E_out.item_A + Mean_GPT_E_out.item_B + Mean_GPT_E_out.item_O + Mean_GPT_E_out.item_S +
                       Mean_GPT_P_out.item_A + Mean_GPT_P_out.item_B + Mean_GPT_P_out.item_O + Mean_GPT_P_out.item_S +
                       Mean_GPT_A_out.item_A + Mean_GPT_A_out.item_B + Mean_GPT_A_out.item_O + Mean_GPT_A_out.item_S +
                       ### interaction terms are defined following Smith-Lovin 1987 - SEE FORMULA 2 and 3
                       Mean_GPT_E_out.item_B * Mean_GPT_E_out.item_O
                     , data = regr_data)

res_behavior_p_centered <- lm(Mean_GPT_P_in.item_B ~
                                mean_eva_out.Subject_centered + mean_eva_out.Behavior_centered + mean_eva_out.Object_centered + mean_eva_out.Setting_centered +
                                mean_pot_out.Subject_centered + mean_pot_out.Behavior_centered + mean_pot_out.Object_centered + mean_pot_out.Setting_centered +
                                mean_act_out.Subject_centered + mean_act_out.Behavior_centered + mean_act_out.Object_centered + mean_act_out.Setting_centered +
                                ### interaction terms are defined following Smith-Lovin 1987 - SEE FORMULA 2 and 3
                                mean_eva_out.Behavior_centered * mean_eva_out.Object_centered
                              , data = regr_data)

# Extract summaries of both models
summary_res_behavior_p <- summary(res_behavior_p)
summary_res_behavior_p_centered <- summary(res_behavior_p_centered)

# Get coefficients and p-values
coefficients_res_behavior_p <- summary_res_behavior_p$coefficients
coefficients_res_behavior_p_centered <- summary_res_behavior_p_centered$coefficients

# Create a data frame for comparison
comparison_df_beh_P <- data.frame(
  Effect = row.names(coefficients_res_behavior_p),
  Coefficient_Original = coefficients_res_behavior_p[, "Estimate"],
  P_value_Original = coefficients_res_behavior_p[, "Pr(>|t|)"],
  Coefficient_Centered = coefficients_res_behavior_p_centered[, "Estimate"],
  P_value_Centered = coefficients_res_behavior_p_centered[, "Pr(>|t|)"]
)

# Function to mark significant coefficients
mark_significance <- function(coefficient, p_value) {
  if (p_value < 0.05) {
    return(paste0(coefficient, "*"))
  } else {
    return(as.character(coefficient))
  }
}

# Apply the function to the coefficients
comparison_df_beh_P$Coefficient_Original <- mapply(mark_significance, 
                                                   comparison_df_beh_P$Coefficient_Original, 
                                                   comparison_df_beh_P$P_value_Original)

comparison_df_beh_P$Coefficient_Centered <- mapply(mark_significance, 
                                             comparison_df_beh_P$Coefficient_Centered, 
                                             comparison_df_beh_P$P_value_Centered)

# Remove p-values from the table as significance is now indicated
comparison_df_beh_P$P_value_Original <- NULL
comparison_df_beh_P$P_value_Centered <- NULL

# Use View to inspect the table
View(comparison_df_beh_P)


# activity
res_behavior_a <- lm(Mean_GPT_A_in.item_B ~
                       Mean_GPT_E_out.item_A + Mean_GPT_E_out.item_B + Mean_GPT_E_out.item_O + Mean_GPT_E_out.item_S +
                       Mean_GPT_P_out.item_A + Mean_GPT_P_out.item_B + Mean_GPT_P_out.item_O + Mean_GPT_P_out.item_S +
                       Mean_GPT_A_out.item_A + Mean_GPT_A_out.item_B + Mean_GPT_A_out.item_O + Mean_GPT_A_out.item_S +
                       ### interaction terms are defined following Smith-Lovin 1987 - SEE FORMULA 2 and 3
                       Mean_GPT_E_out.item_B * Mean_GPT_E_out.item_O
                     , data = regr_data)

res_behavior_a_centered <- lm(Mean_GPT_A_in.item_B ~
                                mean_eva_out.Subject_centered + mean_eva_out.Behavior_centered + mean_eva_out.Object_centered + mean_eva_out.Setting_centered +
                                mean_pot_out.Subject_centered + mean_pot_out.Behavior_centered + mean_pot_out.Object_centered + mean_pot_out.Setting_centered +
                                mean_act_out.Subject_centered + mean_act_out.Behavior_centered + mean_act_out.Object_centered + mean_act_out.Setting_centered +
                                ### interaction terms are defined following Smith-Lovin 1987 - SEE FORMULA 2 and 3
                                mean_eva_out.Behavior_centered * mean_eva_out.Object_centered
                              , data = regr_data)

# Extract summaries of both models
summary_res_behavior_a <- summary(res_behavior_a)
summary_res_behavior_a_centered <- summary(res_behavior_a_centered)

# Get coefficients and p-values
coefficients_res_behavior_a <- summary_res_behavior_a$coefficients
coefficients_res_behavior_a_centered <- summary_res_behavior_a_centered$coefficients

# Create a data frame for comparison
comparison_df_beh_A <- data.frame(
  Effect = row.names(coefficients_res_behavior_a),
  Coefficient_Original = coefficients_res_behavior_a[, "Estimate"],
  P_value_Original = coefficients_res_behavior_a[, "Pr(>|t|)"],
  Coefficient_Centered = coefficients_res_behavior_a_centered[, "Estimate"],
  P_value_Centered = coefficients_res_behavior_a_centered[, "Pr(>|t|)"]
)

# Function to mark significant coefficients
mark_significance <- function(coefficient, p_value) {
  if (p_value < 0.05) {
    return(paste0(coefficient, "*"))
  } else {
    return(as.character(coefficient))
  }
}

# Apply the function to the coefficients
comparison_df_beh_A$Coefficient_Original <- mapply(mark_significance, 
                                                   comparison_df_beh_A$Coefficient_Original, 
                                                   comparison_df_beh_A$P_value_Original)

comparison_df_beh_A$Coefficient_Centered <- mapply(mark_significance, 
                                                   comparison_df_beh_A$Coefficient_Centered, 
                                                   comparison_df_beh_A$P_value_Centered)

# Remove p-values from the table as significance is now indicated
comparison_df_beh_A$P_value_Original <- NULL
comparison_df_beh_A$P_value_Centered <- NULL

# Use View to inspect the table
View(comparison_df_beh_A)



##################### REGRESSION FOR THE OBJECT ##################### 
# evaluation
res_object_e <- lm(Mean_GPT_E_in.item_O ~
                     Mean_GPT_E_out.item_A + Mean_GPT_E_out.item_B + Mean_GPT_E_out.item_O + Mean_GPT_E_out.item_S +
                     Mean_GPT_P_out.item_A + Mean_GPT_P_out.item_B + Mean_GPT_P_out.item_O + Mean_GPT_P_out.item_S +
                     Mean_GPT_A_out.item_A + Mean_GPT_A_out.item_B + Mean_GPT_A_out.item_O + Mean_GPT_A_out.item_S +
                     ### interaction terms are defined following Smith-Lovin 1987 - SEE FORMULA 2 and 3
                     Mean_GPT_E_out.item_B * Mean_GPT_E_out.item_O
                   , data = regr_data)

res_object_e_centered <- lm(Mean_GPT_E_in.item_O ~
                              mean_eva_out.Subject_centered + mean_eva_out.Behavior_centered + mean_eva_out.Object_centered + mean_eva_out.Setting_centered +
                              mean_pot_out.Subject_centered + mean_pot_out.Behavior_centered + mean_pot_out.Object_centered + mean_pot_out.Setting_centered +
                              mean_act_out.Subject_centered + mean_act_out.Behavior_centered + mean_act_out.Object_centered + mean_act_out.Setting_centered +
                              ### interaction terms are defined following Smith-Lovin 1987 - SEE FORMULA 2 and 3
                              mean_eva_out.Behavior_centered * mean_eva_out.Object_centered
                            , data = regr_data)

# Extract summaries of both models
summary_res_object_e <- summary(res_object_e)
summary_res_object_e_centered <- summary(res_object_e_centered)

# Get coefficients and p-values
coefficients_res_object_e <- summary_res_object_e$coefficients
coefficients_res_object_e_centered <- summary_res_object_e_centered$coefficients

# Create a data frame for comparison
comparison_df_obj_E <- data.frame(
  Effect = row.names(coefficients_res_object_e),
  Coefficient_Original = coefficients_res_object_e[, "Estimate"],
  P_value_Original = coefficients_res_object_e[, "Pr(>|t|)"],
  Coefficient_Centered = coefficients_res_object_e_centered[, "Estimate"],
  P_value_Centered = coefficients_res_object_e_centered[, "Pr(>|t|)"]
)

# Function to mark significant coefficients
mark_significance <- function(coefficient, p_value) {
  if (p_value < 0.05) {
    return(paste0(coefficient, "*"))
  } else {
    return(as.character(coefficient))
  }
}

# Apply the function to the coefficients
comparison_df_obj_E$Coefficient_Original <- mapply(mark_significance, 
                                                   comparison_df_obj_E$Coefficient_Original, 
                                                   comparison_df_obj_E$P_value_Original)

comparison_df_obj_E$Coefficient_Centered <- mapply(mark_significance, 
                                                   comparison_df_obj_E$Coefficient_Centered, 
                                                   comparison_df_obj_E$P_value_Centered)

# Remove p-values from the table as significance is now indicated
comparison_df_obj_E$P_value_Original <- NULL
comparison_df_obj_E$P_value_Centered <- NULL

# Use View to inspect the table
View(comparison_df_obj_E)

# potency
res_object_p <- lm(Mean_GPT_P_in.item_O ~
                     Mean_GPT_E_out.item_A + Mean_GPT_E_out.item_B + Mean_GPT_E_out.item_O + Mean_GPT_E_out.item_S +
                     Mean_GPT_P_out.item_A + Mean_GPT_P_out.item_B + Mean_GPT_P_out.item_O + Mean_GPT_P_out.item_S +
                     Mean_GPT_A_out.item_A + Mean_GPT_A_out.item_B + Mean_GPT_A_out.item_O + Mean_GPT_A_out.item_S +
                     ### interaction terms are defined following Smith-Lovin 1987 - SEE FORMULA 2 and 3
                     Mean_GPT_E_out.item_B * Mean_GPT_E_out.item_O
                   , data = regr_data)

res_object_p_centered <- lm(Mean_GPT_P_in.item_O ~
                              mean_eva_out.Subject_centered + mean_eva_out.Behavior_centered + mean_eva_out.Object_centered + mean_eva_out.Setting_centered +
                              mean_pot_out.Subject_centered + mean_pot_out.Behavior_centered + mean_pot_out.Object_centered + mean_pot_out.Setting_centered +
                              mean_act_out.Subject_centered + mean_act_out.Behavior_centered + mean_act_out.Object_centered + mean_act_out.Setting_centered +
                              ### interaction terms are defined following Smith-Lovin 1987 - SEE FORMULA 2 and 3
                              mean_eva_out.Behavior_centered * mean_eva_out.Object_centered
                            , data = regr_data)

# Extract summaries of both models
summary_res_object_p <- summary(res_object_p)
summary_res_object_p_centered <- summary(res_object_p_centered)

# Get coefficients and p-values
coefficients_res_object_p <- summary_res_object_p$coefficients
coefficients_res_object_p_centered <- summary_res_object_p_centered$coefficients

# Create a data frame for comparison
comparison_df_obj_P <- data.frame(
  Effect = row.names(coefficients_res_object_p),
  Coefficient_Original = coefficients_res_object_p[, "Estimate"],
  P_value_Original = coefficients_res_object_p[, "Pr(>|t|)"],
  Coefficient_Centered = coefficients_res_object_p_centered[, "Estimate"],
  P_value_Centered = coefficients_res_object_p_centered[, "Pr(>|t|)"]
)

# Function to mark significant coefficients
mark_significance <- function(coefficient, p_value) {
  if (p_value < 0.05) {
    return(paste0(coefficient, "*"))
  } else {
    return(as.character(coefficient))
  }
}

# Apply the function to the coefficients
comparison_df_obj_P$Coefficient_Original <- mapply(mark_significance, 
                                                   comparison_df_obj_P$Coefficient_Original, 
                                                   comparison_df_obj_P$P_value_Original)

comparison_df_obj_P$Coefficient_Centered <- mapply(mark_significance, 
                                                   comparison_df_obj_P$Coefficient_Centered, 
                                                   comparison_df_obj_P$P_value_Centered)

# Remove p-values from the table as significance is now indicated
comparison_df_obj_P$P_value_Original <- NULL
comparison_df_obj_P$P_value_Centered <- NULL

# Use View to inspect the table
View(comparison_df_obj_P)

# activity
res_object_a <- lm(Mean_GPT_A_in.item_O ~
                     Mean_GPT_E_out.item_A + Mean_GPT_E_out.item_B + Mean_GPT_E_out.item_O + Mean_GPT_E_out.item_S +
                     Mean_GPT_P_out.item_A + Mean_GPT_P_out.item_B + Mean_GPT_P_out.item_O + Mean_GPT_P_out.item_S +
                     Mean_GPT_A_out.item_A + Mean_GPT_A_out.item_B + Mean_GPT_A_out.item_O + Mean_GPT_A_out.item_S +
                     ### interaction terms are defined following Smith-Lovin 1987 - SEE FORMULA 2 and 3
                     Mean_GPT_E_out.item_B * Mean_GPT_E_out.item_O
                   , data = regr_data)

res_object_a_centered <- lm(Mean_GPT_A_in.item_O ~
                              mean_eva_out.Subject_centered + mean_eva_out.Behavior_centered + mean_eva_out.Object_centered + mean_eva_out.Setting_centered +
                              mean_pot_out.Subject_centered + mean_pot_out.Behavior_centered + mean_pot_out.Object_centered + mean_pot_out.Setting_centered +
                              mean_act_out.Subject_centered + mean_act_out.Behavior_centered + mean_act_out.Object_centered + mean_act_out.Setting_centered +
                              ### interaction terms are defined following Smith-Lovin 1987 - SEE FORMULA 2 and 3
                              mean_eva_out.Behavior_centered * mean_eva_out.Object_centered
                            , data = regr_data)

# Extract summaries of both models
summary_res_behavior_a <- summary(res_behavior_a)
summary_res_behavior_a_centered <- summary(res_behavior_a_centered)

# Get coefficients and p-values
coefficients_res_behavior_a <- summary_res_behavior_a$coefficients
coefficients_res_behavior_a_centered <- summary_res_behavior_a_centered$coefficients

# Create a data frame for comparison
comparison_df_obj_A <- data.frame(
  Effect = row.names(coefficients_res_behavior_a),
  Coefficient_Original = coefficients_res_behavior_a[, "Estimate"],
  P_value_Original = coefficients_res_behavior_a[, "Pr(>|t|)"],
  Coefficient_Centered = coefficients_res_behavior_a_centered[, "Estimate"],
  P_value_Centered = coefficients_res_behavior_a_centered[, "Pr(>|t|)"]
)

# Function to mark significant coefficients
mark_significance <- function(coefficient, p_value) {
  if (p_value < 0.05) {
    return(paste0(coefficient, "*"))
  } else {
    return(as.character(coefficient))
  }
}

# Apply the function to the coefficients
comparison_df_obj_A$Coefficient_Original <- mapply(mark_significance, 
                                                   comparison_df_obj_A$Coefficient_Original, 
                                                   comparison_df_obj_A$P_value_Original)

comparison_df_obj_A$Coefficient_Centered <- mapply(mark_significance, 
                                             comparison_df_obj_A$Coefficient_Centered, 
                                             comparison_df_obj_A$P_value_Centered)

# Remove p-values from the table as significance is now indicated
comparison_df_obj_A$P_value_Original <- NULL
comparison_df_obj_A$P_value_Centered <- NULL

# Use View to inspect the table
View(comparison_df_obj_A)


##################### REGRESSION FOR THE SETTING ##################### 
# evaluation
res_setting_e <- lm(Mean_GPT_E_in.item_S ~
                      Mean_GPT_E_out.item_A + Mean_GPT_E_out.item_B + Mean_GPT_E_out.item_O + Mean_GPT_E_out.item_S +
                      Mean_GPT_P_out.item_A + Mean_GPT_P_out.item_B + Mean_GPT_P_out.item_O + Mean_GPT_P_out.item_S +
                      Mean_GPT_A_out.item_A + Mean_GPT_A_out.item_B + Mean_GPT_A_out.item_O + Mean_GPT_A_out.item_S +
                      ### interaction terms are defined following Smith-Lovin 1987 - SEE FORMULA 2 and 3
                      Mean_GPT_E_out.item_B * Mean_GPT_E_out.item_O
                    , data = regr_data)

res_setting_e_centered <- lm(Mean_GPT_E_in.item_S ~
                               mean_eva_out.Subject_centered + mean_eva_out.Behavior_centered + mean_eva_out.Object_centered + mean_eva_out.Setting_centered +
                               mean_pot_out.Subject_centered + mean_pot_out.Behavior_centered + mean_pot_out.Object_centered + mean_pot_out.Setting_centered +
                               mean_act_out.Subject_centered + mean_act_out.Behavior_centered + mean_act_out.Object_centered + mean_act_out.Setting_centered +
                               ### interaction terms are defined following Smith-Lovin 1987 - SEE FORMULA 2 and 3
                               mean_eva_out.Behavior_centered * mean_eva_out.Object_centered
                             , data = regr_data)

# Extract summaries of both models
summary_res_setting_e <- summary(res_setting_e)
summary_res_setting_e_centered <- summary(res_setting_e_centered)

# Get coefficients and p-values
coefficients_res_setting_e <- summary_res_setting_e$coefficients
coefficients_res_setting_e_centered <- summary_res_setting_e_centered$coefficients

# Create a data frame for comparison
comparison_df_set_E <- data.frame(
  Effect = row.names(coefficients_res_setting_e),
  Coefficient_Original = coefficients_res_setting_e[, "Estimate"],
  P_value_Original = coefficients_res_setting_e[, "Pr(>|t|)"],
  Coefficient_Centered = coefficients_res_setting_e_centered[, "Estimate"],
  P_value_Centered = coefficients_res_setting_e_centered[, "Pr(>|t|)"]
)

# Function to mark significant coefficients
mark_significance <- function(coefficient, p_value) {
  if (p_value < 0.05) {
    return(paste0(coefficient, "*"))
  } else {
    return(as.character(coefficient))
  }
}

# Apply the function to the coefficients
comparison_df_set_E$Coefficient_Original <- mapply(mark_significance, 
                                                   comparison_df_set_E$Coefficient_Original, 
                                                   comparison_df_set_E$P_value_Original)

comparison_df_set_E$Coefficient_Centered <- mapply(mark_significance, 
                                                   comparison_df_set_E$Coefficient_Centered, 
                                                   comparison_df_set_E$P_value_Centered)

# Remove p-values from the table as significance is now indicated
comparison_df_set_E$P_value_Original <- NULL
comparison_df_set_E$P_value_Centered <- NULL

# Use View to inspect the table
View(comparison_df_set_E)


# potency
res_setting_p <- lm(Mean_GPT_P_in.item_S ~
                      Mean_GPT_E_out.item_A + Mean_GPT_E_out.item_B + Mean_GPT_E_out.item_O + Mean_GPT_E_out.item_S +
                      Mean_GPT_P_out.item_A + Mean_GPT_P_out.item_B + Mean_GPT_P_out.item_O + Mean_GPT_P_out.item_S +
                      Mean_GPT_A_out.item_A + Mean_GPT_A_out.item_B + Mean_GPT_A_out.item_O + Mean_GPT_A_out.item_S +
                      ### interaction terms are defined following Smith-Lovin 1987 - SEE FORMULA 2 and 3
                      Mean_GPT_E_out.item_B * Mean_GPT_E_out.item_O
                    , data = regr_data)

res_setting_p_centered <- lm(Mean_GPT_P_in.item_S ~
                               mean_eva_out.Subject_centered + mean_eva_out.Behavior_centered + mean_eva_out.Object_centered + mean_eva_out.Setting_centered +
                               mean_pot_out.Subject_centered + mean_pot_out.Behavior_centered + mean_pot_out.Object_centered + mean_pot_out.Setting_centered +
                               mean_act_out.Subject_centered + mean_act_out.Behavior_centered + mean_act_out.Object_centered + mean_act_out.Setting_centered +
                               ### interaction terms are defined following Smith-Lovin 1987 - SEE FORMULA 2 and 3
                               mean_eva_out.Behavior_centered * mean_eva_out.Object_centered
                             , data = regr_data)

# Extract summaries of both models
summary_res_setting_p <- summary(res_setting_p)
summary_res_setting_p_centered <- summary(res_setting_p_centered)

# Get coefficients and p-values
coefficients_res_setting_p <- summary_res_setting_p$coefficients
coefficients_res_setting_p_centered <- summary_res_setting_p_centered$coefficients

# Create a data frame for comparison
comparison_df_set_P <- data.frame(
  Effect = row.names(coefficients_res_setting_p),
  Coefficient_Original = coefficients_res_setting_p[, "Estimate"],
  P_value_Original = coefficients_res_setting_p[, "Pr(>|t|)"],
  Coefficient_Centered = coefficients_res_setting_p_centered[, "Estimate"],
  P_value_Centered = coefficients_res_setting_p_centered[, "Pr(>|t|)"]
)

# Function to mark significant coefficients
mark_significance <- function(coefficient, p_value) {
  if (p_value < 0.05) {
    return(paste0(coefficient, "*"))
  } else {
    return(as.character(coefficient))
  }
}

# Apply the function to the coefficients
comparison_df_set_P$Coefficient_Original <- mapply(mark_significance, 
                                                   comparison_df_set_P$Coefficient_Original, 
                                                   comparison_df_set_P$P_value_Original)

comparison_df_set_P$Coefficient_Centered <- mapply(mark_significance, 
                                                   comparison_df_set_P$Coefficient_Centered, 
                                                   comparison_df_set_P$P_value_Centered)

# Remove p-values from the table as significance is now indicated
comparison_df_set_P$P_value_Original <- NULL
comparison_df_set_P$P_value_Centered <- NULL

# Use View to inspect the table
View(comparison_df_set_P)

# activity
res_setting_a <- lm(Mean_GPT_A_in.item_S ~
                      Mean_GPT_E_out.item_A + Mean_GPT_E_out.item_B + Mean_GPT_E_out.item_O + Mean_GPT_E_out.item_S +
                      Mean_GPT_P_out.item_A + Mean_GPT_P_out.item_B + Mean_GPT_P_out.item_O + Mean_GPT_P_out.item_S +
                      Mean_GPT_A_out.item_A + Mean_GPT_A_out.item_B + Mean_GPT_A_out.item_O + Mean_GPT_A_out.item_S +
                      ### interaction terms are defined following Smith-Lovin 1987 - SEE FORMULA 2 and 3
                      Mean_GPT_E_out.item_B * Mean_GPT_E_out.item_O
                    , data = regr_data)

res_setting_a_centered <- lm(Mean_GPT_A_in.item_S ~
                               mean_eva_out.Subject_centered + mean_eva_out.Behavior_centered + mean_eva_out.Object_centered + mean_eva_out.Setting_centered +
                               mean_pot_out.Subject_centered + mean_pot_out.Behavior_centered + mean_pot_out.Object_centered + mean_pot_out.Setting_centered +
                               mean_act_out.Subject_centered + mean_act_out.Behavior_centered + mean_act_out.Object_centered + mean_act_out.Setting_centered +
                               ### interaction terms are defined following Smith-Lovin 1987 - SEE FORMULA 2 and 3
                               mean_eva_out.Behavior_centered * mean_eva_out.Object_centered
                             , data = regr_data)

# Extract summaries of both models
summary_res_setting_a <- summary(res_setting_a)
summary_res_setting_a_centered <- summary(res_setting_a_centered)

# Get coefficients and p-values
coefficients_res_setting_a <- summary_res_setting_a$coefficients
coefficients_res_setting_a_centered <- summary_res_setting_a_centered$coefficients

# Create a data frame for comparison
comparison_df_set_A <- data.frame(
  Effect = row.names(coefficients_res_setting_a),
  Coefficient_Original = coefficients_res_setting_a[, "Estimate"],
  P_value_Original = coefficients_res_setting_a[, "Pr(>|t|)"],
  Coefficient_Centered = coefficients_res_setting_a_centered[, "Estimate"],
  P_value_Centered = coefficients_res_setting_a_centered[, "Pr(>|t|)"]
)

# Function to mark significant coefficients
mark_significance <- function(coefficient, p_value) {
  if (p_value < 0.05) {
    return(paste0(coefficient, "*"))
  } else {
    return(as.character(coefficient))
  }
}

# Apply the function to the coefficients
comparison_df_set_A$Coefficient_Original <- mapply(mark_significance, 
                                                   comparison_df_set_A$Coefficient_Original, 
                                                   comparison_df_set_A$P_value_Original)

comparison_df_set_A$Coefficient_Centered <- mapply(mark_significance, 
                                                   comparison_df_set_A$Coefficient_Centered, 
                                                   comparison_df_set_A$P_value_Centered)

# Remove p-values from the table as significance is now indicated
comparison_df_set_A$P_value_Original <- NULL
comparison_df_set_A$P_value_Centered <- NULL

# Use View to inspect the table
View(comparison_df_set_A)

print(res_setting_a)



###################### CALCULATE DEFLECTION FOR EACH EVENT
# Formula defined following Heise, 2007 and Schr?der 2011 and extended to include Settings
calc_deflection <- function(x) {
  deflection <- (x$Mean_GPT_E_in.item_A - x$Mean_GPT_E_out.item_A)^2 +
    (x$Mean_GPT_P_in.item_A - x$Mean_GPT_P_out.item_A)^2 +
    (x$Mean_GPT_A_in.item_A - x$Mean_GPT_A_out.item_A)^2 +
    (x$Mean_GPT_E_in.item_B - x$Mean_GPT_E_out.item_B)^2 +
    (x$Mean_GPT_P_in.item_B - x$Mean_GPT_P_out.item_B)^2 +
    (x$Mean_GPT_A_in.item_B - x$Mean_GPT_A_out.item_B)^2 +
    (x$Mean_GPT_E_in.item_O - x$Mean_GPT_E_out.item_O)^2 +
    (x$Mean_GPT_P_in.item_O - x$Mean_GPT_P_out.item_O)^2 +
    (x$Mean_GPT_A_in.item_O - x$Mean_GPT_A_out.item_O)^2 +
    (x$Mean_GPT_E_in.item_S - x$Mean_GPT_E_out.item_S)^2 +
    (x$Mean_GPT_P_in.item_S - x$Mean_GPT_P_out.item_S)^2 +
    (x$Mean_GPT_A_in.item_S - x$Mean_GPT_A_out.item_S)^2
}

regr_data$deflection <- calc_deflection(regr_data)
