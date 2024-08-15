### Visualization 
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

# Assuming all your CSV files follow the naming pattern "model_coefficients_<team>_round_<round>.csv"
# and they are stored in the same directory

# Define the folder path containing the CSV files
folder_path <- here("output")

# Get a list of all CSV files in the folder
csv_files <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)

# Read and combine all CSV files, ensuring Team_ID is always a character
combined_data <- csv_files %>%
  lapply(function(file) {
    df <- read.csv(file)
    df$Team_ID <- as.character(df$Team_ID)  # Ensure Team_ID is a character
    return(df)
  }) %>%
  bind_rows(.id = "file_id") %>%
  filter(!is.na(Team_ID))

# Count how many times each coefficient is significant and not significant across rounds
significance_summary <- combined_data %>%
  group_by(Covariate, Team_ID, Vero_type) %>%
  summarize(
    Significant_Count = sum(Significant_at_0.001),
    Not_Significant_Count = n() - Significant_Count,
    Mean_Estimate_If_Significant = mean(Estimate[Significant_at_0.001 == TRUE], na.rm = TRUE)
  ) %>%
  ungroup()

# mean viz - box plot
ggplot(significance_summary, aes(x = Covariate, y = Mean_Estimate_If_Significant)) +
  geom_boxplot() +
  labs(
    title = "Distribution of Mean Estimate (If Significant) for Each Covariate",
    x = "Covariate",
    y = "Mean Estimate (If Significant)"
  ) +
  theme_minimal()

# # mean visualization - density plot
# ggplot(significance_summary, aes(x = Mean_Estimate_If_Significant, fill = Covariate)) +
#   geom_density(alpha = 0.5) +
#   labs(
#     title = "Density Plot of Mean Estimate (If Significant) for Each Covariate",
#     x = "Mean Estimate (If Significant)",
#     y = "Density"
#   ) +
#   theme_minimal()

# Summarize across teams to get overall significance counts per coefficient
overall_significance <- significance_summary %>%
  group_by(Covariate) %>%
  summarize(
    Total_Significant = sum(Significant_Count),
    Total_Not_Significant = sum(Not_Significant_Count), 
    Percent_Significant = Total_Significant / (Total_Significant + Total_Not_Significant),
    Mean_Estimate_If_Significant = mean(Mean_Estimate_If_Significant, na.rm = TRUE)
  )

# Visualization 1
ggplot(overall_significance, aes(x = Covariate)) +
  geom_bar(aes(y = Total_Significant), stat = "identity", fill = "blue", alpha = 0.7) +
  geom_bar(aes(y = -Total_Not_Significant), stat = "identity", fill = "red", alpha = 0.7) +
  coord_flip() +
  labs(
    title = "Significance of Coefficients Across Rounds",
    x = "Coefficient",
    y = "Number of Rounds",
    fill = "Significance"
  ) +
  theme_minimal()

# Mean estimate viz
ggplot(overall_significance, aes(x = Covariate)) +
  geom_bar(aes(y = Mean_Estimate_If_Significant), stat = "identity", fill = "blue", alpha = 0.7) +
  labs(
    title = "Mean of Coefficients (if Significant)",
    x = "Coefficient",
    y = "Log-likelihood", 
    fill = "Significance"
  ) +
  theme_minimal()

# Across Verotypes and Coefficients
overall_significance_by_type <- significance_summary %>%
  group_by(Covariate, Vero_type) %>%
  summarize(
    Total_Significant = sum(Significant_Count),
    Total_Not_Significant = sum(Not_Significant_Count), 
    Percent_Significant = Total_Significant / (Total_Significant + Total_Not_Significant),
    Mean_Estimate_If_Significant = mean(Mean_Estimate_If_Significant, na.rm = TRUE)
  )


# Visualization
ggplot(overall_significance_by_type, aes(x = Covariate, y = Percent_Significant, fill = Vero_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Percent_Significant, 1)), 
            position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(
    title = "Significance Percentage of Covariates by Vero_type",
    x = "Covariate",
    y = "Percent_Significant",
    fill = "Vero_type"
  ) +
  theme_minimal()

ggplot(overall_significance_by_type, aes(x = Covariate, y = Mean_Estimate_If_Significant, fill = Vero_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Mean_Estimate_If_Significant,2)), 
            position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(
    title = "Mean of Covariates by Vero_type",
    x = "Covariate",
    y = "Estimate",
    fill = "Vero_type"
  ) +
  theme_minimal()


# Save the plot to a file
ggsave("coefficient_significance_summary.png", width = 10, height = 6)
ggsave("significance_percentage_by_vero_type.png", width = 10, height = 6)

