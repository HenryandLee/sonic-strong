## ----setup, include = FALSE----------------------------------------------------------------------------------
library(here)
library(dplyr) 
library(tidyr)
library(purrr)
library(relevent)


## ------------------------------------------------------------------------------------------------------------
# Capture the arguments passed to the script and validate number of arguments
args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 1) {
  stop("Please provide the input file name as an argument.")
}

input_name <- args[1]
file_path <- here("evan-format-input", input_name)
testdata <- read.csv(file_path)


## ------------------------------------------------------------------------------------------------------------
testdata <- testdata %>%
  filter(!is.na(user) & !is.na(to))


## ------------------------------------------------------------------------------------------------------------
participants <- unique(testdata$user)
participants <- participants[!participants %in% c("Vero", "all")]


## ------------------------------------------------------------------------------------------------------------
participants_with_vero <- if("Vero" %in% participants) {
  c(participants)
} else {
  c("Vero", participants)
}


## ------------------------------------------------------------------------------------------------------------
testdata <- testdata %>%
  separate_longer_delim(c(to, task_pos, task_conf, rel_pos, rel_conf, X3rdVero), delim = ",")


## ------------------------------------------------------------------------------------------------------------
testdata$user <- gsub("Participant ", "", testdata$user)


## ------------------------------------------------------------------------------------------------------------
set.seed(123)
testdata <- testdata %>%
  group_by(time) %>%
  mutate(time = time + runif(n(), min = 0, max = 0.1)) %>% # check this max = 1
  ungroup() %>%
  arrange(time)


## ------------------------------------------------------------------------------------------------------------
testdata <- testdata %>%
  mutate(
    to = if_else(to == "all", "1", to),  # Replace "all" with "1"
    to = as.numeric(to),                 # Convert "to" to numeric
    time = time - min(time)              # Adjust "time" column to 0
  )

map_ids_to_integers <- function(df, from_col, to_col) {
  # Convert specified columns to character type and gather unique IDs
  unique_ids <- df %>%
    select(all_of(c(from_col, to_col))) %>%
    mutate(across(everything(), as.character)) %>%
    unlist() %>%
    unique()
  filtered_ids <- sort(unique_ids[unique_ids != 1 & unique_ids != "Vero" & unique_ids != 0])

# Create the dataframe conditionally based on the number of IDs
  if (length(filtered_ids) == 2) {
    id_rowvec <- data.frame(participant1 = filtered_ids[1], participant2 = filtered_ids[2])} 
  else if (length(filtered_ids) == 3) {
    id_rowvec <- data.frame(participant1 = filtered_ids[1], participant2 = filtered_ids[2], participant3 = filtered_ids[3])} 
  else {
    id_rowvec <- data.frame()}
  print(id_rowvec)
  # Create a mapping of unique IDs to integers
  id_mapping <- tibble(ID = unique_ids, Integer = seq_along(unique_ids))
  
  # Apply the mapping to the specified columns
  df <- df %>%
    mutate(across(all_of(c(from_col, to_col)), ~ id_mapping$Integer[match(.x, id_mapping$ID)]))

  # Return the adjusted df and mapping table
  return(list(adjusted_df = df, mapping_table = id_mapping, id_rowvec = id_rowvec))
}

# Example usage with test data
test_attribute <- tibble(team_id = testdata$Team_id[1], Vero_type = testdata$condition[1])
test_data_run <- tibble(time = testdata$time, from = testdata$user, to = testdata$to)
data_prepared <- map_ids_to_integers(test_data_run, "from", "to")


## ------------------------------------------------------------------------------------------------------------
edgelist_df = data_prepared$adjusted_df
mapping_table = data_prepared$mapping_table
id_rowvec = data_prepared$id_rowvec
model1 <- rem.dyad(
  edgelist = edgelist_df ,
  effects = c("RSndSnd", "RRecSnd", "CovSnd","PSAB-BA","PSAB-BY", "PSAB-XA", "PSAB-XB"), 
  n = max(c(edgelist_df$from,edgelist_df$to)),
  covar = list(CovSnd = 1),
  ordinal = FALSE, 
  hessian = TRUE)

# Remember CovSnd.1 is the model intercept
summary(model1)


## ------------------------------------------------------------------------------------------------------------
coef <- model1$par
SE <- sqrt(diag(model1$cov))
z_values <- coef / SE
p_values <- 2 * pnorm(-abs(as.numeric(z_values)))
AIC <- model1$AIC
BIC <- model1$BIC
team_id <- testdata$Team_id[1]
round_number <- testdata$round[1]
Vero_type <- testdata$condition[1]
table <- cbind(Team_ID = team_id, Round_Number = round_number, Covariate = rownames(as.data.frame(model1$coef)), Estimate = coef, SE = SE, Z_value = z_values, p_value = p_values, Significant_at_0.001 = ifelse(p_values < 0.001, TRUE, FALSE), Significant_at_0.05 = ifelse(p_values < 0.05, TRUE, FALSE), AIC = AIC, BIC = BIC, id_rowvec, Vero_type) # names of covariates

# if (!is.null(table)) {
#   print(head(table))
# }

# output model statistics file
file_name <- paste0("model_coefficients_", team_id, "_round_", round_number, ".csv")
output_file_path <- here::here("output", file_name)
write.csv(table, file = output_file_path, row.names = FALSE)

# Print message
print(id_rowvec)
print(paste("Processed and saved:", file_name))

