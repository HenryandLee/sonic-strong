## REM 0407.R 
## input: 
## output: 
## purpose: 

library(dplyr)
library(tidyr)
library(relevent)

testdata <- read.csv("~/Desktop/SONIC/Summer/sonic-strong/cleaned-data-as-input/03_06_2021AM_vero #1_task+team_round 1_otter.ai.csv")


# 1. Remove NA entries
testdata <- testdata %>%
  filter(!is.na(user) & !is.na(to))

# 2. List of all participants (excluding Vero)
participants <- unique(testdata$user)
participants <- participants[!participants %in% c("Vero", "all")]

# 3. List of participants + Vero
participants_with_vero <- if("Vero" %in% participants) {
  c("Vero", participants)
} else {
  participants
}


# 3. Expand "to" column and handle different cases
testdata <- testdata %>%
  separate_rows(to, sep = ",") %>%
  mutate(to = gsub("Participant ", "", to),  # Remove "Participant " prefix from "to"
         to = ifelse(to == "0", "Vero", to))  # Replace "0" with "Vero"

# 4. Remove "Participant " from user column
testdata$user <- gsub("Participant ", "", testdata$user)


# 5. Process time and add tiny gaps for sequence
testdata <- testdata %>%
  group_by(user, min, sec) %>%
  mutate(sec = sec + (row_number() - 1) * 0.1) %>%
  ungroup()

# 6. Handle "all" case and ensure consistent formatting
testdata <- testdata %>%
  mutate(to = ifelse(to == "all", paste(participants_with_vero, collapse = ", "), to)) %>%
  separate_rows(to, sep = ", ") %>%
  mutate(to = gsub("Participant ", "", to),  # Remove "Participant " prefix from "to"
         to = ifelse(to == "0", "Vero", to))  # Replace "0" with "Vero"

# 7. Convert "min" and "sec" to a single "time" column (in seconds)
testdata$time <- testdata$min * 60 + testdata$sec

# 8. Add a binary indicator for whether the sender or receiver is "Vero"
testdata <- testdata %>%
  mutate(vero_sender = ifelse(user == "Vero", 1, 0),
         vero_receiver = ifelse(to == "Vero", 1, 0))

# 9. Final filtering to remove rows where sender and receiver are the same (extra rows we created from "all")
testdata <- testdata %>%
  filter(user != to)

# 10. Split the task_pos. Because the sheet is in "perfected" format, task_conf, rel_pos, rel_conf entries
# with commas are also automatically split.  
for (i in 1:(nrow(testdata) - 1)) {
  if (grepl("^\\d+,\\d+$", testdata$task_pos[i]) && 
      testdata$task_pos[i] == testdata$task_pos[i + 1]) {
    parts <- strsplit(testdata$task_pos[i], ",")[[1]]
    A <- as.integer(parts[1])
    B <- as.integer(parts[2])
    testdata$task_pos[i] <- A
    testdata$task_pos[i + 1] <- B
  }
}


# 11. Prepare data for REM
rem_data <- testdata %>%
  select(time, user, to, task_pos,X3rdVero)
names(rem_data) = c("time","sender","receiver","task_pos", "X3rdVero")
rem_data$increment <- 1


### 12. (optional) Set initial time to 0 
rem_data$time = rem_data$time - min(rem_data$time) 





# Prepare the event data as an edgelist matrix
edgelist <- as.matrix(rem_data[, c("time", "sender", "receiver")])
colnames(edgelist) <- c("time", "senderID", "receiverID")

# Convert sender and receiver IDs to integer factors
edgelist <- as.data.frame(edgelist)

# Explicitly convert the "senderID" column to a factor
edgelist$senderID <- as.factor(edgelist$senderID)
edgelist$receiverID <- as.factor(edgelist$receiverID)

# Create the mapping with factor levels as names and integers as values
user_to_number_mapping <- setNames(as.character(seq_along(levels(edgelist$senderID))), levels(edgelist$senderID))

# Now convert the factors to integers
edgelist$senderID <- as.integer(edgelist$senderID)
edgelist$receiverID <- as.integer(edgelist$receiverID)


edgelist_df <- as.data.frame(edgelist, stringsAsFactors = FALSE)
edgelist_df <- data.frame(lapply(edgelist_df, as.numeric))


# Check the structure of the edgelist to confirm that all columns are numeric
str(edgelist_df)

############## Introduce randomness for equal times and apply tiny increments
edgelist_df <- edgelist_df %>%
  group_by(time) %>%
  mutate(
    random_order = sample(row_number()), # Randomize order within each time group
    time = time + (random_order - 1) * 0.001 # Apply increments based on random order
  ) %>%
  ungroup() %>%
  select(-random_order)

edgelist_df <- edgelist_df %>% arrange(time)
head(edgelist_df,2)
##################################


effects = c("CovSnd","RRecSnd","RSndSnd","NTDegRec","PSAB-XA") 
### "CovSnd" is a self-defined covariate, well be used as intercept
rem_model_dyad <- rem.dyad(
  edgelist = edgelist_df,
  effects = effects, n = max(c(edgelist_df$senderID,edgelist_df$receiverID)),
  covar =list(CovSnd = 1), ### define "CovSnd" as a list of 1's as intercept
  ordinal=FALSE, hessian = TRUE
)

# Check the model summary
summary(rem_model_dyad)


