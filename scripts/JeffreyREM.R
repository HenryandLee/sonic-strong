library(relevent)
library(dplyr)

# Load Data
round1_data <- read.csv("/Users/haiqili/Desktop/SONIC/Summer/sonic-strong/raw-data/Team 7 Round 1.csv", header=TRUE)
round3_data <- read.csv("/Users/haiqili/Desktop/SONIC/Summer/sonic-strong/raw-data/Team 7 Round 3.csv", header=TRUE)


# Extract just the event data
round1_data <- data.frame(from=round1_data$user_id, to=round1_data$to, time=round1_data$time)
round3_data <- data.frame(from=round3_data$user_id, to=round3_data$to, time=round3_data$time)

# Drop where empty 
round1_data <- na.omit(round1_data)
round3_data <- na.omit(round3_data)

# Relabel Vero cases in round 3
round3_data[round3_data == "vero3_7_1"] <- 0
round3_data$from <- as.numeric(round3_data$from) 


### This is your function perpare_for_rem
prepare_for_rem <- function(curr_dataset){
  splitrow_data <- data.frame()
  
  for (i in 1:nrow(curr_dataset)) {
    # Check if the specified column contains a comma
    if (grepl(",", curr_dataset[i, "to"])) {
      # Split the column value at the comma
      split_vals <- strsplit(curr_dataset[i, "to"], ",")[[1]]
      
      # Trim leading and trailing spaces from the split values
      split_vals <- trimws(split_vals)
      
      # Create new rows for each split value
      timelag <- 0
      for (val in split_vals) {
        new_row <- curr_dataset[i,]
        new_row["time"] <- curr_dataset[i,"time"] + timelag
        new_row["to"] <- val
        splitrow_data <- rbind(splitrow_data, new_row)
        timelag <- timelag + 0.1
      }
    } else {
      # If no comma, just add the original row to the splitrow_data
      splitrow_data <- rbind(splitrow_data, curr_dataset[i,])
    }
  }
  
  for (i in 1:nrow(splitrow_data)) {
    if (splitrow_data[i, "to"] == "all") {
      splitrow_data[i, "to"] <- 1
    }
  }
  
  splitrow_data$to <- as.numeric(splitrow_data$to)
  
  splitrow_data <- splitrow_data |> 
    dplyr::mutate(time = time - min(time)) 
  #dplyr::select(time, from, to) |> ###*** the following lines are unnecessary
  #dplyr::arrange(time)             ###*** they turned time into row numbers
  #dplyr::mutate(time = dplyr::row_number()-1)  ###*** which is not what we want
  
  return(splitrow_data)
}


###*** same time adjustment function
adjust_time <- function(df) {
  for (i in 2:nrow(df)) { # Start from the second row
    if (df$time[i] == df$time[i - 1]) {
      df$time[i] <- df$time[i] + 0.1
    }
  }
  return(df)
}


# Split rows and clean alls 
round1_data_prepared <- prepare_for_rem(round1_data)
round3_data_prepared <- prepare_for_rem(round3_data)
rownames(round1_data_prepared) = NULL ###*** remove rownames from your use of dplyr::mutate function
rownames(round3_data_prepared) = NULL
round1_data_prepared = round1_data_prepared[,c(3,1,2)] ###*** rearrange columns in the order of "time", "from", "to"
round3_data_prepared = round3_data_prepared[,c(3,1,2)]
round1_data_prepared <- adjust_time(round1_data_prepared) ###*** same time adjustment
round3_data_prepared <- adjust_time(round3_data_prepared)



###*** map Participant IDs into 1,2,3,4,...
map_ids_to_integers <- function(df, from_col, to_col) {
  # Convert specified columns to character type
  df[[from_col]] <- as.character(df[[from_col]])
  df[[to_col]] <- as.character(df[[to_col]])
  
  # Extract unique IDs from both specified columns
  unique_ids <- unique(c(df[[from_col]], df[[to_col]]))
  
  # Create a mapping of unique IDs to integers
  id_mapping <- setNames(1:length(unique_ids), unique_ids)
  
  # Apply the mapping to the specified columns
  df[[from_col]] <- id_mapping[df[[from_col]]]
  df[[to_col]] <- id_mapping[df[[to_col]]]
  
  # Create and print the mapping table
  mapping_table <- data.frame(ID = names(id_mapping), Integer = id_mapping)
  print(mapping_table)
  
  # Return the adjusted dataframe
  return(list(adjusted_df = df, mapping_table = mapping_table))
}


round1_data_prepared <- map_ids_to_integers(round1_data_prepared, "from", "to")
round3_data_prepared <- map_ids_to_integers(round3_data_prepared, "from", "to")


### Run REM
edgelist_df = round3_data_prepared$adjusted_df
mapping_table = round3_data_prepared$mapping_table
model1 <- rem.dyad(
  edgelist = edgelist_df ,
  effects = c("RSndSnd", "RRecSnd", "CovSnd","PSAB-BA","PSAB-BY"), 
  n = max(c(edgelist_df$from,edgelist_df$to)),
  covar = list(CovSnd = 1),
  ordinal = FALSE, 
  hessian = TRUE)

# Remember CovSnd.1 is the model intercept
summary(model1)





