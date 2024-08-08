library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(readr)

# Function to check if all raters agree on the number of commas
check_comma_agreement <- function(rows) {
  if (length(rows) == 0) return(FALSE)
  
  comma_counts <- sapply(rows, function(row) stringr::str_count(row$to, ","))
  return(length(unique(comma_counts)) == 1)
}

# Function to split a row based on commas
split_row <- function(row) {
  recipients <- unlist(strsplit(as.character(row$to), ","))
  n_recipients <- length(recipients)
  
  # Columns to split
  split_columns <- c("to", "task_pos", "task_conf", "rel_pos", "rel_conf", "3rdVero")
  
  # Create new rows
  new_rows <- lapply(1:n_recipients, function(i) {
    new_row <- row
    for (col in split_columns) {
      values <- unlist(strsplit(as.character(row[[col]]), ","))
      new_row[[col]] <- values[i]
    }
    new_row
  })
  
  return(new_rows)
}

# Function to adjust data for inconsistent comma counts
adjust_data <- function(row, max_commas_row) {
  max_commas <- str_count(max_commas_row$to, ",")
  current_commas <- str_count(row$to, ",")
  
  if (current_commas < max_commas) {
    split_columns <- c("to", "task_pos", "task_conf", "rel_pos", "rel_conf", "3rdVero")
    
    # First, adjust the 'to' column
    to_values <- unlist(strsplit(as.character(row$to), ","))
    to_options <- lapply(0:(max_commas + 1 - length(to_values)), function(n_front) {
      n_back <- max_commas + 1 - length(to_values) - n_front
      c(rep(to_values[1], n_front), to_values, rep(to_values[length(to_values)], n_back))
    })
    
    # Choose the option with maximum matches to max_commas_row for 'to'
    max_matches <- which.max(sapply(to_options, function(opt) {
      sum(opt == unlist(strsplit(as.character(max_commas_row$to), ",")))
    }))
    
    best_to_option <- to_options[[max_matches]]
    row$to <- paste(best_to_option, collapse = ",")
    
    # Now adjust other columns based on the best 'to' option
    for (col in split_columns[-1]) {  # Exclude 'to' as it's already adjusted
      values <- unlist(strsplit(as.character(row[[col]]), ","))
      if (length(values) < max_commas + 1) {
        new_values <- numeric(max_commas + 1)
        for (i in 1:(max_commas + 1)) {
          original_index <- which(best_to_option[i] == to_values)
          new_values[i] <- values[min(original_index, length(values))]
        }
        row[[col]] <- paste(new_values, collapse = ",")
      }
    }
  }
  return(row)
}


# Main function to process all datasets
process_datasets <- function(datasets) {
  if (length(datasets) == 0) {
    stop("No datasets provided")
  }
  
  dimensions <- sapply(datasets, dim)
  if (!all(apply(dimensions, 1, function(x) length(unique(x)) == 1))) {
    stop("Datasets have different dimensions")
  }
  
  processed_datasets <- map(1:length(datasets), function(dataset_index) {
    processed_data <- map(1:nrow(datasets[[1]]), function(i) {
      current_rows <- map(datasets, ~.x[i,])
      
      cat("Processing row", i, "for dataset", dataset_index, "\n")
      print(sapply(current_rows, function(row) row$to))
      
      agreement <- check_comma_agreement(current_rows)
      cat("Comma agreement:", agreement, "\n")
      
      if (!agreement) {
        comma_counts <- sapply(current_rows, function(row) str_count(row$to, ","))
        max_commas <- which.max(comma_counts)
        max_commas_row <- current_rows[[max_commas]]
        
        current_rows <- map(current_rows, ~adjust_data(.x, max_commas_row))
      }
      
      return(split_row(current_rows[[dataset_index]]))
    })
    
    return(bind_rows(unlist(processed_data, recursive = FALSE)))
  })
  
  return(processed_datasets)
}

# if input argument is one sheet with entry "a,b,c"; then split into row with a,b,c
# if input argument is more than one sheet, align the format;A: "abc" B: "de" -> 
# A: "abc", B: "dee"/"dde" (whichever more closely matches the input by rater A)

# this script does not add time 

# Read the datasets
col_types <- cols(
  to = col_character(),
  task_pos = col_character(),
  task_conf = col_character(),
  rel_pos = col_character(),
  rel_conf = col_character(),
  `3rdVero` = col_character(),
  .default = col_guess()
)

# dataset3 <- read_csv("C:/Users/lds_l/Downloads/perfectdata_EVan.csv", col_types = col_types)
# dataset1 <- read_csv("C:/Users/lds_l/Downloads/perfectdata_test.csv", col_types = col_types)
# dataset2 <- read_csv("C:/Users/lds_l/Downloads/perfectdata_Emil.csv", col_types = col_types)


# Process the datasets
# datasets <- list(dataset1, dataset2, dataset3)

dataset4 <- read_csv("/Users/haiqili/Desktop/SONIC/STRONG/Week 7 Assignments/cleaned_data/cleaned_1.csv", col_types =  col_types)
datasets_new <- list(dataset4)
results <- process_datasets(datasets_new)

# Print the results

## TODO: Delete rows where is.na(user) = TRUE 
# Write the results to separate CSV files
for (i in 1:length(results)) {
write_csv(results[[i]], paste0("/Users/haiqili/Desktop/SONIC/STRONG/Week 7 Assignments/cleaned_data/cleaned_1_result_", i, ".csv"))
}

results[[1]][1:5,18:23]
# results[[2]][1:5,18:23]
# results[[3]][1:5,18:23]



