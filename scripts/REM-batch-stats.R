library(here)

# Define the folder containing the CSV files
folder_path <- here("evan-format-input")

# Get a list of all CSV files in the folder
csv_files <- list.files(path = folder_path, pattern = "*.csv", full.names = FALSE)

print("Starting batch processing...")  # Debugging output
print(csv_files)
# Loop over each CSV file
for (each in csv_files) {
  # Extract the base name of the file (without path and extension)
  print(paste("Processing file:", each))  # Debugging output
  # Run the processing script on the current file
  command <- paste("Rscript REM-Henry.R", shQuote(each))
  system(command)
}
print("Batch processing completed!")