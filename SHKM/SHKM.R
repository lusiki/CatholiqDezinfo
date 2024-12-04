
library(readxl)
library(dplyr)
library(stringi)
library(data.table)
library(stringr)


file_list <- list.files(path = "C:/Users/lukas/Dropbox/Determ_mediaspace/2024", pattern = "*.xlsx", full.names = TRUE)


# Function to read a single Excel file
read_single_file <- function(file_path) {
  read_excel(file_path)
}

# Read all files and combine into one data frame
all_data <- lapply(file_list, read_single_file) %>%
  bind_rows()


all_data <- as.data.table(all_data)
# View the combined data
#print(all_data)

# Replace 'your_column_name' with the actual name of the column you want to search
your_column_name <- "TITLE"

# Define the function to check for matches
check_matches <- function(text, words_vector) {
  any(stri_detect_regex(text, words_vector, negate = FALSE))
}

# Define the words vector
words_vector <- c("Susret hrvatske katoličke mladeži", "SHKM", "Antunovski hod mladih",
  "Međunarodni festival mladih", "Mladifest", "Medjugorje Youth Festival",
  # Add other case variations and grammatical forms if needed
  "susret hrvatske katoličke mladeži", "antunovski hod mladih",
  "međunarodni festival mladih", "mladifest", "medjugorje youth festival")

# Define batch size
batch_size <- 1000

# Calculate the number of batches
num_batches <- ceiling(nrow(all_data) / batch_size)

# Loop through each batch
for (i in 1:num_batches) {

  start_time <- Sys.time()

  # Calculate the start and end row indices for the current batch
  start_idx <- (i - 1) * batch_size + 1
  end_idx <- min(i * batch_size, nrow(all_data))

  # Print the current batch number and row indices
  cat(sprintf("Processing batch %d (rows %d to %d)...\n", i, start_idx, end_idx))

  # Subset the data table for the current batch and apply the operations
  all_data[start_idx:end_idx, `:=` (
    has_word = sapply(.SD[[your_column_name]], check_matches, words_vector),
    matched_word = sapply(.SD[[your_column_name]], function(x) paste(unlist(stri_extract_all_regex(x, words_vector)), collapse=", "))
  ), .SDcols = your_column_name]

  batch_data <- all_data[start_idx:end_idx]

  end_time <- Sys.time()
  duration <- end_time - start_time

  # Print the duration for the current batch
  cat(sprintf("Batch %d processed in %f seconds.\n", i, duration))

  # Save or further process batch_data if necessary
}

mladez <- all_data[has_word==T,]



write.xlsx(mladez, "C:/Users/lukas/Dropbox/HKS/Projekti/Digitalni Kat/SHKM/sve_title_24.xlsx")
