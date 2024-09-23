

file_list <- list.files(path = "C:/Users/lukas/Dropbox/Determ_mediaspace/2021", pattern = "*.xlsx", full.names = TRUE)


first_dates <- gsub(".*op_e_([0-9]{2}-[0-9]{2}-[0-9]{4})_.*", "\\1", file_list)
second_dates <- gsub(".*_([0-9]{2}-[0-9]{2}-[0-9]{4})\\.xlsx", "\\1", file_list)

# Convert the string dates into Date objects
first_dates <- as.Date(first_dates, format = "%d-%m-%Y")
second_dates <- as.Date(second_dates, format = "%d-%m-%Y")

# Create a data frame with file paths and the two dates
file_df <- data.frame(File_Path = file_list, First_Date = first_dates, Second_Date = second_dates)

# Sort the data frame by the first date in ascending order
sorted_file_df <- file_df[order(file_df$First_Date), ]


# Combine all dates into a single vector
all_dates <- unique(c(file_df$First_Date, file_df$Second_Date))

# Find the range of dates from the earliest to the latest
min_date <- min(all_dates)
max_date <- max(all_dates)

# Generate a sequence of all dates that should be covered
expected_dates <- seq(from = min_date, to = max_date, by = "day")

# Check which dates are missing
missing_dates <- setdiff(expected_dates, all_dates)


# Output the missing datesif (length(missing_dates) > 0) {
if (length(missing_dates) > 0) {
  print("Missing dates:")
  # Convert numeric dates back to readable date format before printing
  print(as.Date(missing_dates, origin = "1970-01-01"))
} else {
  print("All days are covered.")
}





# Load necessary libraries
library(readxl)
library(dplyr)
library(data.table)
# Load the readxl package
library(readxl)
library(lubridate)



file_list <- list.files(path = "C:/Users/lukas/Dropbox/Determ_mediaspace/2024", pattern = "*.xlsx", full.names = TRUE)


# Function to read specific columns from each file with progress printing
read_specific_columns_with_progress <- function(file_list, columns_to_read) {
  total_files <- length(file_list)
  data_list <- list()

  for (i in seq_along(file_list)) {
    cat(sprintf("Processing file %d of %d: %s\n", i, total_files, file_list[i]))

    # Read the specific columns from the file
    data_list[[i]] <- read_excel(file_list[i], col_names = TRUE, .name_repair = "unique") %>%
      select(all_of(columns_to_read))
  }

  # Optionally, name the list elements by their respective file names
  names(data_list) <- file_list

  return(data_list)
}




# Specify the columns to read
columns_to_read <- c("DATE", "TITLE", "URL")

# Call the function to read specific columns from the files
data_names <- read_specific_columns_with_progress(file_list, columns_to_read)

dns<- bind_rows(data_names)







library(lubridate)

# Assuming your dataframe is named df and has a column named 'date'
# Convert the date column to Date type if it's not already
radno <- dns %>%
  mutate(date = as.Date(DATE)) %>%
  arrange(desc(date))

# Specify the year you want to check
year_to_check <- 2024

# Generate a sequence of all dates in the specified year
all_dates <- seq.Date(from = as.Date(paste0(year_to_check, "-01-01")),
                      to = as.Date(paste0(year_to_check, "-05-31")),
                      by = "day")

# Find the missing dates
missing_dates <- setdiff(all_dates, radno$date)

# Output the missing dates
missing_dates <- as.Date(missing_dates, origin = "1970-01-01")

# Output the missing dates
print(missing_dates)














