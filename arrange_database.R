library("DBI")
library("RMySQL")
library("readxl")
library("readxl")
library("DBI")
library("lubridate")



conn <- dbConnect(RMySQL::MySQL(), dbname = "determ_all", host = "127.0.0.1",
                  user = "Lux", password = "Theanswer0207", local_infile = TRUE)

can_connect <- dbCanConnect(RMySQL::MySQL(), dbname = "determ", host = "127.0.0.1",
                            user = "Lux", password = "Theanswer0207")

### create table----
dbExecute(conn, "
CREATE TABLE media_space_2021 (
    DATE VARCHAR(10),                 -- assuming date as 'YYYY-MM-DD'
    TIME VARCHAR(8),                  -- assuming time as 'HH:MM:SS'
    TITLE TEXT,                       -- TEXT for longer string
    FROM_SITE VARCHAR(255),           -- renamed FROM to FROM_SITE to avoid SQL keyword conflict
    AUTHOR VARCHAR(255),
    URL TEXT,                         -- TEXT for potentially long URLs
    URL_PHOTO TEXT,                   -- TEXT for potentially long URLs
    SOURCE_TYPE VARCHAR(50),
    GROUP_NAME VARCHAR(50),
    KEYWORD_NAME VARCHAR(50),
    FOUND_KEYWORDS TEXT,              -- TEXT if potentially long
    LANGUAGES VARCHAR(10),
    LOCATIONS VARCHAR(10),
    TAGS BOOLEAN,                     -- BOOLEAN for logical (TRUE/FALSE) values
    MANUAL_SENTIMENT BOOLEAN,         -- BOOLEAN for logical (TRUE/FALSE) values
    AUTO_SENTIMENT VARCHAR(50),
    MENTION_SNIPPET TEXT,             -- TEXT for longer string
    REACH INT,                        -- INT for numeric values
    VIRALITY DECIMAL(10,2),           -- DECIMAL for numbers with decimals
    ENGAGEMENT_RATE DECIMAL(10,2),
    INTERACTIONS INT,
    FOLLOWERS_COUNT INT,
    LIKE_COUNT INT,
    COMMENT_COUNT INT,
    SHARE_COUNT INT,
    TWEET_COUNT BOOLEAN,              -- BOOLEAN for logical (TRUE/FALSE) values
    LOVE_COUNT INT,
    WOW_COUNT INT,
    HAHA_COUNT INT,
    SAD_COUNT INT,
    ANGRY_COUNT INT,
    TOTAL_REACTIONS_COUNT INT,
    FAVORITE_COUNT INT,
    RETWEET_COUNT INT,
    VIEW_COUNT INT,
    DISLIKE_COUNT BOOLEAN,            -- BOOLEAN for logical (TRUE/FALSE) values
    COUNT BOOLEAN,                    -- BOOLEAN for logical (TRUE/FALSE) values
    REPOST_COUNT BOOLEAN,             -- BOOLEAN for logical (TRUE/FALSE) values
    REDDIT_TYPE BOOLEAN,              -- BOOLEAN for logical (TRUE/FALSE) values
    REDDIT_SCORE BOOLEAN,             -- BOOLEAN for logical (TRUE/FALSE) values
    INFLUENCE_SCORE INT,
    TWEET_TYPE VARCHAR(50),
    TWEET_SOURCE_NAME BOOLEAN,        -- BOOLEAN for logical (TRUE/FALSE) values
    TWEET_SOURCE_URL BOOLEAN,         -- BOOLEAN for logical (TRUE/FALSE) values
    FULL_TEXT TEXT                    -- TEXT for longer string
)")


### arrange data ----

file_list <- list.files(path = "C:/Users/lukas/Dropbox/Determ_mediaspace/2023", pattern = "*.xlsx", full.names = TRUE)


# Function to extract and parse the first date in the filename
parse_date_from_filename <- function(filename) {
  # Extract the date using regex
  matches <- regmatches(filename, regexec("op_e_(\\d{2}-\\d{2}-\\d{4})", filename))
  date_str <- matches[[1]][2]

  # Parse the date string to a Date object
  dmy(date_str)
}

# Apply the function to each file, then sort
sorted_files <- file_list[order(sapply(file_list, parse_date_from_filename))]



# Assuming your database connection is stored in 'conn'
# and your table name is "media_space_2021"

for (file_path in sorted_files) {
  # Read the Excel file into a data frame
  df <- read_excel(file_path)

  # Rename the 'FROM' column to avoid SQL keyword conflict
  # Check if 'FROM' column exists and rename it
  if ("FROM" %in% names(df)) {
    names(df)[names(df) == "FROM"] <- "FROM_SITE"
  }

  # Optional: Convert all character columns to UTF-8 to avoid encoding issues
  for (i in 1:ncol(df)) {
    if (is.character(df[[i]])) {
      df[[i]] <- iconv(df[[i]], to = "UTF-8")
    }
  }

  # Write the data frame to the database
  # Using tryCatch to handle potential errors for individual files
  tryCatch({
    dbWriteTable(conn, "media_space_2023", df, append = TRUE, row.names = FALSE)
  }, error = function(e) {
    message("Error with file: ", file_path)
    message("Error message: ", e$message)
  })
}

















data_list <- lapply(file_list, read_excel)
#data <- do.call(rbind, data_list)




dbWriteTable(conn, "determ", data, append = TRUE, row.names = FALSE, overwrite = FALSE)



for (df in data_list) {
  dbWriteTable(conn, "your_table_name", df, append = TRUE)
}



query <- "SELECT * FROM media_space_2021"
data <- dbGetQuery(conn, query)

dbDisconnect(conn)

# Your SQL query to count the number of rows
query <- "SELECT COUNT(*) FROM media_space_2021"

# Execute the query and retrieve the result
num_rows <- dbGetQuery(conn, query)

# num_rows will be a data frame; the count will be in the first cell
row_count <- num_rows[1, 1]




