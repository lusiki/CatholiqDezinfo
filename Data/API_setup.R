

library(httr)
library(lubridate)
library(jsonlite)
library(dplyr)
library(tidyverse)


base_url <- "https://api.mediatoolkit.com/organizations/126686/groups/182718/keywords/6521533/mentions"



# Create a POSIXct date-time object
datetimeA <- as.POSIXct("2022-01-05 12:00:00", tz = "UTC")
datetimeB <- as.POSIXct("2022-01-06 12:00:00", tz = "UTC")

# Format it in the desired format, including the time zone
timestampA <- format(datetimeA, format = "%d.%m.%Y. %H:%M %Z")
timestampB <- format(datetimeB , format = "%d.%m.%Y. %H:%M %Z")

# get access token from environment variable
access_token <- Sys.getenv("DETERM_API_KEY")

from_time <- timestampA
to_time <-  timestampB
count <- 10000
sort <- "time"
type <- "all"
offset <- 0
ids_only <- FALSE

# create a list of query parameters
query_params <- list(
  access_token = access_token,
  from_time = from_time,
  to_time = to_time,
  count = count,
  sort = sort,
  type = type,
  offset = offset,
 ids_only = ids_only,
  include_full_text = TRUE
)

# make a GET request to the API
response <- GET(url = base_url, query = query_params)
cat(content(response, 'text'), "\n")
response_list <- fromJSON(content(response, "text", encoding = "UTF-8"))


# Convert the response to a tidy data frame
tidy_df <-  as_tibble(response_list$data) %>%
  unnest(response)

# Convert Unix timestamp to human-readable format if necessary
tidy_df$insert_time <- as.POSIXct(tidy_df$insert_time, origin="1970-01-01", tz="UTC")

# View the tidy data frame
print(tidy_df)






