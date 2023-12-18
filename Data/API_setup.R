

library(httr)
library(lubridate)


base_url <- "https://api.mediatoolkit.com/organizations/126686/groups/182718/keywords/6521533/mentions"



# Create a POSIXct date-time object
datetime <- as.POSIXct("2022-01-02 12:00:00", tz = "UTC")

# Format it in the desired format, including the time zone
timestampB <- format(datetime, format = "%d.%m.%Y. %H:%M %Z")
timestampE <- format(datetime, format = "%d.%m.%Y. %H:%M %Z")


# get access token from environment variable
access_token <- Sys.getenv("DETERM_API_KEY")

from_time <- timestampB
to_time <-  timestampE
count <- 10
sort <- "time"
type <- "all"
offset <- 0
ids_only <- FALSE


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


response <- GET(url = base_url, query = query_params)
cat(content(response, 'text'))

response <- GET(url = base_url, query = query_params)

# parse this response into a list

response_list <- content(response, "parsed")

# extract responses from web

response_list$data





