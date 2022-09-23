# Let's test the endpoint to see which data is available

# SETUP ------------------------------------------------------------------------

# Import libraries
library(dplyr)
library(stringr)
library(purrr)
library(httr2)

# TESTING API ENDPOINTS --------------------------------------------------------

## PLAYER API ------------------------------------------------------------------

# perform API call
resp <- request("https://api.chess.com/pub/player/balcoo") %>% 
  req_perform()

# extract information on expiration from response header
# this tells us when the data will refresh (I think...)
expires <- resp %>% 
  resp_header(header = 'expires') %>% 
  as.POSIXct(format = '%a, %d %b %Y %X', tz = 'GMT')

# extract the body of the response
res <- resp %>% 
  resp_body_json() %>% 
  # when converting to tibble, fix the "@id" column name
  bind_cols(.name_repair = make.names)

# some preprocessing to keep only relevant information
res <- res %>%
  # if the user doesn't have it, add a "twitch_url" column (empty)
  { if("twitch_url" %in% colnames(.)) . else 
    mutate(., twitch_url = NA_character_, .after = is_streamer) } %>% 
  mutate(
    # convert EPOCH timestamps to redable times
    across(c(joined, last_online), anytime::anytime, tz = 'GMT'),
    # remove unneeded URLs
    country = str_remove(country, '^https://api.chess.com/pub/country/'),
    twitch_url = str_remove(twitch_url, '^https://twitch.tv/'),
    expires = expires
  ) %>% 
  select(-c(X.id, url))

# TITLED PLAYERS ---------------------------------------------------------------

# perform API call
# valid titles: GM, WGM, IM, WIM, FM, WFM, NM, WNM, CM, WCM
resp <- request("https://api.chess.com/pub/titled/GM") %>% 
  req_perform()

# extract the body of the response
res <- resp %>% 
  resp_body_json() %>% 
  unlist() %>% 
  unname()

# PLAYER STATS -----------------------------------------------------------------

# There's a lot of stuff in here; need to find a smart way to store this info
# That's work for another day

# perform API call
resp <- request("https://api.chess.com/pub/player/balcoo/stats") %>% 
  req_perform()

# extract the body of the response
res <- resp %>% 
  resp_body_json()

View(res)
