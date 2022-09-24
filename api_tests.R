# Let's test the endpoint to see which data is available
# See https://www.chess.com/news/view/published-data-api for a list of endpoints

# SETUP ------------------------------------------------------------------------

# Import libraries
library(tibble)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(httr2)
library(anytime)

# TESTING API ENDPOINTS --------------------------------------------------------

## PLAYER API ------------------------------------------------------------------

# make a request to the player endpoint
resp <- request("https://api.chess.com/pub/player/balcoo") |> 
  # perform API call
  req_perform() |> 
  # extract the body of the response as a list
  resp_body_json() |> 
  # convert to tibble, fix the "@id" column name
  bind_cols(.name_repair = make.names)

# according to documentation, this are all available fields
#   "@id": "URL", // the location of this profile (always self-referencing)
#   "url": "URL", // the chess.com user's profile page (the username is 
#       displayed with the original letter case)
#   "username": "string", // the username of this player
#   "player_id": 41, // the non-changing Chess.com ID of this player
#   "title": "string", // (optional) abbreviation of chess title, if any
#   "status": "string", // account status: closed, closed:fair_play_violations, 
#       basic, premium, mod, staff
#   "name": "string", // (optional) the personal first and last name
#   "avatar": "URL", // (optional) URL of a 200x200 image
#   "location": "string", // (optional) the city or location
#   "country": "URL", // API location of this player's country's profile
#   "joined": 1178556600, // timestamp of registration on Chess.com
#   "last_online": 1500661803, // timestamp of the most recent login
#   "followers": 17 // the number of players tracking this player's activity
#   "is_streamer": "boolean", //if the member is a Chess.com streamer
#   "twitch_url": "Twitch.tv URL",
#   "fide": "integer" // FIDE rating

# Additional notes:
# - "fide" is NEVER sent, might be deprecated
# - "twitch_url" is also OPTIONAL, despite not being labeled as such

# for the optional fields, we create empty column if they are not present
opt_cols <- c(
  title      = NA_character_, 
  name       = NA_character_, 
  avatar     = NA_character_, 
  location   = NA_character_,
  twitch_url = NA_character_
)

# add empty columns for optional values not present in the response
resp <- resp |> 
  add_column(!!!opt_cols[!names(opt_cols) %in% names(resp)])

# some preprocessing to keep only relevant information
resp <- resp |>
  mutate(
    # convert EPOCH timestamps to readable times
    across(c(joined, last_online), anytime, tz = 'GMT'),
    # remove unneeded URLs
    country = str_remove(country, '^https://api.chess.com/pub/country/'),
    twitch_url = str_remove(twitch_url, '^https://twitch.tv/'),
    username = str_remove(url,'^https://www.chess.com/member/'),
    .keep = 'unused'
  ) |> 
  # remove column(s) containing no useful information
  select(
    # this is just the endpoint called, no point in keeping it
    -X.id
  ) |> 
  # relocate columns to an order that makes more sense to me
  relocate(
    player_id,
    username,
    name,
    joined,
    last_online,
    country,
    location,
    followers,
    status,
    is_streamer,
    twitch_url,
    verified,
    title,
    avatar
  )

## TITLED PLAYERS --------------------------------------------------------------

# make a request to the titled player endpoint
# valid titles: GM, WGM, IM, WIM, FM, WFM, NM, WNM, CM, WCM
resp <- request("https://api.chess.com/pub/titled/GM") |> 
  # perform API call
  req_perform() |> 
  # extract the body of the response as a list
  resp_body_json() |> 
  # the response is an array, so convert it to the equivalent R object
  unlist(use.names = FALSE)

### COMBINE TITLED PLAYERS AND PLAYER API --------------------------------------

# define a function to fetch data from a player api request
# this is the same as what was done above when calling the player endpoint
get_player_profile <- function(player){
  
  # make a request to the player endpoint
  resp <- request(paste0("https://api.chess.com/pub/player/", player)) |> 
    req_perform() |> 
    resp_body_json() |> 
    bind_cols(.name_repair = make.names)
  
  # for the optional fields, we create empty column if they are not present
  opt_cols <- c(
    title      = NA_character_, 
    name       = NA_character_, 
    avatar     = NA_character_, 
    location   = NA_character_,
    twitch_url = NA_character_
  )
  
  # add empty columns for optional values not present in the response
  resp <- resp |> 
    add_column(!!!opt_cols[!names(opt_cols) %in% names(resp)])
  
  # some preprocessing to keep only relevant information
  resp <- resp |>
    mutate(
      across(c(joined, last_online), anytime, tz = 'GMT'),
      country = str_remove(country, '^https://api.chess.com/pub/country/'),
      twitch_url = str_remove(twitch_url, '^https://twitch.tv/'),
      username = str_remove(url,'^https://www.chess.com/member/'),
      .keep = 'unused'
    ) |> 
    select(
      -X.id
    ) |> 
    relocate(
      player_id,
      username,
      name,
      joined,
      last_online,
      country,
      location,
      followers,
      status,
      is_streamer,
      twitch_url,
      verified,
      title,
      avatar
    )
  
  # return the response, after preprocessing it
  return(resp)
  
}

# # make a player profile API call to each of the titled players
# resp <- map_dfr(
#   .x = resp, # vector with the list of titled players
#   .f = get_player_profile
# )
# this takes a few minutes to run for some reason, so let's leave it commented... 

## PLAYER STATS ----------------------------------------------------------------

# make a request to the player stats endpoint
resp <- request("https://api.chess.com/pub/player/balcoo/stats") |> 
  # perform API call
  req_perform() |> 
  # extract the body of the response as a list
  resp_body_json()

# There's a lot of stuff in here; need to find a smart way to store this info
# Will come back to this later on

## PLAYER ONLINE STATUS --------------------------------------------------------

# make a request to the online status endpoint
resp <- request("https://api.chess.com/pub/player/balcoo/is-online") |> 
  # perform API call
  req_perform()

# This returns an error 404
# The official examples are also not working...
# This endpoint might be deprecated ?

## PLAYER GAMES ----------------------------------------------------------------

# As per documentation, there are 5 different endpoints available:
# - Current Daily Chess: daily Chess games that a player is currently playing;
# - To-Move Daily Chess: daily Chess games where it is the player's turn to act;
# - List of Monthly Archives: monthly archives available for this player;
# - Complete Monthly Archives: Live and Daily Chess games that a player finished;
# - Multi-Game PGN Download: standard multi-game PGN with all games for a month.

### CURRENT DAILY CHESS --------------------------------------------------------

# make a request to the player stats endpoint
resp <- request("https://api.chess.com/pub/player/tuhieu/games") |> 
  # perform API call
  req_perform() |> 
  # extract the body of the response as a list
  resp_body_json() |> 
  # for some reason, the response is a list within a list; extract it
  unlist(recursive = FALSE, use.names = FALSE) |> 
  # convert the list with all info for each game, to a tibble
  map_dfr(bind_cols)

# there are 2 optional fields:
# - tournament: only for tournament matches, link to the tournament
# - match : only for team matches, link to the team match
opt_cols <- c(
  tournament = NA_character_,
  match = NA_character_
)

# add empty columns for optional values not present in the response
resp <- resp |> 
  add_column(!!!opt_cols[!names(opt_cols) %in% names(resp)])

# preprocessing (only if there are rows)
if (nrow(resp) > 0){
  
  resp <- resp |>
    mutate(
      # remove URL from game ID (this also contains the game time format!)
      game_id = str_remove(url, pattern = "^https://www.chess.com/game/daily/"),
      # remove URL from tournament name
      tournament = str_remove(tournament, "^https://api.chess.com/pub/tournament/"),
      # remove URL from team match
      match = str_remove(match, "^https://api.chess.com/pub/match/"),
      # convert EPOCHs to readable timestamps
      across(c(move_by, last_activity, start_time), anytime, tz = 'GMT'),
      # remove URL from player IDs
      across(
        .cols = c(white, black), 
        .fns = str_remove,
        pattern = "^https://api.chess.com/pub/player/",
        .names = 'player_{col}'
      ),
      # add match type: one of "other", "tournament", "team_match"
      # - tournament matches have a "tournament" field with tournament info;
      # - team_match matches have a "match" field with team match info;
      # - other matches don't have any additional optional field
      # also add a "match_type_info" field with the additional information
      match_type = case_when(
        !is.na(tournament) ~ "tournament",
        !is.na(match) ~ "team_match",
        TRUE ~ "other"
      ),
      match_type_info = case_when(
        !is.na(tournament) ~ tournament,
        !is.na(match) ~ match,
        TRUE ~ NA_character_
      ),
      .keep = 'unused'
    ) |> 
    # remove column(s) containing no useful information (or no more needed)
    select(
      # these are contained in the new "match_type" and "match_type_info" fields
      -c(tournament, match)
    ) |> 
    # relocate columns to an order that makes more sense to me
    relocate(
      game_id,
      start_time,
      time_class,
      time_control,
      rated,
      rules,
      match_type,
      match_type_info,
      player_white,
      player_black,
      turn,
      last_activity,
      move_by,
      fen,
      pgn
    )
  
}

### TO-MOVE DAILY CHESS --------------------------------------------------------

# This endpoint contains information already retrievable from the one above.
# Can be ignored.

### LIST OF MONTHLY ARCHIVES ---------------------------------------------------

# make a request to the player stats endpoint
resp <- request("https://api.chess.com/pub/player/balcoo/games/archives") |> 
  # perform API call
  req_perform() |> 
  # extract the body of the response as a list
  resp_body_json() |> 
  # the response is an array, so convert it to the equivalent R object
  unlist(use.names = FALSE)

### COMPLETE MONTHLY ARCHIVES --------------------------------------------------

# make a request to the player stats endpoint
resp <- request("https://api.chess.com/pub/player/balcoo/games/2021/01") |> 
  # perform API call
  req_perform() |> 
  # extract the body of the response as a list
  resp_body_json() |> 
  # the response is a list within a list; extract it
  unlist(recursive = FALSE, use.names = FALSE)

# WIP
