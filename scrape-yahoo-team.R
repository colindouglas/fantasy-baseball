### Use the httr bugfix from https://github.com/r-lib/httr/issues/493
#devtools::install_github("ctrombley/httr")
library(httr)
library(xml2)
library(tidyverse)
library(jsonlite)


# Authorize with Yahoo Fantasy API ----------------------------------------

keys <- read_csv("../oauth_keys.csv") %>%
  filter(website == "yahoo_old")

userAgent <- "contact colindouglas@gmail.com"

# Stolen from https://stackoverflow.com/questions/49709988/r-yahoo-fantasy-api-permission?rq=1
# Updated 10-Feb-2020
options("httr_oob_default" = T)
yahoo <- httr::oauth_endpoint(authorize ="https://api.login.yahoo.com/oauth2/request_auth", access = "https://api.login.yahoo.com/oauth2/get_token", base_url = "https://fantasysports.yahooapis.com")
app <- httr::oauth_app("yahoo", key = keys$key, secret = keys$secret, redirect_uri = "oob")

# Scope for Fantasy read/write = "fspt-w"
# Scope for just read = "fspt-r"
httr::BROWSE(httr::oauth2.0_authorize_url(yahoo, app, scope="fspt-w", redirect_uri = app$redirect_uri))

# Cut and paste this from the window that pops up
passcode <- rstudioapi::showPrompt(title = "Passcode?", message = "Enter your passcode from the window that just popped up")
token <- httr::oauth2.0_access_token(yahoo, app, code = passcode)

# Const
endpoint <- oauth_endpoint(
  authorize = "https://api.login.yahoo.com/oauth2/request_auth",
  access =    "https://api.login.yahoo.com/oauth2/get_token",
  base_url = "https://api.login.yahoo.com/oauth2"
)


# Figure out the league ID that we want -----------------------------------

# API request to get game IDs
game_ids <- content(GET(
  "https://fantasysports.yahooapis.com/fantasy/v2/users;use_login=1/games;game_keys/?format=json",
  add_headers(Authorization = paste0("Bearer ", token$access_token)))) 

# Game ID dot "l" dot league ID
#leagueID <- "378.l.21228" # DFL 2018
leagueID <- "388.l.18437" # DFL 2019
#leagueID <- "398.l.35309" # DFL 2020


# Get the rosters for each time all crafty, using purrr -------------------

# Construct the Yahoo API call to get the team roster
team_rosters <- map(1:12, function(team_number) {
  Sys.sleep(2) # Wait two seconds so the API doesn't get mad
  
  # Make the URL for the API request
  url <- paste0("https://fantasysports.yahooapis.com/fantasy/v2/team/", leagueID, ".t.", team_number, "/roster?format=json")
  
  # Return the content from the API request
  content(GET(url, 
              user_agent(userAgent),
              add_headers(Authorization = paste0("Bearer ", token$access_token)))
  )}
)


# Get the rosters for each team -------------------------------------------

fantasy_teams <- tibble()
for (i in 1:12) {
  ### Wait two seconds so the API doesn't get mad
  Sys.sleep(2)
  
  team_number <- i

  # Construct the Yahoo API call to get the team roster
  url <- paste0("https://fantasysports.yahooapis.com/fantasy/v2/team/", leagueID, ".t.", team_number, "/roster?format=json")
  roster_list <- content(GET(url, user_agent(userAgent),
                     add_headers(Authorization = paste0("Bearer ", token$access_token))))
  
  # Skip if team is empty
  if(length(roster_list$fantasy_content) == 0) {
    print(paste(team_number, " failed"))
    next
  }
  
  roster <- data_frame(
    player_key = unlist(map_if(roster_list$fantasy_content$team[[2]]$roster[[4]]$players, is.list, ~ .$player[[1]][[1]]$player_key)),
    player_id = unlist(map_if(roster_list$fantasy_content$team[[2]]$roster[[4]]$players, is.list, ~ .$player[[1]][[2]]$player_id)),
    name = unlist(map_if(roster_list$fantasy_content$team[[2]]$roster[[4]]$players, is.list, ~ .$player[[1]][[3]]$name$full)),
    fantasy_team = team_number,
    team_name = team_rosters[[1]]$fantasy_content$team[[1]][[3]][[1]]) %>%

    # Remove the last row of the df (its junk)
    head(-1) %>% 
    
    # Transliterate the names so accents don't mess up your merges
    mutate(name = iconv(as.character(name), from="UTF-8", to="ASCII//TRANSLIT"))
  
  message("Team: ", team_number, " // Length: ", nrow(roster))
  
  if(nrow(roster) == 0) next # Skip if the team doesn't exist
  
  fantasy_teams <- bind_rows(fantasy_teams, roster)

}

# Save the rosters as CSVs
path <- paste0("data/DFL_rosters_2019.csv")
write_csv(fantasy_teams, path)
