### Use the httr bugfix from https://github.com/r-lib/httr/issues/493
#devtools::install_github("ctrombley/httr")
library(httr)
library(xml2)
library(tidyverse)
library(jsonlite)


# Constants ---------------------------------------------------------------
year <- 2019 # Download data for what year?
league_minimum <- 555000 # League minimum salary for {year}

# Function to construct a Yahoo Fantasy API call
yahoo_api <- function(...) {
  url <- paste0(c(...), collapse = "")
  content(GET(url, 
              user_agent(userAgent),
              add_headers(Authorization = paste0("Bearer ", token$access_token))
  ))}

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
leagueIDs <- c(
  "2018" = "378.l.21228", # DFL 2018
  "2019" = "388.l.18437", # DFL 2019
  "2020" = "398.l.35309"  # DFL 2020
)

leagueID <- leagueIDs[[as.character(year)]]

# Get the rosters for each team in the league -----------------------------

# Get the league info
league_info <- yahoo_api("https://fantasysports.yahooapis.com/fantasy/v2/league/", leagueID, "?format=json")$fantasy_content$league[[1]]

# Get the team roster info for each team in the league from Yahoo
team_rosters <- map(1:league_info$num_teams, function(team_number) {
  
  Sys.sleep(2) # Wait two seconds so the API doesn't get mad
  yahoo_api("https://fantasysports.yahooapis.com/fantasy/v2/team/", leagueID, ".t.", team_number, "/roster?format=json")
  }
)

# Extract the rosters for each team ------------------------------------------

fantasy_teams <- map_dfr(1:length(team_rosters), function(team_number) {
  roster_list <- team_rosters[[team_number]]
  
  # Skip if team is empty
  if(length(roster_list$fantasy_content) == 0) {
    warning(team_number, " failed")
    return(NULL)
  }
  
  # This is the ugliest thing in the world
  # But it works so I'm sticking with it for now
  roster <- tibble(
    player_key = unlist(map_if(roster_list$fantasy_content$team[[2]]$roster[[4]]$players, is.list, ~ .$player[[1]][[1]]$player_key)),
    player_id = unlist(map_if(roster_list$fantasy_content$team[[2]]$roster[[4]]$players, is.list, ~ .$player[[1]][[2]]$player_id)),
    name = unlist(map_if(roster_list$fantasy_content$team[[2]]$roster[[4]]$players, is.list, ~ .$player[[1]][[3]]$name$full)),
    fantasy_team = team_number,
    team_name = team_rosters[[team_number]]$fantasy_content$team[[1]][[3]][[1]]) %>%
    head(-1) %>% # Remove the last row of the df (its junk)
    # Transliterate the names so accents don't mess up your joins
    mutate(name = iconv(as.character(name), from="UTF-8", to="ASCII//TRANSLIT")) 
  
  message("Team: ", team_number, " // Length: ", nrow(roster))
  
  if(nrow(roster) == 0) {
    return(NULL) # Skip if the team doesn't exist
  } else {
    return(roster)
  }
})

# Write the data to some CSVs ---------------------------------------------
write_csv(fantasy_teams, "data/DFL-rosters.csv")

# Join the USA Today salaries and save that separately
salaries <- read_csv("data/usatoday_salary.csv") %>% 
  select(-aav)

teams_with_salary <- fantasy_teams %>%
  mutate(year = year) %>% 
  left_join(salaries, by = c("name", "year")) %>%
  mutate(salary = ifelse(is.na(salary), league_minimum, salary)) # League minimum is $550,000

write_csv(teams_with_salary, "data/DFL-rosters_USA-Today-salaries.csv")
            