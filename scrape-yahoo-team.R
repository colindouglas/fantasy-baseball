<<<<<<< HEAD
### Use the httr bugfix from https://github.com/r-lib/httr/issues/493
#devtools::install_github("ctrombley/httr")
library(httr)
library(xml2)
library(tidyverse)
library(jsonlite)

keys <- read_csv("../oauth_keys.csv") %>%
  filter(website == "yahoo")

userAgent <- "contact colindouglas@gmail.com"

endpoint <- oauth_endpoint(
  authorize = "https://api.login.yahoo.com/oauth2/request_auth",
  access =    "https://api.login.yahoo.com/oauth2/get_token",
  base_url = "https://api.login.yahoo.com/oauth2"
)

app <- oauth_app("Fantasy Baseball Scraper", keys$key, keys$secret, redirect_uri = "oob")

token <- oauth2.0_token(endpoint = endpoint, app = app, use_oob = TRUE, config_init = user_agent(userAgent))

### MLB 2017 GameID = 370
### DFL 2018 league ID = 21228 / game 378
### DFL 2019 league ID = 18437 / game 388 therefore 388.l.18437

leagueID <- "378.l.21228"

# url <- paste0("https://fantasysports.yahooapis.com/fantasy/v2/team/", leagueID, ".t.", team, "?format=json")
# team_info <- GET(url, user_agent(userAgent), config(token = token)) %>% content() 

fantasy_teams <- data_frame()
for (i in 1:15) {
  ### Wait two seconds so the API doesn't get mad
  Sys.sleep(2)
  
  team_number <- i

  # Construct the Yahoo API call to get the team roster
  url <- paste0("https://fantasysports.yahooapis.com/fantasy/v2/team/", leagueID, ".t.", team_number, "/roster?format=json")
  roster_list <- GET(url, user_agent(userAgent), config(token = token)) %>% content()
  
  # Skip if team is empty
  if(length(roster_list$fantasy_content) == 0) {
    print(paste(team_number, " failed"))
    next
  }
  
  roster <- data_frame(
    player_key = unlist(map_if(roster_list$fantasy_content$team[[2]]$roster[[4]]$players, is.list, ~ .$player[[1]][[1]]$player_key)),
    player_id = unlist(map_if(roster_list$fantasy_content$team[[2]]$roster[[4]]$players, is.list, ~ .$player[[1]][[2]]$player_id)),
    name = unlist(map_if(roster_list$fantasy_content$team[[2]]$roster[[4]]$players, is.list, ~ .$player[[1]][[3]]$name$full)),
    fantasy_team = team_number) %>%

    # Remove the last row of the df (its junk)
    head(-1) %>% 
    
    # Transliterate the names so accents don't mess up your merges
    mutate(name = iconv(as.character(name), from="UTF-8", to="ASCII//TRANSLIT"))
  
  print(paste0("Team:", team_number, "length:", nrow(roster)))
  
  if(nrow(roster) == 0) next # Skip if the team doesn't exist
  
  fantasy_teams <- bind_rows(fantasy_teams, roster)
  


}

# Save the rosters as CSVs
path <- paste0("data/DFL_rosters.csv")
write_csv(fantasy_teams, path)

# This gets the current gameID
game_id <- GET("https://fantasysports.yahooapis.com/fantasy/v2/game/mlb?format=json", user_agent(userAgent), config(token = token)) %>% content()
str(game_id)

=======
### Use the httr bugfix from https://github.com/r-lib/httr/issues/493
devtools::install_github("ctrombley/httr")
library(httr)
library(xml2)
library(tidyverse)
library(jsonlite)

keys <- read_csv("../oauth_keys.csv") %>%
  filter(website == "yahoo")

userAgent <- "contact colindouglas@gmail.com"

endpoint <- oauth_endpoint(
  authorize = "https://api.login.yahoo.com/oauth2/request_auth",
  access =    "https://api.login.yahoo.com/oauth2/get_token",
  base_url = "https://api.login.yahoo.com/oauth2"
)

app <- oauth_app("Fantasy Baseball Scraper", keys$key, keys$secret, redirect_uri = "oob")

token <- oauth2.0_token(endpoint = endpoint, app = app, use_oob = TRUE, config_init = user_agent(userAgent))

### MLB 2017 GameID = 370
leagueID <- "370.l.55086"

# url <- paste0("https://fantasysports.yahooapis.com/fantasy/v2/team/", leagueID, ".t.", team, "?format=json")
# team_info <- GET(url, user_agent(userAgent), config(token = token)) %>% content() 

fantasy_teams <- data_frame()
for (i in 1:12) {
  ### Wait two seconds so the API doesn't get mad
  Sys.sleep(2)
  
  team_number <- i

  # Construct the Yahoo API call to get the team roster
  url <- paste0("https://fantasysports.yahooapis.com/fantasy/v2/team/", leagueID, ".t.", team_number, "/roster?format=json")
  roster_list <- GET(url, user_agent(userAgent), config(token = token)) %>% content()
  
  # Extract the player info out of the JSON response
  roster <- data_frame(
    player_key = unlist(map_if(roster_list$fantasy_content$team[[2]]$roster[[4]]$players, is.list, ~ .$player[[1]][[1]]$player_key)),
    player_id = unlist(map_if(roster_list$fantasy_content$team[[2]]$roster[[4]]$players, is.list, ~ .$player[[1]][[2]]$player_id)),
    name = unlist(map_if(roster_list$fantasy_content$team[[2]]$roster[[4]]$players, is.list, ~ .$player[[1]][[3]]$name$full)),
    fantasy_team = team_number) %>% 

    # Remove the last row of the df (its junk)
    head(-1) %>% 
    
    # Transliterate the names so accents don't mess up your merges
    mutate(name = iconv(as.character(name), from="UTF-8", to="ASCII//TRANSLIT"))
  
  fantasy_teams <- bind_rows(fantasy_teams, roster)

}

# Save the rosters as CSVs
path <- paste0("data/DFL_rosters.csv")
write_csv(fantasy_teams, path)


>>>>>>> f46006cf36ec1cda500107390ce643f62d8df146
