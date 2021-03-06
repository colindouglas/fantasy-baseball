# Use retrosheet fork that works with no-substitution games and caching
# devtools::install_github("colindouglas/retrosheet")
# devtools::install("~/projects/retrosheet/")
library(retrosheet)
library(tidyverse)
library(DBI)
library(tictoc)

# Sets an SQLite connection called 'rsdb' to local file "data/retrosheet.db"
source("connect_SQL.R")

# Function to drop all the tables if you want to restart
DropAllTables <- function() {
  table_names <- c("comments", "data", "info_long", "info", "plays", "starters", "subs")
  walk(table_names, ~ dbRemoveTable(con = rsdb, name = .)) 
}

# This script downloads gamelog data from Retrosheet and reorganizes into tables that play nice with SQL
# Most of the work here is just unnesting the game data and organizing it key-wise
DownloadTeamData <- possibly(
  function(year, team) {

    message("Year: ", year, " // Team: ", team)
    games <- getRetrosheet(type = "play", year = year, team = team, stringsAsFactors = FALSE, cache = "data/retrosheet")

    # 1999-and-earlier seasons have a copyright notice as the first element of the list
    # The line below removes games where the game_id is NULL to avoid this problem
    games <- games[unlist(map(games, ~ !is.null(.$id)))] 
    
    #game <- games[[77]] # debug
    
    # Table with data about each individual name
    # More details here: https://www.retrosheet.org/eventfile.htm
    # "game_id"    = game identifier in the form of PARKYYYMMDDI (PRIMARY KEY)     
    # "visteam"   = visiting team
    # "hometeam"  = home team
    # "site"      = Park Identifier
    # "date"      = Date in the form of YYYY/MM/DD
    # "number"    = Index for game played that day (e.g., 0 is the first game of the day)
    # "starttime" = Game start time, in the form of H:MM A
    # "daynight"  = Day game or night game?
    # "usedh"     = Did the game have a DH?
    # "umphome"   = HP umpire
    # "ump1b"     = 1B umpire
    # "ump2b"     = 2B umpire
    # "ump3b"     = 3B umpire
    # "howscored" = Describes how the game account was obtained and processed
    # "pitches"   = Describes how the pitch data is recorded
    # "oscorer"   = The official scorer
    # "temp"      = Gametime temperature (in F)
    # "winddir"   = Direction of wind (possible values: fromcf, fromlf, fromrf, ltor, rtol, tocf, tolf, torf, unknown)
    # "windspeed" = Wind speed in MPH
    # "fieldcond" = Field conditions (possible values: dry, soaked, wet, unknown)
    # "precip"    = Precipitation (possible values: drizzle, none, rain, showers, snow, unknown)
    # "sky"       = Sky conditions (possible values: cloudy, dome, night, overcast, sunny, uknown) (is this a typo?)
    # "timeofgame"= Length of game in minutes
    # "attendance"= Attendance
    # "wp"        = Winning pitcher
    # "lp"        = Losing pitcher
    # "save"      = Pitcher awarded the save
    
    tic("Game info")
    info_long <- map_dfr(games, function(game) {
      as_tibble(game$info) %>%
        mutate(gameID = head(game$id, 1)) %>%
        filter(!is.na(category)) %>%
        mutate(year = year) 
    })
    toc()
    
    tic("Starters")
    # The starting players for each game
    starters <-  map_dfr(games, function(game) {
      as_tibble(game$start) %>%
        filter(!is.na(retroID)) %>%
        mutate(gameID = head(game$id, 1)) %>%
        select(gameID, everything()) %>%
        type_convert(col_types = cols()) 
    })
    toc()
    
    tic("Play-by-Play")
    # Play-by-play of each game
    # Parsed by event_log_parser.R to a more R-friendly format
    plays <-  map_dfr(games, function(game) {
      as_tibble(game$play) %>%
        mutate(gameID = head(game$id, 1)) %>%
        select(gameID, everything()) %>%
        type_convert(col_types = cols())
    })
    toc()
    
    tic("Comments")
    # Comments for the game
    # Usually includes  injury information and video challenges
    comments <-  map_dfr(games, function(game) {
      tibble(gameID = head(game$id, 1), 
             comments = game$com %>% 
               paste(collapse = "") %>% 
               str_split(pattern = "\\$") %>% 
               unlist() %>% 
               tail(-1)) %>%
        type_convert(col_types = cols(col_character(), col_character()))
    })
    toc()
    
    tic("Subs")
    # Substitutions that occur in the game
    subs <- map_dfr(games, function(game) {
      if (NCOL(game$sub) >= 5) { 
        as_tibble(game$sub) %>%
          mutate(gameID = head(game$id, 1),
                 # The line below is here because PIT200206180 has a team listed as 1X and it messes up the typing. 
                 # Not sure why? Maybe pinch runner?
                 team = gsub("[a-zA-Z]", "", team)) %>%
          filter(!is.na(retroID)) %>%
          type_convert(col_types = cols())
      } else {
        return(NULL)
      }
    })
    
    
    toc()
    
    tic("Extra data")
    # This is just extra data, probably not useful?
    # "er" fields count the number of earned runs for each player within a game
    data <-  map_dfr(games, function(game) {
      as_tibble(game$data) %>%
        filter(!is.na(projCode)) %>%
        mutate(gameID = head(game$id, 1)) %>%
        type_convert(col_types = cols())
    })
    toc()
    
    tic("Writing to database")
    table_names <- c("comments", "data", "info_long", "plays", "starters")
    walk(table_names, ~ dbWriteTable(con = rsdb, name = ., value = eval(as.name((.))), append = TRUE)) # Write to DB
    if (length(subs) > 0) {
      dbWriteTable(con = rsdb, name = "subs", value = subs, append = TRUE)
    }
    toc()
    
    return(tibble(year, team))
  }, 
  otherwise = NULL)


# This is the code the does all the legwork
# For a given year, get all the teams in that year and then download all the team data
# Walk this over a range of years
years <- 2018:1919 # Low end: 1919

successful_seasons <- map_dfr(years,
     function(year) {
       map_dfr(
         getTeamIDs(year = year), 
         ~ DownloadTeamData(year, team = .))
     })

# Converts the info_long table to a wide table
# Reshaping it long allows for greater flexibility
# But coverting it wide allows for easier joining
tic("Info long to wide")
info_long <- dbReadTable(rsdb, name = "info_long") 

# Certain categories have more than one value (for some reason)
# This collapses them into comma separated values in a single cell
collapse_list <- function(x) paste(unlist(x), collapse = ", ")

info <- info_long %>% 
  distinct() %>% # Just in case I accidentally processed a game twice
  pivot_wider(
    id_cols = c("gameID", "year"),
    names_from = category,
    values_from = info,
    values_fn = list(info = collapse_list)
  )

dbWriteTable(con = rsdb, name = "info", value = info, overwrite = TRUE)
toc()

# Logging for troubleshooting
all_seasons <- map_dfr(years, ~ tibble(year = ., team = getTeamIDs(year = .) ))
failed_seasons <- write_csv(anti_join(all_seasons, successful_seasons), path = "logs/RS_to_SQL - failed_seasons.csv")
