# Use retrosheet fork that works with zero substitution games
# devtools::install_github("colindouglas/retrosheet")

library(retrosheet)
library(tidyverse)
library(DBI)
library(odbc)
library(tictoc)


# This sets a connection called 'rsdb' to a Postgres server where the data is stored
source("connect_SQL.R")

# This script downloads gamelog data from Retrosheet and reorganizes into tables that play nice with SQL
# Most of the work here is just unnesting the game data and organizing it key-wise

DownloadTeamData <- function(year, team) {
  
  #year <- 2018;  team_number <- 2 #debug
  message("Year: ", year, " // Team: ", team)
  games <- getRetrosheet(type = "play", year = year, team = team, stringsAsFactors = FALSE)
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
      mutate(game_id = head(game$id, 1)) %>%
      filter(!is.na(category)) %>%
      mutate(year = year) 
  })
  toc()
  
  tic("Starters")
  # The starting players for each game
  starters <-  map_dfr(games, function(game) {
    as_tibble(game$start) %>%
      filter(!is.na(retroID)) %>%
      mutate(game_id = head(game$id, 1)) %>%
      select(game_id, everything()) %>%
      type_convert(col_types = cols())
  })
  toc()
  
  tic("Play-by-Play")
  # Play-by-play of each game
  # Parsed by event_log_parser.R to a more R-friendly format
  plays <-  map_dfr(games, function(game) {
    as_tibble(game$play) %>%
      mutate(game_id = head(game$id, 1)) %>%
      select(game_id, everything()) %>%
      type_convert(col_types = cols())
  })
  toc()
  
  tic("Comments")
  # Comments for the game
  # Usually includes  injury information and video challenges
  comments <-  map_dfr(games, function(game) {
    tibble(game_id = head(game$id, 1), 
           comments = game$com %>% 
             paste(collapse = "") %>% 
             str_split(pattern = "\\$") %>% 
             unlist() %>% 
             tail(-1)) %>%
      type_convert(col_types = cols())
  })
  toc()
  
  tic("Subs")
  # Substitutions that occur in the game
  subs <-  map_dfr(games, function(game) {
    as_tibble(game$sub) %>%
      filter(!is.na(retroID)) %>%
      type_convert(col_types = cols())
  })
  toc()
  tic("Extra data")
  # This is just extra data, probably not useful?
  # "er" fields count the number of earned runs for each player within a game
  data <-  map_dfr(games, function(game) {
    as_tibble(game$data) %>%
      filter(!is.na(projCode)) %>%
      mutate(game_id = head(game$id, 1)) %>%
      type_convert(col_types = cols())
  })
  toc()
  tic("Writing to DB")
  table_names <- c("comments", "data", "info_long", "plays", "starters", "subs")
  walk(table_names, ~ dbWriteTable(con = rsdb, name = ., value = eval(as.name(.)), append = TRUE)) # Write to DB
  toc()
}

# This is the code the does all the legwork
# For a given year, get all the teams in that year and then download all the team data
# Walk this over a range of years
walk(2015:1994, 
     function(year) {
       walk(getTeamIDs(year = year), 
            ~ DownloadTeamData(year, team = .))
     })

# Converts the info_long table to a wide table
# Compiling it long allows for greater flexibility
# But coverting it wide allows for easier joining
dbGetQuery(rsdb, statement = "select * from info_long") %>% 
  pivot_wider(id_cols = c("game_id", "year"), 
              names_from = category, 
              values_from = info) %>%
  dbWriteTable(con = rsdb, name = "info", value = ., overwrite = TRUE)

# Function to drop all the tables if you want to restart
DropAllTables <- function() {
  table_names <- c("comments", "data", "info_long", "info", "plays", "starters", "subs")
  walk(table_names, ~ dbRemoveTable(con = rsdb, name = .)) 
}