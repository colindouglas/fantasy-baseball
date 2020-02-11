library(tidyverse)

# Sets a connection called 'rsdb' to a Postgres server where the data is stored
source("connect_SQL.R")

plays_df <- tbl(rsdb, "plays") %>%
  left_join(tbl(rsdb, "info"), by = "gameID") %>%
  filter(year == 2018, hometeam == "TOR") %>%
  distinct() %>%
  collect()

event_to_df <- function(events) {
  events <- plays_df$play[1:100] #debug
  primary <- map(str_split(events, pattern = "/"), ~.[1])
  event_df <- tibble(event = events, fielding_pos = NA)
  event_df$K <- primary == "K" # Strikeouts
  event_df$IBB <- primary == "I" | primary == "IW" # Intentional walks
  event_df$BB <- primary == "W" | event_df$IBB # Walks (including IBB)
  event_df$Out <- grepl("^[0-9]+", primary) | event_df$K
  event_df$`1B` <- grepl("^S[^B]", primary) # Singles, match "S" but not "SB"
  event_df$`2B` <- grepl("^D[0-9]+", primary) # Doubles
  event_df$`3B` <- grepl("^T[0-9]+", primary) # Triples
  event_df$`HR` <- grepl("^HR[0-9]+", primary) # Dongs
  event_df$E <- grepl("^E[0-9]+", primary) # Errors
  
  # Handling ground rule doubles (they're doubles, but they aren't fielded by anyone)
  event_df$DGR <- grepl("^DGR+", primary)
  event_df$fielding_pos[event_df$DGR] <- NA
  event_df$`2B` <- event_df$`2B` | event_df$DGR
  
  # Who fields a non-HR hit?
  event_df$fielding_pos[event_df$`1B` | (event_df$`2B` & !event_df$DGR)  | event_df$`3B`] <- str_sub(str_extract(event_df$event[event_df$`1B` | (event_df$`2B` & !event_df$DGR) | event_df$`3B`], "^S[0-9]+|^D[0-9]+|^T[0-9]+"), start = 2)
  
  # Who fields an out?
  event_df$fielding_pos[event_df$Out & !event_df$K] <- str_extract(event_df$event[event_df$Out & !event_df$K], "^[0-9]+")
  
  # Who commited the error?
  event_df$fielding_pos[event_df$E] <- str_sub(str_extract(event_df$event[event_df$E], "^E[0-9]+"), start = 2)
  

}

  
  
  # Easy cases: 
  # W = Walk (BB)
  # K = Strikeout (K)
