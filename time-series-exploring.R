library(retrosheet)
library(tidyverse)
library(DBI)
library(fable)
library(tsibble)

# Sets an SQLite connection called 'rsdb' to local file "data/retrosheet.db"
source("connect_SQL.R")

retrosheets <- tbl(rsdb, "plays") %>%
  left_join(tbl(rsdb, "info"), by = "gameID")


twenty18 <- retrosheets %>%
  filter(year %in% 1950:2018) %>%
  group_by(date, retroID) %>%
  summarize(BB = sum(play == "W"),
            K = sum(play == "K")) %>%
  ungroup() 

interesting_players <- twenty18 %>%
  group_by(retroID) %>%
  summarize(games = n()) %>%
  arrange(desc(games)) %>%
  pull(retroID) %>%
  head(42)

ts <- twenty18 %>%
  filter(retroID %in% interesting_players) %>%
  collect() %>%
  filter(!is.na(date), !is.na(retroID)) %>%
  mutate(date = lubridate::ymd(date)) %>%
  as_tsibble(index = date, key = retroID)

name_lookup <- tbl(rsdb, "starters") %>%
  select(retroID, name) %>%
  collect() %>%
  distinct(retroID, .keep_all = TRUE)

ts %>%
  group_by(retroID) %>%
  index_by(year = lubridate::year(date)) %>% 
  summarize(K = sum(K)) %>%
  as_tibble() %>%
  left_join(name_lookup, by = "retroID", copy = TRUE) %>%
  ggplot(aes(x = year, y = K)) +
  geom_line(aes(color = retroID)) +
  facet_wrap(~ name) +
  scale_color_discrete(guide = FALSE)
