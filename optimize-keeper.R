library(tidyverse)

# Functions ---------------------------------------------------------------

### Function to get the draft positions of a team that drafts <position>th, 
### in a league with x teams, and gets x draft slots overall
get_picks <- function(position, teams, slots) {
  return(seq(from = position, by = teams, length.out = slots))
}

# Constants ---------------------------------------------------------------

league_min <- 555000
salary_cap <- 68853720 # Not updated for 2020
team_count <- 12
roster_size <- 30
salary_year <- 2019

# Last place team is draft_order[1], first place team is draft_order[n]
draft_order <- c(2, 3, 4, 7, 6, 12, 8, 9, 11, 1, 10)

# Project player values ---------------------------------------------------
source("project-player-value.R")
player_values <- weightedProj %>%
  mutate(name = iconv(as.character(Name), from = "UTF-8", to = "ASCII//TRANSLIT")) %>%
  ungroup() %>%
  select(name, WAR = weightedWAR)

# Read in Data ------------------------------------------------------------
player_teams <- read_csv("data/DFL-rosters.csv")
player_salaries <- read_csv("data/usatoday_salary.csv")

players <- full_join(player_teams, player_salaries, by = 'name') %>%
  full_join(player_values, by = 'name') %>%
  filter(WAR > 0) %>%
  mutate(salary = ifelse(is.na(salary), league_min, salary))

team_names <- players %>%
  distinct(fantasy_team, team_name)

# Do first round of knapsacking -------------------------------------------

players <- players %>% 
  select(-team_name) %>%
  group_by(fantasy_team) %>%
  mutate(knapsack = as.logical(
    mknapsack::knapsack(volume = salary, 
                        profit = WAR, 
                        cap = salary_cap)) & !is.na(fantasy_team))

# Find the available players ----------------------------------------------

draft_values <- players %>% 
  ungroup() %>%
  filter(!knapsack) %>%
  arrange(-WAR, salary, name) %>%
  mutate(draft_pick = row_number()) %>%
  select(name, draft_pick, WAR) %>%
  mutate(fantasy_team = rep_len(draft_order, length.out = n()),
         salary = 0)


# Try to guess who would get drafted by who -------------------------------

teams_with_picks <- players %>%
  bind_rows(draft_values) %>%
  group_by(fantasy_team) %>%
  top_n(30, WAR) %>% 
  arrange(fantasy_team)


# Knapsack it again with draft pick guesses -------------------------------

players_round2 <- teams_with_picks %>% 
  group_by(fantasy_team) %>%
  mutate(knapsack2 = as.logical(
    mknapsack::knapsack(volume = salary, 
                        profit = WAR, 
                        cap = salary_cap)))



# Figure out keepers after second round -----------------------------------
keepers <- players_round2 %>%
  filter(is.na(draft_pick), knapsack2) %>%
  left_join(team_names, by = "fantasy_team")

keepers_summary <- keepers %>%
  group_by(team_name, fantasy_team) %>%
  summarize(keepers = n(), total_salary = sum(salary)) %>%
  filter(!is.na(fantasy_team))



# Write a pretty Excel worksheet via openxlsx ---------------------------------

tabs <- split(keepers, keepers$team_name)
tabs <- purrr::prepend(tabs, list(keepers_summary))
names(tabs)[1] <- "Summary"

xlsx_out <- openxlsx::createWorkbook()

walk2(tabs, names(tabs), function(data, name) {
  openxlsx::addWorksheet(xlsx_out, name)
  openxlsx::writeData(xlsx_out , name, data)
})

openxlsx::saveWorkbook(xlsx_out, file = 'data/keepers.xlsx', overwrite = TRUE)
