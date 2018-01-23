library(adagio)
library(tidyverse)

# Functions ---------------------------------------------------------------

### Function to get the draft positions of a team that drafts <position>th, 
### in a league with x teams, and gets x draft slots overall
get_picks <- function(position, teams, slots) {
  return(seq(from = position, by = teams, length.out = slots))
}


# Constants ---------------------------------------------------------------

league_min <- 535000
salary_cap <- 75000000
team_count <- 12
draft_order <- order(c(11, 6, 1, 9, 2, 7, 5, 10, 4, 8, 3, 12)) # T11 picks first, T6 picks second, etc.

# Read in Data ------------------------------------------------------------

### Import the USA Today Salary data set 
salary_data <- read_csv("data/usatoday_salary.csv") %>%
  filter(year == 2017)  %>%
  
  ### Remove periods from names
  mutate(name = gsub("\\.","", name)) %>%
  
  ### Remove accents from names
  mutate(name = iconv(as.character(name), from="UTF-8", to="ASCII//TRANSLIT"))
  
### Import the 2018 Steamer Projections
steamer_proj <- read_csv("data/steamer_2018_batters.csv") %>%
  select(Name, Team, WAR, playerid) %>%
  bind_rows(read_csv("data/steamer_2018_pitchers.csv") %>% select(Name, Team, WAR, playerid)) %>%
  mutate(Name = gsub("\\.","", Name)) %>%
  mutate(Name = iconv(as.character(Name), from="UTF-8", to="ASCII//TRANSLIT"))


# First pass of keeper optimization ---------------------------------------

# Init the data frame
all_keepers <- data_frame()

for (i in 1:team_count) {
  
  ### Read in the rosters of a fantasy team
  keeper_roster <- read_csv("data/DFL_rosters.csv") %>%
    filter(fantasy_team == i) %>%
    
    ### Clean up the names
    mutate(name = gsub("\\.","", name)) %>%
    mutate(name = iconv(as.character(name), from="UTF-8", to="ASCII//TRANSLIT"))
  
  ### Merge the keeper data with their steamer projection and 2017 salary
  merged_keeper <- left_join(keeper_roster, steamer_proj, by = c("name" = "Name")) %>%
    left_join(salary_data, by = c("name" = "name"))  %>%
    
    ### If there's no salary data, use the league minimum, working in units of $100,000 
    mutate(salary = case_when(
      is.na(salary)  ~ league_min / 10^5,
      !is.na(salary) ~ salary / 10^5)) %>%
    
    ### If there's no WAR projection, assume they're replacement level
    mutate(WAR = case_when(
      is.na(WAR)  ~ 0,
      !is.na(WAR) ~ WAR))
  
  ### Use the adagio implimentation of the knapsack problem solution to find optimal keepers
  keepers <- knapsack(merged_keeper$salary, merged_keeper$WAR, cap = 750) # Satisfy salary cap
  
  ### Make a data frame of just the players to keep
  best_roster <- merged_keeper %>%
    filter(row_number() %in% keepers$indices)
  
  ### Add it to the overall dataframe
  all_keepers <- bind_rows(all_keepers, best_roster)
  rm(keeper_roster, merged_keeper, best_roster)
}

### Make a summary dataframe for each team
team_summary <- all_keepers %>%
  group_by(fantasy_team) %>%
  summarize(keepers = n(), WAR = sum(WAR), salary = sum(salary)) %>%
  mutate(picks = 30 - keepers)

### Write the data to CSVs
write_csv(all_keepers, "data/DFL_rosters_opt.csv")
write_csv(team_summary, "data/DFL_rosters_opt_summary.csv")

# Attempt to value draft slots --------------------------------------------

### Find the draftable players based on keeper optimization
draftable <- steamer_proj %>%
  left_join(salary_data, by = c("Name" = "name")) %>%
  mutate(salary = case_when(
    is.na(salary)  ~ league_min,
    !is.na(salary) ~ salary)) %>%
  anti_join(all_keepers, by = c("Name" = "name")) %>%
  filter(WAR > 0.2) %>%
  arrange(desc(WAR))

### Try to predict the WAR value of each draft slot. Wiggle is the width of the rolling average
draft_wiggle <- 5
draft_values <- draftable %>%
  mutate(pick = row_number(), 
         est_WAR = c(zoo::rollmean(x = WAR, draft_wiggle), rep(0, draft_wiggle-1))) %>%
  select(pick, est_WAR)

###

all_keepers_r2 <- data_frame()

for (i in 1:team_count) {
  team_number <- i

  ### Load the list of keepers
team_keepers <- all_keepers %>%
  filter(fantasy_team == team_number)

### 
team_pick_count <- team_summary %>%
  filter(fantasy_team == team_number) %>%
  pull(picks)

last_pick <- max(get_picks(draft_order[i], team_count, team_pick_count))

last_pick_value <- draft_values %>%
  filter(row_number() == last_pick) %>%
  pull(est_WAR)

all_keepers_r2 <- team_keepers %>%
  filter(WAR > last_pick_value) %>%
  bind_rows(all_keepers_r2, .)

}

team_summary_r2 <- all_keepers_r2 %>%
  group_by(fantasy_team) %>%
  summarize(keepers_r2 = n(), WAR_r2 = sum(WAR), salary_r2 = sum(salary)) %>%
  mutate(picks_r2 = 30 - keepers_r2) %>%
  bind_cols(team_summary, .) %>%
  mutate(diff_r2 = keepers - keepers_r2)
