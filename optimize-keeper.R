library(tidyverse)

# Functions ---------------------------------------------------------------

### Function to get the draft positions of a team that drafts <position>th, 
### in a league with x teams, and gets x draft slots overall
get_picks <- function(position, teams, slots) {
  return(seq(from = position, by = teams, length.out = slots))
}


# Constants ---------------------------------------------------------------

league_min <- 555000
salary_cap <- 66553041.75 # Not updated for 2020
team_count <- 12
roster_size <- 30
salary_year <- 2019

# First number is team #, index position is order
draft_order <- order(c(2, 3, 12, 6, 5, 4, 8, 7, 9, 10, 1, 11)) 


# Read in Data ------------------------------------------------------------

### Import the USA Today Salary data set 
salary_data <- read_csv("data/usatoday_salary.csv") %>%
  mutate(salary = as.numeric(gsub("[$,.]", "", salary))) %>%
  filter(year == salary_year)  %>%
  select(-aav) %>%
  
  ### Remove periods from names
  mutate(name = gsub("\\.","", name)) %>%
  
  ### Remove accents from names
  mutate(name = iconv(as.character(name), from="UTF-8", to="ASCII//TRANSLIT"))
  
# Run the player projection script
source("project-player-value.R")
steamer_proj <- weightedProj %>%
  rename(WAR = weightedWAR)


# First pass of keeper optimization ---------------------------------------

# Init the data frame
keepers_max <- tibble()

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
      is.na(salary)  ~ league_min, # / 10^5, 
      !is.na(salary) ~ salary)) %>% # / 10^5)) %>%
    
    ### If there's no WAR projection, assume they're replacement level
    mutate(WAR = case_when(
      is.na(WAR)  ~ 0,
      !is.na(WAR) ~ WAR))
  
  ### Use the adagio implimentation of the knapsack problem solution to find optimal keepers
  keepers <- mknapsack::knapsack(volume = merged_keeper$salary, 
                                 profit = merged_keeper$WAR, 
                                 cap = salary_cap) # Satisfy salary cap
  
  ### Make a data frame of just the players to keep
  best_roster <- merged_keeper[as.logical(keepers), ]
  
  ### Add it to the overall dataframe
  keepers_max <- bind_rows(keepers_max, best_roster)
  rm(keeper_roster, merged_keeper, best_roster)
}

### Make a summary dataframe for each team
team_summary <- keepers_max %>%
  group_by(fantasy_team) %>%
  summarize(keepers = n(), WAR = sum(WAR), salary = sum(salary)) %>%
  mutate(picks = roster_size - keepers)

### Write the data to CSVs
write_csv(keepers_max, "data/DFL_keepers_max.csv")

# Attempt to value draft slots --------------------------------------------

player_stats <- steamer_proj %>%
  left_join(salary_data, by = c("Name" = "name")) %>%
  mutate(salary = case_when(
    is.na(salary)  ~ league_min,
    !is.na(salary) ~ salary)) %>%
  mutate(WAR = case_when(
    is.na(WAR)  ~ 0,
    !is.na(WAR) ~ WAR))

keepers_current <- keepers_max
teams_trimmed <- 1

### Here is where the while loop needs to start
while (teams_trimmed > 0) {
  ### Find the draftable players based on keeper optimization
  draftable_players <- player_stats %>%
    anti_join(keepers_current, by = c("Name" = "name")) %>%
    arrange(desc(WAR))
  
  ### Try to predict the WAR value of each draft slot. Wiggle is the width of the rolling average

  draft_wiggle <- 3
  slot_values <- draftable_players %>%
    mutate(pick = rank(-WAR, ties = "first"), 
           est_WAR = WAR) %>%
    select(pick, WAR, est_WAR)
  slot_values$pick <- 1:nrow(slot_values)
  
  keepers_adj <- data_frame()
  
  ### Do this better: Stepwise, remove one pick at a time, throw it back into the pool.
  teams_trimmed <- 0
  for (i in 1:team_count) {
    team_number <- i
    ### Load the list of keepers from the last iteraton
    keepers_team <- keepers_current %>%
      filter(fantasy_team == team_number)
    
    ### Find out how many picks the team has
    number_picks <- roster_size - nrow(keepers_team)
    
    ### Estimate the position of their last pick (very rough)
    last_pick <- max(get_picks(draft_order[i], team_count, number_picks))
    
    ### Estimate the value of their last pick (very rough)
    last_pick_value <- slot_values %>%
      filter(pick == last_pick) %>%
      pull(est_WAR)
    
    ### If the value of their last pick is more than one of their keepers, drop the least valuable keeper
    if (last_pick_value > min(keepers_team$WAR)) {
      keepers_team <- keepers_team %>%
        arrange(desc(WAR)) %>%
        head(-1) 
      teams_trimmed <- teams_trimmed + 1
    } else {
      keepers_team <- keepers_team %>%
        arrange(desc(WAR))
    }
    keepers_adj <- bind_rows(keepers_adj, keepers_team)
    
  }
  keepers_current <- keepers_adj
}

### Make a summary table with revisions
team_summary <- keepers_max %>%
  group_by(fantasy_team) %>%
  summarize(keepers_max = n(), WAR_max = sum(WAR), salary_max = sum(salary)) %>%
  full_join(keepers_adj %>% 
              group_by(fantasy_team) %>%
              summarize(keepers_adj = n(), WAR_adj = sum(WAR), salary_adj = sum(salary)), 
            by = "fantasy_team"
  )

write_csv(keepers_adj, "data/DFL_keepers_opt.csv")
write_csv(team_summary, "data/DFL_keepers_summary.csv")
