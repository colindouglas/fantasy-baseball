library(adagio)
library(tidyverse)

league_min <- 535000
salary_cap <- 75000000

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

# Init the data frame
all_rosters <- data_frame()

for (i in 1:12) {
  
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
  all_rosters <- bind_rows(all_rosters, best_roster)
}

### Make a summary dataframe for each team
team_summary <- all_rosters %>%
  group_by(fantasy_team) %>%
  summarize(keepers = n(), WAR = sum(WAR), salary = sum(salary))

### Write the data to CSVs
write_csv(all_rosters, "data/DFL_rosters_opt.csv")
write_csv(team_summary, "data/DFL_rosters_opt_summary.csv")