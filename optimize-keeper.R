library(adagio)
library(tidyverse)

league_min <- 535000
salary_cap <- 75000000

# Read in Data ------------------------------------------------------------

salary_data <- read_csv("data/usatoday_salary.csv") %>%
  filter(year == 2017)  %>%
  mutate(name = gsub("\\.","", name)) %>%
  mutate(name = iconv(as.character(name), from="UTF-8", to="ASCII//TRANSLIT"))
  
steamer_proj <- read_csv("data/steamer_2018_batters.csv") %>%
  select(Name, Team, WAR, playerid) %>%
  bind_rows(read_csv("data/steamer_2018_pitchers.csv") %>% select(Name, Team, WAR, playerid)) %>%
  mutate(Name = gsub("\\.","", Name)) %>%
  mutate(Name = iconv(as.character(Name), from="UTF-8", to="ASCII//TRANSLIT"))


all_rosters <- data_frame()

for (i in 1:12) {
  keeper_roster <- read_csv("data/DFL_rosters.csv") %>%
    filter(fantasy_team == i) %>%
    mutate(name = gsub("\\.","", name)) %>%
    mutate(name = iconv(as.character(name), from="UTF-8", to="ASCII//TRANSLIT"))
  
  merged_keeper <- left_join(keeper_roster, steamer_proj, by = c("name" = "Name")) %>%
    left_join(salary_data, by = c("name" = "name"))  %>%
    mutate(salary = case_when(
      is.na(salary)  ~ league_min / 10^5,
      !is.na(salary) ~ salary / 10^5)) %>%
    mutate(WAR = case_when(
      is.na(WAR)  ~ 0,
      !is.na(WAR) ~ WAR))
  
  
  keepers <- knapsack(merged_keeper$salary, merged_keeper$WAR, cap = 750) # Satisfy salary cap
  
  best_roster <- data_frame(name = merged_keeper$name[keepers$indices],
                            salary = merged_keeper$salary[keepers$indices] * 10^5,
                            WAR = merged_keeper$WAR[keepers$indices],
                            fantasy_team = i)
  
  all_rosters <- bind_rows(all_rosters, best_roster)
}

team_summary <- all_rosters %>%
  group_by(fantasy_team) %>%
  summarize(WAR = sum(WAR), salary = sum(salary))

write_csv(all_rosters, "data/DFL_rosters_opt.csv")
write_csv(team_summary, "data/DFL_rosters_opt_summary.csv")


##### Find the expected value of a draft pick at each slot
# Then make a team that is 