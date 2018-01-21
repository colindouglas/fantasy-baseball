library(adagio)
library(tidyverse)

league_min <- 550000

salary_data <- read_csv("data/usatoday_salary.csv") %>%
  filter(year == 2017) 
  
steamer_proj <- read_csv("data/steamer_2018_batters.csv") %>%
  select(Name, Team, WAR, playerid) %>%
  bind_rows(read_csv("data/steamer_2018_pitchers.csv") %>% select(Name, Team, WAR, playerid))

### TODO: Write some sort of lookup to fix this join
not_in_steamer <- anti_join(salary_data, steamer_proj, by = c("name" =  "Name"))

all_data <- inner_join(salary_data, steamer_proj, by = c("name" = "Name"))

set.seed(69420)

test_set <- all_data %>%
  filter(WAR > 1) %>%
  filter(row_number() %in% sample(n(), 20)) %>%
  mutate(salary = salary / 10^6) %>%
  arrange(WAR)

result <- knapsack(test_set$salary, test_set$WAR, cap = 75) # Satisfy salary cap

result <- knapsack(rep(1,20), test_set$WAR, cap = 4) # Satisfy roster slots

print(test_set[result$indices,])
paste("WAR:", sum(test_set$WAR[result$indices]))
paste("Salary:", sum(test_set$salary[result$indices]))




##### Find the expected value of a draft pick at each slot
# Then make a team that is 