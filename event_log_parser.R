library(retrosheet)
library(tidyverse)

getRetrosheetYear <- function(p_year) {
  
  # Get a list of teams that were active that year
  # TODO: Pull this from SQL instead
  teams <- getTeamIDs(year = p_year)
  
  # Download the data from retrosheets
  # TODO: Pull this via SQL instead
  team_data <- map(teams, ~ getRetrosheet(type = "play", year = p_year, team = ., stringsAsFactors = FALSE))
  
  # Pull all of the plays out of a given event file
  # TODO: Implement this better based on https://www.retrosheet.org/eventfile.htm
  all_data <- map_dfr(team_data, function(y) { 
    map_dfr(y, function(x) {
      x$play %>%
        as_tibble() %>%
        add_column(date = as.Date(x$info[4,2])) %>%
        mutate(   K = grepl("^K", play),
                  #`SB` = grepl("^SB", play),
                  `1B` = grepl("^S", play),
                  `2B` = grepl("^D", play),
                  `3B` = grepl("^T", play),
                  `HR` = grepl("^HR", play),
                  `BB` = grepl("^W", play),
                  `SH` = grepl("/SH", play),
                  `SF` = grepl("/SF", play),
                  INT  = grepl("INT", play),
                  `E`  = grepl("^E", play),
                  `FC` = grepl("^FC", play),
                  out = grepl("^[0-9]", play),
                  HBP = grepl("^HBP", play),
                  PA = TRUE, 
                  AB = !BB & !HBP & !SF & !SH & !INT)
    })
  }) %>% arrange(date)
  
  return(all_data)
}

this_year <- getRetrosheetYear(2018)
last_year <- getRetrosheetYear(2017)

tor_2018 <- getRetrosheet(type = "play", year = 2018, team = "TOR", stringsAsFactors = FALSE)

game <- tor_2018[[1]]


