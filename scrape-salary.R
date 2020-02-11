library(tidyverse)
library(rvest)

salary_data <- tibble()
for (year in 2019:2019) {
  url <- paste0("https://www.usatoday.com/sports/mlb/salaries/", year, "/player/all/")
  
  response <- read_html(url)
  
  ### Get the episode date
  name <- response %>%
    html_nodes(".player_display") %>%
    html_text() %>%
    trimws()
  
  salary <- response %>%
    html_nodes(".salary") %>%
    html_text() %>%
    tail(-1) %>%
    gsub("\\$", "", .) %>%
    gsub(",", "", .) %>%
    trimws() %>%
    as.numeric()
  
  aav <- response %>%
    html_nodes(".salary_average") %>%
    html_text() %>%
    gsub("\\$", "", .) %>%
    gsub(",", "", .) %>%
    trimws() %>%
    as.numeric()
  
  salary_data <- salary_data %>%
    bind_rows(tibble(name = name, year = year, salary = salary, aav = aav))
}

write_csv(salary_data, "data/usatoday_salary.csv")
