library(tidyverse)


# Constants ---------------------------------------------------------------

# The year for the projections
year <- 2018

# Weights for each projection system, don't need to add up to 1
weights <- c(
  fan = 1, 
  fgdepth = 3,
  steamer = 3)

#weights <- weights/sum(weights) # Fix the weights do they do add up to 1

# Read in all of the projection files
files <- list.files(path = "./data")
files <- files[grepl("proj", files)]

### Find the number of projection systems based off the filenames
systems <- str_split(files, "_") %>%
  map(~.[2]) %>%
  unique() %>%
  unlist()

### Load all of the projections into one file
allProj <- map_dfr(systems, function(system) {
batters <- read_csv(paste0("data/proj_", system, "_", year, "_b.csv")) %>%
  select(Name, playerid, WAR, ADP) %>%
  mutate(playerid = as.character(playerid))

pitchers <- read_csv(paste0("data/proj_", system, "_", year, "_p.csv")) %>%
  select(Name, playerid, WAR, ADP) %>%
  mutate(playerid = as.character(playerid))

allPlayers <- bind_rows(batters, pitchers) %>%
#  rename_at(vars(WAR, ADP), funs(paste0(., "_", system)))
  mutate(system = system) %>%
  arrange(ADP)
})


# Calculate the projections based on weights
weightedProj <- allProj %>% 
  group_by(Name, playerid) %>%
  mutate(weight = case_when( # If there's no weight for a specific system, use the average weight of all systems
    is.na(weights[system]) ~ mean(weights),
    !is.na(weights[system]) ~ weights[system])
    ) %>%
  summarize(systems = n(),
            weightedWAR = sum(weight * WAR) / sum(weight), 
            meanWAR = mean(WAR),
            sdWAR = sd(WAR),
            WARs = paste0(WAR, collapse = " | ")) %>%
  mutate(diff = abs(weightedWAR - meanWAR)) %>%
  arrange(desc(diff))

allProj %>% mutate(weight = weights[system]) %>% filter(Name == "Yoan Moncada")
