library(tidyverse)


# Constants ---------------------------------------------------------------

# The year for the projections
year <- 2018

# Weights for each projection system, don't need to add up to 1
weights <- c(
  fan = 1, 
  fgdepth = 1.5,
  steamer = 1.5)

#weights <- weights/sum(weights) # Fix the weights do they do add up to 1

# Read in all of the projection files
files <- list.files(path = "./data")[grepl("proj",list.files(path = "./data"))]

### Find the number of projection systems based off the filenames
systems <- str_split(files, "_") %>%
  map(~.[2]) %>%
  unique() %>%
  unlist()

### Load all of the projections into one file
allProj <- map_dfr(systems, function(system) {
batters <- read_csv(paste0("data/proj_", system, "_", year, "_b.csv")) %>%
  select(Name, playerid, WAR, ADP) %>%
  mutate(playerid = as.character(playerid), pos = "B")

pitchers <- read_csv(paste0("data/proj_", system, "_", year, "_p.csv")) %>%
  select(Name, playerid, WAR, ADP) %>%
  mutate(playerid = as.character(playerid), pos = "P")

allPlayers <- bind_rows(batters, pitchers) %>%
#  rename_at(vars(WAR, ADP), funs(paste0(., "_", system)))
  mutate(system = system) %>%
  arrange(ADP)
})

# Make a wide untidy dataframe of projections
allProjWide <- allProj %>%
  select(Name, system, WAR, pos) %>%
  arrange(system, desc(WAR)) %>%
  distinct(Name, system, .keep_all = TRUE) %>%
  spread(system, WAR) %>%
  arrange(desc(steamer))

# Calculate the projections based on weights
weightedProj <- allProj %>% 
  group_by(Name, playerid) %>%
  mutate(weight = case_when( # If there's no weight for a specific system, use the average weight of all systems
    is.na(weights[system]) ~ mean(weights),
    !is.na(weights[system]) ~ weights[system])
    ) %>%
  summarize(systems = n(),
            meanWAR = mean(WAR),
            sdWAR = sd(WAR),
            weightedWAR = sum(weight * WAR) / sum(weight)) %>%
  arrange(desc(sdWAR)) %>%
  left_join(allProjWide)

hypeScore <- weightedProj %>%
  # mutate(hype = case_when(
  #   weightedWAR > 1 ~ (fan - meanWAR)/meanWAR,
  #   weightedWAR <= 1 ~ as.numeric(NA)
  #   )) %>%
#  mutate(hype = (fan - meanWAR)/meanWAR) %>%
  mutate(hype = (fan - meanWAR)) %>%
  filter(!is.na(hype), is.finite(hype), meanWAR >= 0.5) %>%
  mutate(hype = (hype - mean(hypeScore$hype))) %>%
  arrange(desc(hype))

hypeScore %>%
  ggplot(aes(x = meanWAR, y = hype)) +
  geom_point() +
  geom_smooth()



allProj %>% mutate(weight = weights[system]) %>% filter(Name == "Yoan Moncada")
