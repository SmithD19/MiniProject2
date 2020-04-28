# Daniel Smith
# dansmi@ceh.ac,uk
# smithd19@github

# preliminary visulisation and tabulations of correlations and values




# library -----------------------------------------------------------------

library(tidyverse)
#library(ggthemr)

load("rdat/01-processing.RData")

# Theme set ---------------------------------------------------------------

#ggthemr("fresh", layout = "clean")
#ggthemr_reset()

# Abundance ---------------------------------------------------------------

dftidy_long %>% 
  group_by(species, management, season, year) %>% 
  summarise(
    t_abund = sum(abundance, na.rm = T)
  ) %>% 
  ggplot(aes(x = season, y = t_abund, fill = management)) +
  geom_col(position = position_dodge()) +
  facet_wrap(~ species)

dftidy_long %>% 
  ggplot(aes(x = easting, y = northing, col = management)) +
  geom_point()

dftidy_long %>% 
  group_by(site, plot, species) %>% 
  summarise(
    m_abundance = mean(abundance)
  ) %>% 
  ggplot(aes(x = site, y = m_abundance, fill = species)) +
  geom_col(position = position_dodge())

dftidy_long %>% 
  group_by(species, management, season, year) %>% 
  summarise(
    total_abund = sum(abundance, na.rm = T)
  )

dftidy_long %>%
  group_by(species, year, season) %>% 
  tally(abundance) %>% 
  pivot_wider(names_from = c(season, year), values_from = n) %>% 
  write_csv("test.csv")

dftidy_long %>% 
  group_by(location, site, plot) %>% 
  summarise




