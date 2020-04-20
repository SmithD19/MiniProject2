source("scripts/cleaning-data.R")

# Morsitans, Cantans, Caspius are very rare species - omit
rare_species <- c("morsitans", "cantans", "caspius")
df <- tidydata %>% filter(!species %in% rare_species)

library(bayesplot)
library(tidybayes)
library(brms)
