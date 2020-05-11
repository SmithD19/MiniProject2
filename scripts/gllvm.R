### gllvm approach

# Load the master data
load("masterdataframe.RData")

# Packages for manipulation and modelling
library(gllvm)
library(tidyverse)

# Example data
data("antTraits")

# Need to create a unique ID for each Year/Season/Site/Plot for this to be working across all years.
# For now let's just take the 2010 data as it is the most complete
df <- dftidy_wide %>% filter(year == 2010)

# Species variable names
species_names <- colnames(dftidy_wide)[2:5]

# Binary co-occurrence variable names
env_names <- df %>% select(buttercup:ladies_frock) %>% colnames()

# Environmental co-variates - just use width and shaded for now
# unsure if categorical variables work with this package
env2_names <- df %>% select(width, shaded, wtemp:ph, total_cover) %>% colnames()

# Drop the value if total abundance is NA - this means data was not recorded so is useless to us
df <- df %>% drop_na(species, cooccurrence, covariate)

# Rows as unique sites and columns as species, First creating unique site names
df <- df %>% unite("ID", c(site, plot, dip_point, year, season))

# # Need to make sure abundance numbers are binary now
# df <- df %>% mutate(
#   cx_pipiens = if_else(cx_pipiens > 1,1,0),
#   an_maculipennis = if_else(an_maculipennis > 1,1,0),
#   an_claviger = if_else(an_claviger > 1,1,0),
#   cs_annulata = if_else(cs_annulata > 1,1,0))

# Presence absence needs to be encoded as numeric not factors
df <- df %>%  
  mutate_at(vars(cooccurrence), as.character) %>% # character first
  mutate_at(vars(cooccurrence), as.numeric) # then numeric

# Now create the matrix for community presence absence
abun <- df %>% select(species)

# Assemble an environmental covariate dataframe
env <-  df %>% select(all_of(env2_names))

# ----- Model Based Ordination using gllvm:

# Absolute values for null mnodel fits using gllvm 
fitpoi <- gllvm(y = abun, family = poisson(), starting.val = "zero") # AICc 4566
fitnegb <- gllvm(y = abun, family = "negative.binomial", starting.val = "zero") # AICc 4456
fitzip <- gllvm(y = abun, family = "ZIP", starting.val = "zero") # AICc -2542

# Poisson fits residuals well
plot(fitpoi)
plot(fitnegb)
plot(fitzip)

# How many latent variables? Run the model fitting voer and over again changing the num.lv specification
# then return the AICc and choose the lowest from there
criterias <- vector()

for(i in 1:3){
  fiti <- gllvm(y = abun, X = env , family = poisson(), num.lv = 3, sd.errors = FALSE, 
                formula = ~ width+shaded+wtemp+turbidity+salinity+do+ph+total_cover, seed = 1234)
  criterias[i + 1] <- summary(fiti)$AICc
  names(criterias)[i + 1] = i
}

# Try with abundance data and Environmental covariates


fiti <- gllvm(y = abun, X = env , family = poisson(), num.lv = 2, sd.errors = FALSE, starting.val = "zero",
              formula = ~ width + shaded + wtemp + turbidity + salinity + do + ph + total_cover, seed = 1234)

plot(fiti)

coefplot(fiti)






