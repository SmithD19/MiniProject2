### BayesComm approach

# Load the master data
load("masterdataframe.RData")

# Packages for manipulation and modelling
library(BayesComm)
library(tidyverse)

# Need to create a unique ID for each Year/Season/Site/Plot for this to be working across all years.
# For now let's just take the 2010 data as it is the most complete
df <- dftidy_wide %>% filter(year == 2010)

# Species variable names
species <- colnames(dftidy_wide)[2:5]

# Binary co-occurrence variable names
cooccurrence <- df %>% select(buttercup:ladies_frock) %>% colnames()

# Environmental co-variates - just use width and shaded for now
# unsure if categorical variables work with this package
covariate <- df %>% select(width, shaded, wtemp:ph, total_cover) %>% colnames()

# Drop the value if total abundance is NA - this means data was not recorded so is useless to us
df <- df %>% drop_na(species, cooccurrence, covariate)

# Rows as unique sites and columns as species, First creating unique site names
df <- df %>% unite("ID", c(site, plot, dip_point, year, season))

# Need to make sure abundance numbers are binary now
df <- df %>% mutate(
  cx_pipiens = if_else(cx_pipiens > 1,1,0),
  an_maculipennis = if_else(an_maculipennis > 1,1,0),
  an_claviger = if_else(an_claviger > 1,1,0),
  cs_annulata = if_else(cs_annulata > 1,1,0))

# Presence absence needs to be encoded as numeric not factors
df <- df %>%  
  mutate_at(vars(cooccurrence), as.character) %>% # character first
  mutate_at(vars(cooccurrence), as.numeric) # then numeric

# Now create the matrix for community presence absence
community_mat <- df %>% select(species) %>% data.matrix()
# Set the rownames to sites
rownames(community_mat) <- df %>% pull(ID)

# Now create the matrix for environmental covariates
covariate_mat <- df %>% select(covariate) %>% data.matrix()
# Set the rownames to sites
rownames(covariate_mat) <- df %>% pull(ID)

# For some reason R session crashes if coocurances and mosquito Pr/Abs included in matrix Y of BC()
# Is this the intended use? Or should they be in the community matrix alongside mosquitoes
# Figured it out. choleskey decomposition can't happen on a variable that has no variance
# Some variables have no variance in cooccurrence. The variables with all or 0 values need to be removed.
counts <- df %>% pivot_longer(cooccurrence) %>% count(name, value) %>% filter(n == 952 | n == 0)

# These are the named variables 
remove <- counts %>% pull(name)

# Matrix for coocurrence data - with modifcations described above
cooccurr_mat <- df %>% select(cooccurrence) %>% select(-remove) %>% data.matrix()

# Full community matrix
com <- cbind(community_mat, cooccurr_mat)


# Let's try a BC model - gives error community matrix not symetrical
null <- BC(Y = com, X = covariate_mat, model = "null", its = 100)
environment <- BC(Y = com, X = covariate_mat, model = "environment", its = 100)
community <- BC(Y = com, X = covariate_mat, model = "community", its = 100)
full <- BC(Y = com, X = covariate_mat, model = "full", its = 100)

# Deviance partitioning
deviance <- devpart(null, environment, community, full)

# Proportion of deviance explained by each parameter of the model explained
deviance




