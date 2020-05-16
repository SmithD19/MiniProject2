### HMSC Modelling ### 

# Need to try and determine the difference between abiotic and biotic 
# covariates

library(tidyverse)
library(Hmsc)

load("hmsc/dfhmsc.RData")

# Prep --------------------------------------------------------------------

# Mosquito species names
n_mosquito <-
  dfhmsc %>% select(cx_pipiens:cs_annulata) %>% colnames()

# Predator species names
n_pred <- dfhmsc %>% select(fish:gammarus) %>% colnames()

########## Setting hmsc variables ##########

# Abundance data
Y <- dfhmsc %>% select(n_mosquito, n_pred) %>% data.matrix()

########## Change abundance data to P/A ##########
Y[Y > 0] <- 1

# Covariates
X <- dfhmsc %>% select(width:freshwater, -landcover) %>% data.matrix()

# Geographical co-ordinates for each site
xy <- dfhmsc %>% select(easting, northing) %>% data.matrix()

# Step 1. Setting model structure and fitting the model ----------

studydesign <-  dfhmsc %>%
  # study vars
  select(site, plot_id) %>%
  # Change to useful names
  mutate(
    site = paste0("site_", as.character(site)),
    plot_id = paste0("plot_id_", as.character(plot_id))
  ) %>% 
  # As factor
  mutate_all(as.factor) %>%
  # NEEDS TO BE A DATA FRAME DONT USE TIBBLES 
  # WASTED AN HOUR ON THIS
  as.data.frame()


rL1 <- HmscRandomLevel(units = unique(studydesign[,1]))
rL2 <- HmscRandomLevel(units = unique(studydesign[,2]))


# Model Spec ---

paste(colnames(X), collapse = "+")

m1 <- Hmsc(
  # A matrix of species occurence/abundance records
  Y = Y,
  # A formula object for linear regression
  XFormula = ~ width+waterboard+shaded+edgedepth+centredepth+total_cover+wtemp+turbidity+salinity+do+ph+easting+northing+management+arable_horticulture+broadleaved_woodland+improved_grassland+neutral_grassland+rough_grassland+freshwater,
  # A matrix of measured covariates
  X = X,
  # Distribution
  distr = "probit",
  # Study Design
  studyDesign = studydesign,
  # Random Levels
  ranLevels = list(site = rL1, plot_id = rL2)
)


# Step 2. Examining MCMC convergence --------------------------------------

# Number of MCMC chains
nChains = 4

# Test run or not
test.run = F

if (test.run) {
  # with this option mcmc runs fast for checking
  thin = 1
  samples = 10
  transient = 5
  verbose = 5
} else {
  # with this option mcmc runs slow for analysis
  thin = 100
  samples = 1000
  adaptNf = rep(ceiling(0.4 * samples * thin), 1)
  transient = ceiling(0.5 * samples * thin)
  verbose = 500 * thin
}


m = sampleMcmc(
  m1,
  thin = thin,
  samples = samples,
  transient = transient,
  #adaptNf = adaptNf,
  nChains = nChains,
  verbose = verbose,
  nParallel = nChains
)

save(m, file = "model1/mod1_cluster_full.RData")
