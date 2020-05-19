library(dplyr)
library(Hmsc)

dfhmsc <- read_rds("hmsc/dfhmsc2.rds")

# Mosquito species names
n_mosquito <-
  dfhmsc %>% select(cx_pipiens:cs_annulata) %>% colnames()

# Predator species names
n_pred <- dfhmsc %>% select(fish:gammarus) %>% colnames()

# Numeric covariate names
n_num_cov <- dfhmsc %>%
  select(
    # These are structural variables
    width,
    waterboard,
    shaded,
    edgedepth,
    centredepth,
    # These are water chemistry variables - probably not reliable lets remove
    # wtemp,
    # turbidity,
    # salinity,
    # do,
    # ph,
    # The functional plant coverings
    cover_emergent,
    cover_surface, 
    cover_vertical
  ) %>%
  # The names of these cols
  colnames()

# Presence Absence data - Only include Mosquito species
Y <- dfhmsc %>% select(n_mosquito) %>% data.matrix()

# Covariates Species Occurence (mutate to binary)
X.co <- dfhmsc %>% select(n_pred) %>% mutate_all(.funs = function(x) ifelse(x > 0, 1, 0)) 

# Covariates Numeric
X.num <- dfhmsc %>% select(n_num_cov)

# Covariates Categorical
X.cat <- dfhmsc %>% select(management, season) %>% mutate_all(as.character)

# Covariates Landcover
X.lan <- dfhmsc %>% select(arable_horticulture:freshwater)

# cbind
X.dat <- cbind(X.num, X.cat, X.lan, X.co)

# Geographical co-ordinates for each site
xy <- dfhmsc %>% select(easting, northing) %>% data.matrix()

# Study Design
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
  #NEEDS TO BE A DATA FRAME DONT USE TIBBLES 
  # WASTED AN HOUR ON THIS
  as.data.frame()

# Spatial latent random variables
rL1 <- HmscRandomLevel(units = unique(studydesign[,1]))
rL2 <- HmscRandomLevel(units = unique(studydesign[,2]))

# Formula for regression
XFormula <- as.formula(paste("~" ,paste(colnames(X.dat), collapse = "+")))


# Models for PA with predators as poisson predictors of presence -----------

# This is a full model with co-occuring predators
ABU2_Full_PredPred <- Hmsc(
  # A matrix of species occurence/abundance records
  Y = Y,
  # A formula object for linear regression
  XFormula = XFormula,
  # A matrix of measured covariates
  X = model.matrix(XFormula, data = X.dat),
  # Distribution
  distr = "lognormal poisson",
  # Study Design
  studyDesign = studydesign,
  # Random Levels
  ranLevels = list(site = rL1, plot_id = rL2)
)



# Model with predators as a response variable -----------------------------

# Presence Absence data - include Mosquito species & Predator
Y3 <- dfhmsc %>% select(n_mosquito, n_pred) %>% data.matrix()

# Covariates Numeric
X.num3 <- dfhmsc %>% select(n_num_cov)

# Covariates Categorical
X.cat3 <- dfhmsc %>% select(management, season) %>% mutate_all(as.character)

# Covariates Landcover
X.lan3 <- dfhmsc %>% select(arable_horticulture:freshwater)

# cbind
X.dat3 <- cbind(X.num3, X.cat3, X.lan3)

# Formula for regression
XFormula3 <- as.formula(paste("~" ,paste(colnames(X.dat3), collapse = "+")))

# This is a full model with co-occuring predators
ABU2_Full_PredResp <- Hmsc(
  # A matrix of species occurence/abundance records
  Y = Y3,
  # A formula object for linear regression
  XFormula = XFormula3,
  # A matrix of measured covariates
  X = model.matrix(XFormula3, data = X.dat3),
  # Distribution
  distr = "lognormal poisson",
  # Study Design
  studyDesign = studydesign,
  # Random Levels
  ranLevels = list(site = rL1, plot_id = rL2)
)

# Start Models ------------------------------------------------------------

# List them for for loop
models <- list("ABU2_Full_PredPred" = ABU2_Full_PredPred,
               "ABU2_Full_PredResp" = ABU2_Full_PredResp)

# Test run or not ---------------------------------------------------
test.run = F

if (test.run) {
  # with this option mcmc runs fast for checking
  thin = 1
  samples = 10
  transient = 5
  verbose = 5
} else {
  # with this option mcmc runs slow for analysis
  thin = 100 ############ This has been edted for longer runtime on abundance models that failed to converge last time
  samples = 2000
  adaptNf = rep(ceiling(0.4 * samples * thin), 1)
  transient = ceiling(0.5 * samples * thin)
  verbose = 500 * thin
}

# Number of MCMC chains
nChains = 8

# Create File Path if doesnt exist
ifelse(!dir.exists(file.path("hmsc/", "abu2_models")), dir.create(file.path("hmsc/", "abu2_models")), FALSE)

# Run models in loop
for (i in seq_along(models)) {
  # Timings
  ptm = proc.time()
  
  m = sampleMcmc(
    models[[i]],
    thin = thin,
    samples = samples,
    transient = transient,
    #adaptNf = adaptNf,
    nChains = nChains,
    verbose = verbose,
    nParallel = nChains
  )
  # Timings
  computationtime = proc.time() - ptm
  # Filename Saving
  filename = file.path("hmsc/abu2_models", paste0(names(models)[[i]], ".RData"))
  # Save file
  save(m, file = filename, computationtime)
}
