library(dplyr)
library(Hmsc)

load("hmsc/dfhmsc.RData")

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
    total_cover,
    # These are water chemistry variables
    wtemp,
    turbidity,
    salinity,
    do,
    ph
  ) %>%
  # The names of these cols
  colnames()

# Abundance data
Y <- dfhmsc %>% select(n_mosquito, n_pred) %>% data.matrix()

# Covariates Numeric
X.num <- dfhmsc %>% select(n_num_cov)

# Covariates Categorical
X.cat <- dfhmsc %>% select(management, landcover) %>% mutate_all(as.character) %>% as.matrix()

# Covariates Landcover
X.lan <- dfhmsc %>% select(arable_horticulture:freshwater) %>% as.matrix()

# cbind
X.dat <- cbind(X.num, X.cat, X.lan)

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


# Models for Abundances ---------------------------------------------

# Environmental covariates and Spatial
ABU_SC <- Hmsc(
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

# Environmental covariates only 
ABU_C <- Hmsc(
  # A matrix of species occurence/abundance records
  Y = Y,
  # A formula object for linear regression
  XFormula = XFormula,
  # A matrix of measured covariates
  X = model.matrix(XFormula, data = X.dat),
  # Distribution
  distr = "lognormal poisson",
  # Study Design
  studyDesign = studydesign
)

# Spatial latent variables only
ABU_S <- Hmsc(
  # A matrix of species occurence/abundance records
  Y = Y,
  # A matrix of measured covariates
  X = model.matrix(XFormula, data = X.dat),
  # A formula object for linear regression
  XFormula = NULL,
  # Distribution
  distr = "lognormal poisson",
  # Study Design
  studyDesign = studydesign,
  # Random Levels
  ranLevels = list(site = rL1, plot_id = rL2)
)

# Null model with only abundances
ABU_N <- Hmsc(
  # A matrix of species occurence/abundance records
  Y = Y,
  # A matrix of measured covariates
  X = model.matrix(XFormula, data = X.dat),
  # A formula object for linear regression
  XFormula = NULL,
  # Distribution
  distr = "lognormal poisson",
  # Study Design
  studyDesign = studydesign,
)

# List them for for loop
abundancemodels <- list("ABU_SC" = ABU_SC, "ABU_C" = ABU_C, 
                        "ABU_S" = ABU_S, "ABU_N" = ABU_N)

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
  thin = 100
  samples = 1000
  adaptNf = rep(ceiling(0.4 * samples * thin), 1)
  transient = ceiling(0.5 * samples * thin)
  verbose = 500 * thin
}

# Number of MCMC chains
nChains = 8


for (i in seq_along(abundancemodels)) {
  # Timings
  ptm = proc.time()
  
  m = sampleMcmc(
    abundancemodels[[i]],
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
  filename = file.path("hmsc/abundance_models", paste0(names(abundancemodels)[[i]], ".RData"))
  # Save file
  save(m, file = filename, computationtime)
}

