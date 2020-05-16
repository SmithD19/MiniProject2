##### HMSC Approach #####

library(tidyverse)
library(Hmsc)
library(corrplot)

# Import abundance data
load("masterdf2.RDS")

# Reproducible seed
set.seed(1234)

# Import land usage data and bind
load("data/buffers.RData")

# Bind land usage data to master data
dftidy_wide <- dftidy_wide %>% left_join(df100m)


# Only use year 2010
df <- dftidy_wide %>% filter(year == "2010")

# Step 0. Creating Hmsc data ----------------------------------------------


# Aggregate geographical points together
xy_agg <- df %>% 
  # Grouping
  group_by(site, plot, season) %>% 
  # Means for each site/plot
  summarise_at(
    .vars = vars(easting, northing),
    .funs = function(x) as.integer(mean(x, na.rm = T))
  )


# Aggregate factoral variables 
fac_agg <- df %>% 
  # Grouping
  group_by(site, plot, season) %>% 
  # Aggregate
  summarise_at(
    # the factor vars we care about
    .vars = vars(management, landcover),
    # this function chooses the most common factor level for each grouping
    .funs = function(x) names(table(x))[which.max(table(x))]
  )


# Mosquito species names
n_mosquito <-
  df %>% select(cx_pipiens:cs_annulata) %>% colnames()

# Predator species names
n_pred <- df %>% select(fish:gammarus) %>% colnames()


# Numeric covariate names
n_num_cov <- df %>%
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


# Aggregate abundance data from dip_point to plot level
abundance_agg <- df %>%
  # Grouping
  group_by(site, plot, season) %>%
  # Aggregating
  summarise_at(
    .vars = vars(n_mosquito, n_pred),
    # Sum
    .funs = function(x) as.integer(sum(x, na.rm = T))
  )


# Which indexes are na in abundance_agg
abundance_ind <- which(is.na(abundance_agg))


# Aggregate the numeric covariate data
num_cov_agg <- df %>% 
  # Grouping
  group_by(site, plot, season) %>%
  # Aggregating
  summarise_at(
    .vars = vars(n_num_cov),
    # Mean for average value across dips
    .funs = function(x) median(x, na.rm = T)
  )

# Which indexes are NA in num_cov_agg and how many NA in each column?
num_cov_ind <- 
  # Return table of which indexs are NA
  lapply(num_cov_agg, function(x) which(is.na(x))) %>% 
  unlist() %>% 
  unique()


# Join the summarised dataframes together 
bound <-
  abundance_agg %>% 
  left_join(num_cov_agg) %>% 
  left_join(xy_agg) %>% 
  left_join(fac_agg) %>% 
  left_join(df100m) %>% 
  # Make a unique column for plot ID
  unite(c(site, plot), col = "plot_id", sep = "_", remove = FALSE) %>% 
  # Mae plot_id a factor
  mutate(plot_id = as.factor(plot_id))


# Remove indexes that have NA values into hmsc dataframe
dfhmsc <- bound[-num_cov_ind,] %>% 
  # Ungroup as well to stop dplyr doing things
  ungroup() %>% as.data.frame()

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


# Setting up the study design ---
# studydesign = matrix(NA, nrow(Y), 3)
# studydesign[,1] = sprintf("Site_%.2d", dfhmsc$site)
# studydesign[,2] = sprintf("Plot_%.1d", dfhmsc$plot)
# studydesign[,3] = sprintf("Season_%.1d", dfhmsc$season)
# studydesign = as.data.frame(studydesign)
# colnames(studydesign) = c("Site", "Plot", "Year")
# studydesign[,1] = as.factor(studydesign[,1])
# studydesign[,2] = as.factor(studydesign[,2])
# studydesign[,3] = as.factor(studydesign[,3])

# OR
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

# # Setting up random effect structure (spatial latent var)
# xy <- dfhmsc %>% 
#   select(easting, northing) %>% 
#   data.matrix()
# colnames(xy) <- c("x", "y")
# sRL <- xy
# rownames(sRL) <- unique(dfhmsc$plot_id)
# # Create random levels
# rL <- HmscRandomLevel(sData = sRL)
# # Min and max latent factors
# rL$nfMin <- 5
# rL$nfMax <- 10

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
nChains = 2

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

save(m, file = "model1/mod1_full.RData")

# Step 3. Evaluating model fit --------------------------------------------

post <- convertToCodaObject(m)
post

esBeta <- effectiveSize(post$Beta)
esBeta

plot(post$Beta)

psrfBeta <- gelman.diag(post$Beta)
psrfBeta

predY <- computePredictedValues(m)
predY

MF <- evaluateModelFit(m, predY)
MF

# Variance partitioning
groups <- c(1)
groupnames <- "Abiotic"
VP1 <- computeVariancePartitioning(m, groups, groupnames, na.ignore = TRUE)

# Correlations
OmegaCor <- computeAssociations(m)
supportLevel = 0.50

toplot <- 
  ((OmegaCor[[1]]$support > supportLevel) + 
     (OmegaCor[[1]]$support<(1-supportLevel))>0) * OmegaCor[[1]]$mean 

corrplot(toplot, method = "color", typ = "lower")

plotVariancePartitioning(m, VP1)





