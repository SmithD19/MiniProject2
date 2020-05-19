##### HMSC Approach #####

library(tidyverse)
library(Hmsc)
library(corrplot)

# Import abundance data
load("rdat/Processed-Data.RData")

# Reproducible seed
set.seed(1234)

# Import land usage data and bind
load("rdat/buffers.RData")

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
    ph,
    # Plant cover data
    cover_emergent,
    cover_surface,
    cover_vertical
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

saveRDS(dfhmsc, "hmsc/dfhmsc2.rds")

# Setting model structure and fitting the model ----------

# Setting up the study design ---
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

# Random level structure
rL1 <- HmscRandomLevel(units = unique(studydesign[,1]))
rL2 <- HmscRandomLevel(units = unique(studydesign[,2]))


