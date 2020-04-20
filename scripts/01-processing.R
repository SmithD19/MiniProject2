# Daniel Smith
# dansmi@ceh.ac,uk
# smithd19@github

#+# Specifying models using rethinking.
#+# This will hopefully help me understand how paramters adn data are linked
#+# and their function within a model

#+ Preparing the data-------------------------------------------------------
library(tidyverse)
library(skimr)
library(janitor)

# Use a manually edited version - got rid of entry errors (text)
dfraw <- read_csv("data/full-data (copy).csv")
dfraw <- dfraw %>% clean_names()

# give everything a unique rowid
dfraw <- dfraw %>% mutate(row = 1:nrow(dfraw))

# convenience functions 

qhist <- function(df) {
  df %>%
    keep(is.numeric) %>% 
    gather() %>% 
    ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_histogram()
}

qdot <- function(df) {
  df %>%
    keep(is.numeric) %>% 
    gather() %>% 
    ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_dotplot()
  
}

#+ a few easy cleaning actions
dfraw[dfraw == "nr"] <- NA
dfraw[dfraw == "nd"] <- NA
dfraw[dfraw == "n/a"] <- NA
dfraw[dfraw == "no"] <- 0
dfraw[dfraw == "yes"] <- 1

#+ Abundance of mosquito larvae -----
dfabun <- dfraw %>% 
  # Abundance of the mosquito larvae - taking the totals only
  select(total = totals_n_larvae,
         cx_pipiens = totals_n_cx_pipiens,
         an_maculipennis = totals_n_an_maculipennis,
         an_claviger = totals_n_an_claviger,
         cs_annulata = totals_n_cs_annulata,
         cs_morsitans = totals_n_cs_morsitans,
         cs_cantans = totals_n_oc_annulipes_cantans,
         cs_caspius = totals_n_oc_caspius
  )

dfabun %>% skim()

#+ Site Data -----
dfsite <- dfraw %>% 
  # We'll select the site level factors we care about along with rowname
  select(row,
         site = site_x,
         plot = plot_x,
         location = location_x,
         dip_point = dip_point_x,
         eastings_gps = new_averaged_gps_readings_eastings,
         northings_gps = new_averaged_gps_readings_northings
  )
# vars to mutate
varsite <- dfsite %>% select(everything(), -row) %>% colnames()
# mutate the vars to factors
dfsite <- dfsite %>% mutate_at(.vars = vars(varsite), ~ as.factor(.))
  
skim(dfsite)

#+ Temporal/Seasonal Data -----
dfseason <- dfraw %>% 
  # These factors are likely to be "random" effects
  select(row,
         year = year_x,
         season = season_x,
  ) %>% 
  mutate_at(vars(everything(), -row), ~as.factor(.))

skim(dfseason)

#+ Abiotic Structural Factors -----
dfstruc <- dfraw %>% 
  # These are structural characteristics of the environment
  select(row,
         # dry = rhyne_dry_x,
         landcover = la_n_acover_rough_pasture_rough_pasture_reed_bed_wet_woodla_na,
         cleared = rhyne_cleared,
         width = average_width_m,
         # length = approx_length_m, # Why is this such a long name.....
         exposure = exposure_1_sun_3_shade_2_partial_shade_4_open_but_emergent_vegetation_within_rhyne_5_open_but_shaded_by_very_high_bank_or_dense_overhanging_bank_vegetation,
         management = water_tier_management_x,
         shaded = percentage_water_shaded
         ) %>% # turn shaded to a percentage
  mutate(shaded = shaded/100) %>% 
  # factors for variables
  mutate_at(vars(landcover, cleared, exposure, management), ~ as.factor(.))

skim(dfstruc)

# running `table` on variables that arent properly defined lets us see rogue values
dfstruc %>% select(-row) %>% sapply(table)

#+ Abiotic Water Chemistry -----
dfchem <- dfraw %>% 
  # water chemistry parameters. Important for macroinverts
  select(row,
         wtemp = water_temperature_c,
         turbidity,
         salinity,
         do = dissolved_oxygen,
         ph = p_h_probe
  ) 

dfchem %>% skim()

# some outliers
qdot(dfchem)

dfchem <- # fixed outliers
  dfchem %>% mutate(
    # unreasonable to be higher than this
    turbidity = ifelse(turbidity > 3, NA, turbidity),
    # definiteley not higher than 40
    wtemp = ifelse(wtemp > 40, NA, wtemp),
    # some negative ph values in dataset
    ph = ifelse(ph < 0, NA, ph),
    # do unlikely to be lower than this
    do = ifelse(do < 50, NA, do)
  ) 

#+ Biotic Co-occurrance -----
dfcoocurr <- dfraw %>% 
  # cooccurring species
  select(row,
         buttercup:ladies_frock) %>% 
  # mutate to factors
  mutate_at(vars(everything(), -row), ~ as.factor(.))

# Check factor levels - ones with just one factor level may as well be removed
faclvl <- sapply(dfcoocurr, levels) %>% sapply(length)
faclvl2 <- names(faclvl[faclvl == 1])

# Deselect these
dfcoocurr <- dfcoocurr %>% 
  select(-faclvl2)

#+ Bind it together in a wide tidy format -----
dftidy_wide <- bind_cols(dfabun, dfsite, dfstruc,
                    dfcoocurr, dfchem, dfseason) %>% 
  # Check they all line up right
  # dftidy %>% select(starts_with("row")) # Yes
  select(-c("row1", "row2", "row3", "row4"))

#+ Arrange into long format -----
dftidy_long <- dftidy_wide %>% 
  pivot_longer(cols = colnames(dfabun)[-1], 
               names_to = "species", 
               values_to = "abundance")

#+ Make an aggregated format with just totals -----
dfaggregated <- dftidy_long %>% 
  select(-species, -abundance) %>% 
  distinct()

#+ Final Checks for Numeric Vars
qhist(dftidy_long)

#+ END - DATA CLEANED -----
save.image("rdat/01-processing.RData")
