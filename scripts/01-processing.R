# Daniel Smith
# dansmi@ceh.ac,uk
# smithd19@github

#+# Data processing for somerset levels in Tidyverse.
#+# This will hopefully help me understand how paramters adn data are linked
#+# and their function within a model

#+ Preparing the data-------------------------------------------------------
library(tidyverse)
library(skimr)
library(janitor)

# Use a manually edited version - the copy - to get rid of entry errors
# removed text and punctuation in columns mostly - could have automated
# but cleaning is long enough already

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
dfraw[dfraw == "no"] <- "0"
dfraw[dfraw == "yes"] <- "1"

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
dfsite <- dfsite %>% mutate_at(.vars = vars(varsite), ~ as.factor(.)) %>% 
  # eastings and northings off by some margin. Missing first digit of grid reference
  mutate(eastings_gps = as.integer(as.character(eastings_gps)) + 300000,
         northings_gps = as.integer(as.character(northings_gps)) + 100000)

skim(dfsite)

# Must try and create eastings and northings at a plot level
northeast <- dfsite %>% group_by(site, plot) %>%
  # median values for this grouping and return to df
  summarise(
    easting = median(eastings_gps, na.rm = T),
    northing = median(northings_gps, na.rm = T)
  )

# Join this
dfsite <- dfsite %>% left_join(northeast, by = c("site", "plot")) %>% 
  select(-eastings_gps, -northings_gps)

# Check by plotting
dfsite %>% 
  ggplot(aes(x = easting, y = northing, col = location)) +
  geom_point()

#+ Predator Abundance Data -----
predators <- colnames(dfraw)[98:113]

dfpred <- dfraw %>%
  # get the predator variables and row
  select(row, fish:gammarus) %>% 
  # if is a character only extract numbers
  mutate_if(is.character, parse_number)

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
         waterboard = waterboard_water_level_below_la_na_m,
         # length = approx_length_m, # Why is this such a long name.....
         exposure = exposure_1_sun_3_shade_2_partial_shade_4_open_but_emergent_vegetation_within_rhyne_5_open_but_shaded_by_very_high_bank_or_dense_overhanging_bank_vegetation,
         management = water_tier_management_x,
         shaded = percentage_water_shaded,
         edgedepth = av_depth_proximal_edge_cm,
         centredepth = av_depth_centre_cm
  ) %>% 
  mutate(edgedepth = parse_number(edgedepth), # parsenmbers only
         centredepth = parse_number(centredepth),
         shaded = shaded/100, # turn shaded to a percentage
         waterboard = if_else(waterboard == "0.3-0.4", "0.35", waterboard),
         waterboard = if_else(waterboard == "0.2-0.3", "0.25", waterboard),
         waterboard = as.numeric(waterboard)) %>% 
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
         ph = p_h_probe)

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

# Vegetation Data -----

# vertical group names based on steffi's document
vertvar <- c("carex", "equisetum", "glyceria", "typha", "juncus", "yellow", "phragmite")
vertical_cover <- dfraw %>% select(contains(vertvar) & contains("_cover")) %>% colnames()
vertical_height <- dfraw %>% select(contains(vertvar) & contains("height")) %>% colnames()

# emergent group names
emergentvar <- c("parsnip", "plantain", "arrow")
emergent_cover <- dfraw %>% select(contains(emergentvar)& contains("_cover")) %>% colnames()
emergent_height <- dfraw %>% select(contains(emergentvar) & contains("height")) %>% colnames()

# surface group names
surfvar <- c("duckweed", "ivy", "frog", "blanket")
surface_cover <- dfraw %>% select(contains(surfvar) & contains("_cover")) %>% colnames()
surface_height <- dfraw %>% select(contains(surfvar) & contains("height")) %>% colnames()

# proximal
prox <- c(
vertical_proximal <- dfraw %>% select(contains(vertvar) & contains("proximal")) %>% colnames(),
emergent_proximal <- dfraw %>% select(contains(emergentvar) & contains("proximal")) %>% colnames(),
surface_proximal <- dfraw %>% select(contains(surfvar) & contains("proximal")) %>% colnames()
)

# central 
centr <- c(
vertical_central <- dfraw %>% select(contains(vertvar) & contains("central")) %>% colnames(),
emergent_central <- dfraw %>% select(contains(emergentvar) & contains("central")) %>% colnames(),
surface_central <- dfraw %>% select(contains(surfvar) & contains("central")) %>% colnames()
)

# distal
dist <- c(
vertical_distal <- dfraw %>% select(contains(vertvar) & contains("distal")) %>% colnames(),
emergent_distal <- dfraw %>% select(contains(emergentvar) & contains("distal")) %>% colnames(),
surface_distal <- dfraw %>% select(contains(surfvar) & contains("distal")) %>% colnames()
)

# stripping uneeded text from plant_name function convenience
vegrem <- function(x) {
  y = x %>% 
    str_remove("central_cover_") %>% 
    str_remove("proximal_cover_") %>% 
    str_remove("distal_cover_") %>%
    str_remove("central_height_") %>% 
    str_remove("proximal_height_") %>% 
    str_remove("distal_height_") %>% 
    str_remove("_percent") %>% 
    str_remove("_cm")
  return(y)
}

foo <- dfraw %>% 
  select(plot_id_x, date, contains("_cover"), contains("height")) %>%
  mutate_at(.vars = vars(contains("height"), contains("_cover")), ~ as.numeric(.)) %>% 
  pivot_longer(c(-plot_id_x, -date), names_to = "plant") %>% 
  # add functional groups based on plant name
  mutate(functional_group = 
           # case_when for multiple if else conditionals
           case_when(
             # vertical groups
             plant %in% vertical_cover ~ "vertical",
             plant %in% vertical_height ~ "vertical",
             # surface groups
             plant %in% surface_cover ~ "surface",
             plant %in% surface_height ~ "surface",
             # emergent groups
             plant %in% emergent_cover ~ "emergent",
             plant %in% emergent_height ~ "emergent",
           ),
         plant_measurement =
           # height and cover types
           case_when(
             # vertical groups
             plant %in% vertical_cover ~ "cover",
             plant %in% vertical_height ~ "height",
             # surface groups
             plant %in% surface_cover ~ "cover",
             plant %in% surface_height ~ "height",
             # emergent groups
             plant %in% emergent_cover ~ "cover",
             plant %in% emergent_height ~ "height",),
         position = 
           # proximal, central or distal?
           case_when(
             plant %in% prox ~ "proximal",
             plant %in% centr ~ "central",
             plant %in% dist ~ "distal"
           )
  ) %>% 
  # This drop NA gets rid of species we havent selected or care about 
  drop_na(functional_group) %>% 
  # strip plant names
  mutate(plant = vegrem(plant)) 

# join mean height and mean total cover for each functional group per plot
vegdat <- full_join(
  # calculate the total cover, averaged by functional group and plot
  foo %>% group_by(plot_id_x, date, plant, plant_measurement, functional_group, position) %>% 
    summarise(mean = mean(value, na.rm = T)) %>% 
    group_by(plot_id_x, date, functional_group) %>% 
    filter(plant_measurement == "cover") %>% 
    # have to divide by 300 because it's 300% inital value:
    # (100% prox + 100% cent + 100% dist) / 300 gives overall proportion of rhyne covered
    summarise(mean_cover = sum(mean, na.rm = T)/300),
  # calculate the mean height of each functional groupings plants
  foo %>% group_by(plot_id_x, date, plant, plant_measurement, functional_group, position) %>% 
    summarise(mean = mean(value, na.rm = T)) %>% 
    group_by(plot_id_x, date, functional_group) %>% 
    filter(plant_measurement == "height") %>% 
    summarise(mean_height = mean(mean, na.rm = T))
  ) %>% 
  # ungroup so plays nicely elsewhere
  ungroup() %>% 
  # Add total cover column
  group_by(date, plot_id_x) %>% 
  mutate(total_cover = sum(mean_cover, na.rm = T),
         mean_height = mean(mean_height, na.rm = T))

# aggregate and pivot the veg data 
vegdat_aggregated <- left_join(
  # pivot around total cover
vegdat %>% 
  select(-mean_height) %>% 
  pivot_wider(names_from = functional_group, values_from = mean_cover, names_prefix = "cover_"), 
 # pivot for mean height
vegdat %>% 
  select(-total_cover) %>% 
  pivot_wider(names_from = functional_group, values_from = mean_cover, names_prefix = "height_")
)

##################################################
### APPENDED DFRAW WITH FUNCTIONAL VEG DATA ######
##################################################

dfraw <- left_join(dfraw, vegdat_aggregated)

##################################################
##################################################
##################################################

# final prep for vegetation -----

dfveg <- dfraw %>% 
  select(row, colnames(vegdat_aggregated)[c(-1, -2)])

# check that functional groups add to total cover - ALL TRUE!!!
(dfveg %>% select(cover_emergent, cover_surface, cover_vertical) %>% rowSums() == dfveg %>% pull(total_cover)) %>% 
  table()


#+ Bind it together in a wide tidy format -----
dftidy_wide <- bind_cols(dfabun, dfsite, dfstruc, dfpred,
                         dfcoocurr, dfchem, dfseason,
                         # finalllllllyyyyy
                         dfveg) %>% 
  # Check they all line up right
  # dftidy %>% select(starts_with("row")) # Yes
  select(-c("row1", "row2", "row3", "row4", "row5", "row6")) %>% 
  select(-cs_caspius, -cs_morsitans, -cs_cantans)

#+ Arrange into long format -----
dftidy_long <- dftidy_wide %>% 
  pivot_longer(cols = colnames(dfabun)[c(-1, -6, -7, -8)], 
               names_to = "species", 
               values_to = "abundance")

#+ Make an aggregated format with just totals -----
dfaggregated <- dftidy_long %>% 
  select(-species, -abundance) %>% 
  distinct()

#+ Final Checks for Numeric Vars
qhist(dftidy_long)

#+ END - DATA CLEANED -----
save.image("rdat/Processed-Data.RData")









