library(tidyverse)

rawdat <- read_csv("boral_data.csv")
rawdat$uid = 1:nrow(rawdat)


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


# location ----------------------------------------------------------------

loc <- rawdat %>% 
  # select 
  select(uid,
         site,
         plot,
         year,
         season,
         location, 
         management,
         dip_point) %>%
  # factor them
  mutate(site = site %>% as.factor,
         plot = plot %>% as.factor,
         year = year %>% as.factor,
         season = season %>% as.factor,
         dip_point = dip_point %>% as.factor,
         management = if_else(management == 1, "Tier1", "Tier3") %>% as.factor)


# abundance ---------------------------------------------------------------

abun <- rawdat %>% 
  select(
    total = totals_n_larvae,
    pipiens = totals_n_cx_pipiens,
    maculipennis = totals_n_an_maculipennis,
    claviger = totals_n_an_claviger,
    annulata = totals_n_cs_annulata,
    morsitans = totals_n_cs_morsitans,
    cantans = totals_n_oc_annulipes_cantans,
    caspius = totals_n_oc_caspius
  )

qdot(abun)


# water chemistry ---------------------------------------------------------

wchem <- rawdat %>% 
  select(
    turbidity,
    salinity,
    water_temp = water_temperature_c,
    dissolved_oxygen,
    ph = p_h_probe
  ) %>% 
  mutate_all(as.numeric)

# some outliers
qdot(wchem)

wchem <- # fixed outliers
  wchem %>% mutate(
    turbidity = ifelse(turbidity > 3, NA, turbidity),
    water_temp = ifelse(water_temp > 40, NA, water_temp),
    ph = ifelse(ph < 0, NA, ph),
    dissolved_oxygen = ifelse(dissolved_oxygen < 50, NA, dissolved_oxygen)
  ) 


# structure ---------------------------------------------------------------

struc <- rawdat %>% 
  select(
    rhyne_dry,
    rhyne_cleared,
    exposure,
    landcover,
    width = average_width_m,
    depth = av_depth_centre_cm,
    shaded = percentage_water_shaded
  ) %>% 
  mutate_at(vars(-landcover), as.numeric) %>% 
  mutate(exposure = exposure %>% as.character,
         shaded = ifelse(shaded > 100, NA, shaded/100))

qdot(struc)

# vegetation --------------------------------------------------------------

vegrem <- function(x) {
  y = str_remove(x, "central_")
  z = str_remove(y, "_percent")
  return(z)
}

veg <- rawdat %>% 
  select(starts_with("central_cover")) %>% 
  rename_all(vegrem) %>% 
  mutate_all(~ ./100)


# bind and pivot ----------------------------------------------------------

data <- bind_cols(loc, abun, struc, wchem, veg) 

tidydata <- 
  data %>% 
  pivot_longer(cols = colnames(abun),
               names_to = "species",
               values_to = "abundance")



