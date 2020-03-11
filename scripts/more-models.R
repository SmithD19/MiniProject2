library(brms)
library(tidybayes)
library(tidyverse)
library(bayesplot)

data_raw <- read_csv("boral_data.csv")

# binary co-occuring plants
cooccur_plants <- 
  # give in vector format - easy to manipulate 
  c(
    "buttercup", "grass", "clover", "carex", "juncus", 
    "marsh_marigold", "meadow_sweet", "nettles", 
    "thistle", "water_mint", "dandelion", "birch", 
    "alder", "dog_rose", "hawthorn", "oak", "sallow", "willow"
  )

# cleaning

data <-
  data_raw %>% 
  mutate(site = site %>% as.factor,
         plot = plot %>% as.factor,
         plot_id = plot_id %>% as.factor,
         year = year %>% as.factor,
         season = season %>% as.factor,
         location = location %>% as.factor,
         management = if_else(management == 1, "Tier1", "Tier3") %>% as.factor,
         
         # get rid of characters in shaded and convert to percentage - this way is flawed
         # shaded = str_extract(data_raw$percentage_water_shaded, "\\d+") %>% as.numeric,
         # ph = str_extract(data_raw$p_h_probe, "\\d+") %>% as.numeric,
         # waterboard = str_extract(data_raw$waterboard_water_level_below_land_m, "\\d+") %>% as.numeric,
         # water_temp = str_extract(data_raw$water_temperature_c, "\\d+") %>% as.numeric,
         # turbidity = str_extract(data_raw$turbidity, "\\d+") %>% as.numeric,
         # centre_depth= str_extract(data_raw$av_depth_centre_cm, "\\d+") %>% as.numeric,
         
         # instead - replace letters with nothing?
         centre_depth = str_replace_all(data_raw$av_depth_centre_cm, "[:alpha:]", "") %>% as.numeric,
         water_temp = str_replace_all(data_raw$water_temperature_c, "[:alpha:]", "") %>% as.numeric,
         ph = str_replace_all(data_raw$p_h_probe, "[:alpha:]", "") %>% as.numeric,
         shaded = str_replace_all(data_raw$percentage_water_shaded, "[:alpha:]", "") %>% as.numeric,
         turbidity = str_replace_all(data_raw$turbidity, "[:alpha:]", "") %>% as.numeric,
         salinity = str_replace_all(data_raw$salinity, "[:alpha:]", "") %>% as.numeric,
         
         # one of the shaded values was over 100% -> replace with 0 for safety
         shaded_perc = if_else(shaded <= 100, (shaded/100), 0),
         duckweed_cover = central_cover_duckweed_percent/100,
         carex_cover = central_cover_carex_percent/100,
         juncus_cover = central_cover_juncus_percent/100,
         frog_cover = central_cover_frog_bit_percent/100,
         grass_cover = central_cover_grass_percent/100,
         ivy_cover = central_cover_ivy_leaved_duckweed_percent/100,
  ) %>%
  # mutate binary data_raw to factors
  mutate_at(.vars = cooccur_plants, as.factor) %>% 
  # rename these things
  rename(
    total_larvae = totals_n_larvae,
    cx.pipiens = totals_n_cx_pipiens,
    an.maculipennis = totals_n_an_maculipennis,
    an.claviger = totals_n_an_claviger,
    cs.annulata = totals_n_cs_annulata,
    cs.morsitans = totals_n_cs_morsitans,
    oc.cantans = totals_n_oc_annulipes_cantans,
    oc.caspius = totals_n_oc_caspius
  ) %>% 
  # create binary presence values
  mutate(
    pipiens = if_else(cx.pipiens < 0, 1, 0),
    maculipennis = if_else(an.maculipennis < 0, 1, 0),
    claviger = if_else(an.claviger < 0, 1, 0),
    annulata = if_else(cs.annulata < 0, 1, 0),
    morsitans = if_else(cs.morsitans < 0, 1, 0),
    cantans = if_else(oc.cantans < 0, 1, 0),
    caspius = if_else(oc.caspius < 0, 1, 0),
  )

# vis check
data %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_density()

paste(cooccur_plants[1:2], collapse = " + ")

# f1 <- paste("total_larvae ~", paste(cooccur_plants, collapse = " + "))

m1 <- brm(formula = 
            total_larvae ~ management + ph + water_temp + shaded_perc + turbidity + salinity + (1 | site/plot), 
          data = data,
          family = poisson,
          # prior = set_prior("normal(6,0.5)", class = "b"),
          cores = 4, chains = 4,
          # settings
          control = list(adapt_delta = 0.9),
          # for testing use these simple parameters
          iter = 1000)

conditional_effects(m1)


f2 <- paste("total_larvae ~ 1 +",
            paste(data %>% select(starts_with("central_cover")) %>% colnames(), collapse = " + "),
            " + (1 | site/plot)")

m2 <- brm(formula = f2, 
          data = data,
          family = zero_inflated_poisson(),
          prior = set_prior("normal(0.5 ,1)", class = "b"),
          cores = 4, chains = 4,
          # settings
          control = list(adapt_delta = 0.9),
          # for testing use these simple parameters
          iter = 1000)

plot(conditional_effects(m1), points = TRUE)

prior("normal(0.5, 1)")

m3 <- brm(
  total_larvae ~ 1 + landcover*exposure,
  data = data,
  family = poisson(),
  prior = set_prior("normal(0.5, 1)"),
  cores = 4,
  chains = 4,
  iter = 1000
)

plot(conditional_effects(m3), points = TRUE)
