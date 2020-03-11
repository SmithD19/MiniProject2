library(brms)
library(tidybayes)
library(tidyverse)
library(bayesplot)

data_raw <- read_csv("boral_data.csv")

data <- 
  data_raw %>% 
  mutate(site = site %>% as.factor,
         plot = plot %>% as.factor,
         plot_id = plot_id %>% as.factor,
         year = year %>% as.factor,
         season = season %>% as.factor,
         management = if_else(management == 1, "Tier1", "Tier3") %>% as.factor,
         # get rid of characters in shaded and convert to percentage
         shaded = str_extract(data_raw$percentage_water_shaded, "\\d+") %>% as.numeric,
         ph = str_extract(data_raw$p_h_probe, "\\d+") %>% as.numeric,
         waterboard = str_extract(data_raw$waterboard_water_level_below_land_m, "\\d+") %>% as.numeric,
         water_temp = str_extract(data_raw$water_temperature_c, "\\d+") %>% as.numeric,
         # one of the shaded values was over 100% -> replace with 0 for safety
         shaded_perc = if_else(shaded <= 100, (shaded/100), 0),
         duckweed_cover = central_cover_duckweed_percent/100,
         carex_cover = central_cover_carex_percent/100,
         juncus_cover = central_cover_juncus_percent/100) %>% 
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


# simple model ------------------------------------------------------------

simple <- bf(total_larvae ~ 1 + management)

model_simple <- brm(formula = simple,
                    family = poisson(),
                    data = data,
                    cores = 4)

#shinystan::launch_shinystan(model_simple)

conditional_effects(model_simple)

posterior_summary(model_simple, robust = T, probs = c(.025, .25, .75, .975))

post <- posterior_samples(model_simple)

post %>% 
  ggplot(aes(x = b_Intercept)) +
  geom_histogram(size = .2, bins = 40) +
  stat_pointintervalh(aes(y = 0), 
                      point_interval = mode_hdi, .width = .95) +
  labs(title = "Theta",
       x = expression(theta))


# Simple hierarchical -----------------------------------------------------

simple_h <- bf(total_larvae ~ 1 + management + shaded_perc + plot + (1 | site))

model_h <- brm(formula = simple_h,
               family = poisson(),
               data = data,
               cores = 4,
               control = list(adapt_delta = 0.99))

conditional_effects(model_h)

#shinystan::launch_shinystan(model_h)

# Ahhhhhhhhhhhhhh ---------------------------------------------------------

# generic model - zero inflated cause of many many zero recordings for abundance
model1 <- bf(total_larvae ~ 1 + management)
fit1 <- brm(formula = model1,
            family = zero_inflated_poisson(),
            data = data,
            cores = 4)
conditional_effects(fit1)


# strctural and management?
model2 <- brm(formula = bf(total_larvae ~ 1 + management + shaded_perc),              
              family = zero_inflated_poisson(),
              data = data,
              cores = 4,
              control = list(adapt_delta = 0.9))
conditional_effects(model2) 

# site group level? - divergent chains nonsense output - also tried to do just one species here...
# isnt the response variable for a set of species in the end though so: 
# total_larvae | species ~ model, model, model ?
model3 <- brm(formula = bf(total_larvae ~ 1 + management + shaded_perc + (1 | site)),              
              family = zero_inflated_poisson(),
              data = data,
              cores = 4,
              # divergent chains needed better adapt rate
              control = list(adapt_delta = 0.9))
conditional_effects(model3)

# what about exposure? lets try most of the structural properties
model4 <- brm(formula = 
                # unsure if I transform the total_larvae into log format first like a normal model?
                bf(total_larvae ~ 1 + management + shaded_perc + exposure + landcover),
              # unsure of what priors to set?
              prior = set_prior("student_t(3,0,10)"),
              family = zero_inflated_poisson(),
              data = data,
              cores = 4,
              control = list(adapt_delta = 0.9))
conditional_effects(model4)

# what about some co-coccuring plants?
model5 <- brm(formula = 
                # unsure if I transform the total_larvae into log format first like a normal model?
                bf(total_larvae ~ 1 + management + duckweed_cover + juncus_cover + carex_cover),
              # unsure of what priors to set?
              prior = set_prior("student_t(3,0,10)"),
              family = zero_inflated_poisson(),
              data = data,
              cores = 4,
              control = list(adapt_delta = 0.9))
conditional_effects(model5)

# binary models? ----------------------------------------------------------
model_test <- brm(data = data, family = poisson(),
                  total_larvae ~ 1 + management,
                  chains = 4, cores = 4)
conditional_effects(model_test)

model_test <- add_criterion(model_test, criterion = "waic")



model_test2 <- brm(data = data, family = zero_inflated_poisson(),
                   total_larvae ~ 1 + management,
                   chains = 4, cores = 4)
conditional_effects(model_test2)

model_test2 <- add_criterion(model_test2, criterion = "waic")

loo_compare(model_test, model_test2, criterion = "waic")


# another useless test ----------------------------------------------------

data %>% 
  ggplot(aes(x = shaded_perc, y = cs.annulata)) +
  geom_smooth() +
  geom_point() 

data %>% 
  ggplot(aes(x = plot, y = sqrt(total_larvae), col = management)) +
  geom_boxplot() +
  facet_wrap(~ site)

data %>% 
  ggplot(aes(x = management, y = water_temp)) +
  geom_boxplot() +
  geom_point()

m1 <- brm(data = data, family = zero_inflated_poisson(),
          # model
          total_larvae ~ 1 + water_temp,
          # priors
          prior = set_prior("normal(35,1)"),
          # settings
          cores = 4, chains = 4)

 plot(m1)

posterior_summary(m1)

conditional_effects(m1)

mcmc_pairs(m1)






