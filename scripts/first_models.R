library(brms)
library(tidybayes)
library(tidyverse)

data_raw <- read_csv("boral_data.csv")

data <- 
  data_raw %>% 
  mutate(site = site %>% as.factor,
         plot = plot %>% as.factor,
         year = year %>% as.factor,
         season = season %>% as.factor,
         management = if_else(management == 1, "Tier1", "Tier3") %>% as.factor,
         # get rid of characters in shaded and convert to percentage
         shaded = str_extract(data_raw$percentage_water_shaded, "\\d+") %>% as.numeric,
         shaded_perc = shaded/100,
         duckweed_cover = central_cover_duckweed_percent) %>% 
  
  rename(
    total_larvae = totals_n_larvae,
    cx.pipiens = totals_n_cx_pipiens,
    an.maculipennis = totals_n_an_maculipennis,
    an.claviger = totals_n_an_claviger,
    cs.annulata = totals_n_cs_annulata,
    cs.morsitans = totals_n_cs_morsitans,
    oc.cantans = totals_n_oc_annulipes_cantans,
    oc.caspius = totals_n_oc_caspius
  ) 

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

simple_h <- bf(total_larvae ~ 1 + management + (1 | site) + (1 | season) + (1 | year))

model_h <- brm(formula = simple_h,
               family = poisson(),
               data = data,
               cores = 4,
               control = list(adapt_delta = 0.99))

conditional_effects(model_h)

shinystan::launch_shinystan(model_h)

# Ahhhhhhhhhhhhhh ---------------------------------------------------------

# generic model - zero inflated cause of many many zero recordings for abundance
model1 <- bf(total_larvae ~ 1 + management)
fit1 <- brm(formula = model1,
            family = zero_inflated_poisson(),
            data = data,
            cores = 4)
conditional_effects(fit1)


# strctural and management?
model2 <- brm(formula = bf(cx.pipiens ~ 1 + management + shaded_perc + (1 | site)),              
              family = zero_inflated_poisson(),
              data = data,
              cores = 4,
              control = list(adapt_delta = 0.9))
conditional_effects(model2)

# site group level?
model3 <- brm(formula = bf(cx.pipiens ~ 1 + management + shaded_perc + (1 | site)),              
              family = zero_inflated_poisson(),
              data = data,
              cores = 4,
              control = list(adapt_delta = 0.9))
conditional_effects(model3)


