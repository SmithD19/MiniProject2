# Daniel Smith
# dansmi@ceh.ac,uk
# smithd19@github

## Variable selection using projpred and spares horshoe priors


# Load 01-proccessing -----------------------------------------------------
load("rdat/01-processing.RData")

# ProjPred ----------------------------------------------------------------
library(brms)
library(projpred)
library(bayesplot)
library(tidybayes)
library(tidyverse)

# WAIC --------------------------------------------------------------------
dftidy_long %>% skimr::skim()

dftidy_long %>% 
  ggplot(aes(x = shaded, y = abundance, col = management)) +
  geom_smooth()

# 1.1 management and abundance
f1.1 <- bf(abundance ~ management*species + (1|plot/site))

get_prior(f1.1, data = dftidy_long)

m1.1 <- brm(data = dftidy_long, family = negbinomial(),
            f1.1,
            #prior = prior(normal(0,10), class = "b"),
            cores = 4, iter = 1000)

conditional_effects(m1.1)
mcmc_plot(m1.1)

# 1.2 Do + WTemp & Turbidty + Salinity correlated as expected 
dftidy_long %>% select_if(is.numeric) %>% GGally::ggpairs()

f1.2 <- bf(abundance ~ wtemp + do + salinity + turbidity)

get_prior(f1.2, data = dftidy_long)

p1.2 <- c(
  prior(normal(.5, 100), class = "b", coef = "do"),
  prior(normal(16, 5), class = "b", coef = "wtemp"),
  prior(normal(.5, 1), class = "b", coef = "salinity"),
  prior(normal(.5, 1), class = "b", coef = "turbidity")
)

m1.2 <- brm(data = dftidy_long, family = negbinomial(),
            f1.2,
            prior = p1.2,
            cores = 4, iter = 1000)

conditional_effects(m1.2)

pp_check(m1.2)

