library(tidyverse)
library(brms)
# library(rethinking)

# df <- read_csv("data/tidydata.csv")
# df <- df %>% mutate(sabun = scale(abundance),
#                     labun = sqrt(abundance))

df = tidydata

#+ how does species vary via management?
m1.1 <- brm(
  abundance ~ 1 + species:management,
  family = poisson(),
  data = df,
  cores = 4
)

plot(conditional_effects(m1.1))

#+
m1.2 <- brm(
  abundance ~ 1 + ph,
  prior = set_prior("normal(6.5, .5)"),
  data = df,
  cores = 4
)

plot(conditional_effects(m1.2))

#+
m1.3 <- brm(
  bf(abundance ~ 1 + ph + dissolved_oxygen + water_temp),
  prior = c(prior(normal(6.5, .5), coef = ph),
            prior(normal(140, 0.5), coef = dissolved_oxygen),
            prior(normal(17, 0.5), coef = water_temp)
  ),
  family = poisson(),
  data = df,
  cores = 4
)

plot(conditional_effects(m1.3))

#+
m1.4 <- brm(
  bf(abundance ~ 1 + ph + dissolved_oxygen + water_temp + (1|site/plot)),
  prior = c(prior(normal(6.5, .5), coef = ph),
            prior(normal(140, 0.5), coef = dissolved_oxygen),
            prior(normal(17, 0.5), coef = water_temp)
  ),
  family = poisson(),
  data = df,
  cores = 4
)

plot(conditional_effects(m1.4))

#+ 
m1.5 <- brm(
  abundance ~ 1 + management * shaded + (1|species),
  family = poisson(),
  data = df,
  cores = 4
)

plot(conditional_effects(m1.5))


# standardize some vars ---------------------------------------------------

struc <- df %>% 
  select(rhyne_dry,
         rhyne_cleared,
         exposure,
         landcover,
         shaded,
         width,
         depth)

struc %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()



#+
p1.6 = c(prior(normal(50, 0.5), coef = depth),
         prior(normal(2.3, 0.2), coef = width)
         )

m1.6 <- brm(
  bf(abundance ~ 1 + exposure + landcover + depth + shaded + width + (1|site/plot)),
  family = zero_inflated_poisson,
  prior = p1.6,
  data = df,
  cores = 4
)

plot(conditional_effects(m1.6))

#+
f1.7 = paste("abundance ~", paste(colnames(wchem), collapse = "+"), " + (1|site/plot)")

p1.7 = 

m1.7 <- brm(
  bf(f1.7),
  family = zero_inflated_poisson,
  # prior = p1.6,
  data = df,
  cores = 4
)

plot(conditional_effects(m1.7))

#+ 
m1.8 <- brm(
    bf(depth ~ management * season + (1|plot)),
    family = gaussian(),
    # prior = p1.6,
    data = df,
    cores = 4
  )

conditional_effects(m1.8)

#+ 
m1.9 <- brm(
  bf(depth ~ management * season + (1|site)),
  family = gaussian(),
  # prior = p1.6,
  data = df,
  cores = 4
)

conditional_effects(m1.9)

#+ non-varying intercept
m2.1 <- brm(
  bf(abundance ~ species:shaded),
  family = poisson(),
  data = df,
  cores = 4
)

conditional_effects(m2.1)

#+ varying intercept 
m2.2 <- brm(
  bf(abundance ~ 1 + species:shaded),
  family = poisson(),
  data = df,
  cores = 4
)

conditional_effects(m2.2)

#+ varying intercept 
m2.3 <- brm(
  bf(abundance ~ 1 + species:depth),
  family = gaussian(),
  prior = prior(normal(0,.2)),
  data = df %>% mutate(depth = scale(depth),
                       abundance = scale(abundance)),
  cores = 4
)

conditional_effects(m2.3)

