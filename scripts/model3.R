source("scripts/cleaning-data.R")

# Morsitans, Cantans, Caspius are very rare species - omit
rare_species <- c("morsitans", "cantans", "caspius")
df <- tidydata %>% filter(!species %in% rare_species)


library(brms)

# Question 1.1
# How do the environmental factors influence total abundance for each species?

f1.1 <- paste("abundance ~ 1 + species:",
              paste(colnames(veg), collapse = " + species:"),
              "+ (1|site/plot)")

# p1.1 <- prior(exponential(1), class = b)

m1.1 <- brm(
  formula = f1.1, data = df,
  family = poisson(),
  #prior = prior(exponential(1), coef = ),
  cores = 4, chains = 4,
  iter = 2000
)

plot(conditional_effects(m1.1))

# Question 1.2
# How does the water chemistry influence total abundance by species?

f1.2 <- paste0("abundance ~ 1 + species:",
               paste(colnames(wchem), collapse = " + species:"),
               "+ (1|site/plot)")

df1.2 <- df %>% mutate_at(.vars = colnames(wchem), scale)

m1.2 <- brm(data = df1.2, family = poisson(),
            f1.2,
            prior(normal(0, .5)),
            cores = 4, chains = 4,
            iter = 2000
)

conditional_effects(m1.2)

# Question 1.3
# How does the water chemistry influence total abundance by species?

f1.3 <- bf(abundance ~ 1 + width:species + depth:species + shaded:species + (1|site/plot))

df1.3 <- df %>% mutate(#rhyne_dry = rhyne_dry %>% as.factor,
                       #rhyne_cleared = rhyne_cleared %>% as.factor,
                       width = scale(width),
                       depth = scale(depth),
                       shaded = scale(shaded))

m1.3 <- brm(data = df1.3, family = poisson(),
            f1.3,
            prior(normal(0, .5)),
            cores = 4, chains = 4,
            iter = 2000
)

conditional_effects(m1.3)

