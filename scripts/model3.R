source("scripts/cleaning-data.R")

# Morsitans, Cantans, Caspius are very rare species - omit
rare_species <- c("morsitans", "cantans", "caspius")
df <- tidydata %>% filter(!species %in% rare_species)


library(brms)

# Question 2.
# How do the environmental factors influence total abundance?

f1.2 <- paste("total ~ management +",
paste(colnames(veg), collapse = " + "),
"+ (1|site/plot)")

p1.2 <- prior(exponential())

m1.2 <- brm(
formula = f1.2, data = df,
family = poisson(),
prior = p1.2,
cores = 4, chains = 4,
iter = 200
)

ce1.2 <- conditional_effects(m1.2)
plot(ce1.2)
