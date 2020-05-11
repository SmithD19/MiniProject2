## Aggregating data to the plot level - getting rid of rhyne data
load("masterdf2.RDS")
library(tidyverse)
library(gllvm)

# Only take the year 2010. It is the only year with a full set of values for each season
df <- dftidy_wide %>% filter(year == "2010")

# Drop any values that are NA for species abundances 
df <- df %>% drop_na(total)

# Species variable names
mosquito_names <- colnames(dftidy_wide)[2:5]
pred_names <- df %>% select(fish:gammarus) %>% colnames()

# Binary co-occurrence variable names
env_names <- df %>% select(buttercup:ladies_frock) %>% colnames()

# Environmental co-variates - just use width and shaded for now
# unsure if categorical variables work with this package
env2_names <- df %>% select(width, shaded, wtemp:ph, total_cover) %>% colnames()


# Aggregation -------------------------------------------------------------

# Mosquito and Env data manipulation ----
# Here are the abundances and covariates for site and plot
aggregated1 <- df %>% 
  # Group by site and plot level
  group_by(site, plot, season) %>% 
  # Sum the abundance values for each site and plot
  summarise(
    # Firstly sum the abundances of the species
    cx_pipiens = sum(cx_pipiens, na.rm = T),
    an_maculipennis = sum(an_maculipennis, na.rm = T),
    an_claviger = sum(an_claviger, na.rm = T),
    cs_annulata = sum(cs_annulata, na.rm = T),
    # Secondly average the environmental variables
    width = mean(width, na.rm = T),
    shaded = mean(shaded, na.rm = T),
    wtemp = mean(wtemp, na.rm = T),
    turbidity = mean(turbidity, na.rm = T),
    salinity = mean(salinity, na.rm = T),
    do = mean(do, na.rm = T),
    ph = mean(ph, na.rm = T),
    total_cover = mean(total_cover, na.rm = T)
  ) %>% 
  # Ungroup to prevent dplyr wizardy 
  ungroup() %>% 
  # Replace NaN with 0 - probably trying to find a mean of 0
  replace(is.na(.), 0)

# Predator abundances for site and plot ------
aggregated1.1 <- df %>% 
  # Group by site/plot/season
  group_by(site, plot, season) %>%
  # Sum for eac
  summarise_at(
    .vars = vars(pred_names),
    .funs = sum, na.rm = T) %>% 
  # Ungrouping helps stop dplyr messing up
  ungroup()

# Binary occurance data -----
# Use this function to summarise the binary variables and then back to binary occurance
binaryaggregate <- function(x){ifelse(sum(x, na.rm = T) > 0, 1, 0)}
# Here are the species co-occurrances for site and plot
aggregated2 <- df %>% 
  # Get the binary coocurence values for each site and plot
  mutate_at(vars(env_names), as.character) %>% 
  mutate_at(vars(env_names), as.numeric) %>% 
  # Grouping vars
  group_by(site, plot, season) %>%
  # Summarise the bianry vars
  summarise_at(
    .vars = vars(env_names),
    .funs = binaryaggregate
  ) %>% 
# Ungroup to stop dplyr magic
ungroup() 

# Which values have no variation so we can remove them? -----
remove_me_cooccurr <- aggregated2 %>% 
  # 0 values have no varaition
  summarise_if(is.numeric, sum) %>%
  # Pivot to gather names
  pivot_longer(everything()) %>% 
  # Filter names and select
  filter(value == 0) %>% 
  # Get vector of names
  pull(name)
# Deselect vars with no variation?
aggregated2 <- aggregated2 %>% select(-remove_me_cooccurr)
# edit env2_names
env_names <- colnames(aggregated2[4:15])

# Now do the same for predator abundances
remove_me_pred <- aggregated1.1 %>% 
  # 0 values have no varaition
  summarise_if(is.numeric, sum) %>%
  # Pivot to gather names
  pivot_longer(everything()) %>% 
  # Filter names and select
  filter(value == 0) %>% 
  # Get vector of names
  pull(name)
# Deslect preds with no variation
aggregated1.1 <- aggregated1.1 %>% select(-remove_me_pred)
# Modify pred_names
pred_names <- colnames(aggregated1.1[4:NCOL(aggregated1.1)])


# Join the two summaries together -----
aggregated <- left_join(aggregated1, aggregated1.1, by = c("site", "plot", "season"))
# Pull the abundance variables for mosquitoes
abun <- aggregated %>% select(mosquito_names, pred_names)
# Pull covariates
env <- aggregated %>% select(env2_names)


# glllvm workflow ---------------------------------------------------------

# Try all models and choose lowest AICc:
agg_poi <- gllvm(y = abun, family = poisson()) # AICc 8171
agg_nb <- gllvm(y = abun, family = "negative.binomial") #3 AICc 6502
agg_zip <- gllvm(y = abun, family = "ZIP") # AICc 6800

# NegBinomial has lowest AICc (absolute value)

# Now we check the residual plots:
par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))
plot(agg_poi) # Overdispersion and increased variance

par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))
plot(agg_nb) # Pretty good

par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))
plot(agg_zip) # Increased variance

# Reset plotting
par(mfrow = c(1,1))

# After checking these it looks like using negative.binomial is the best way forward
# What about ordination plots? I'm unsure where to go from here after these plots...
ordiplot(agg_poi, biplot = TRUE)
ordiplot(agg_nb, biplot = TRUE)
ordiplot(agg_zip, biplot = TRUE)

# Despite this. Continue using NegBinom distribution
# Try with environmental covariates - and decide how many latent variables are appropriate 

criterias <- vector()
for(i in 0:5){
  fiti <- gllvm(y = abun, X = env , family = "negative.binomial", num.lv = i, sd.errors = FALSE,
                formula = ~ width+shaded+wtemp+turbidity+salinity+do+ph+total_cover, seed = 1234)
  print(paste("Latent Var", i, "Finished"))
  criterias[i + 1] <- summary(fiti)$AICc
  names(criterias)[i + 1] = i
}

# Which one has lowest absolute AICc?
abs(criterias) # 1 latent variable

# Carry on using four latent variables
fit_agg <- gllvm(y = abun, X = env , family = "negative.binomial", sd.errors = FALSE, num.lv = 1,
                 formula = ~ , seed = 1234)

# Reset plotting
par(mfrow = c(1,1))

# ordinaiton plot
ordiplot(fit_agg, biplot = T)

# summary plot
plot(fit_agg)

# correlation plot
corrplot::corrplot(gllvm::getResidualCor(fit_agg), diag = FALSE, type = "lower", method = "square", tl.cex = 0.8, tl.srt = 45, tl.col = "red")

# Returning a coefficient plot just gives a basic R error...
# This is apparent in gllvm 1.2.1 and 1.2.2 installed though GitHub and CRAN
coefplot.gllvm(fit_agg)
# Will potentially have to email project author. Isnt working and needs a gllvm object to run





