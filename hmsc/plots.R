library(Hmsc)
library(tidyverse)

# All models have the same name (Oops). Use this to change
load(file = "hmsc/pa_models/PA_SC.RData")
pa_sc <- m
load(file = "hmsc/pa_models/PA_C.RData")
pa_c <- m
load(file = "hmsc/pa_models/PA_S.RData")
pa_s <- m
load(file = "hmsc/pa_models/PA_N.RData")
pa_n <- m


# All models have the same name (Oops). Use this to change
load(file = "hmsc/abundance_models/ABU_SC.RData")
abun_sc <- m
load(file = "hmsc/abundance_models/ABU_C.RData")
abun_c <- m
load(file = "hmsc/abundance_models/ABU_S.RData")
abun_s <- m
load(file = "hmsc/abundance_models/ABU_N.RData")
abun_n <- m

# Remove temp model object
rm(m)


# Create list of list for models
pa <- list(
  "pa_sc" = pa_sc,
  "pa_c" = pa_c,
  "pa_s" = pa_s,
  "pa_n" = pa_n
)

abun <- list(
  "abun_sc" = abun_sc,
  "abun_c" = abun_c,
  "abun_s" = abun_s,
  "abun_n" = abun_n
)

# List of list for loops
models <- list("pa" = pa, "abun" = abun)

# Big list for lapply work
model_list <- append(pa, abun)

# For correlations in the following for loop
library(corrplot)

# Directory Creating for storing plots - If doesn't exist then create it
ifelse(!dir.exists(file.path("hmsc/", "hmscplots")), dir.create(file.path("hmsc/", "hmscplots")), FALSE)

for (i in seq_along(model_list)) {
  plotdirectory <- file.path("hmsc/hmscplots", names(model_list)[[i]])
  
  # Create directory to store plots if isn't present
  if (!dir.exists(file.path(plotdirectory))) {
    print(paste(
      "Creating directory to store plots of model",
      names(model_list)[[i]]
    ))
    dir.create(file.path(plotdirectory))
  } else {
    print("Directory already exists")
  }
  
  # Plot Beta Correlations ----
  post <- getPostEstimate(model_list[[i]], parName = "Beta")
  # Set png params
  png(
    filename = paste0(plotdirectory, "/Beta_", names(model_list)[[i]], ".png"),
    res = 320,
    units = "in",
    width = 7,
    height = 5
  )
  # Plot
  plotBeta(
    model_list[[i]],
    post = post,
    param = "Support",
    spNamesNumbers = c(TRUE, FALSE),
    supportLevel = 0.75
  )
  # Dev.off
  dev.off()
  
  # Does the model have random levels?
  if (length(model_list[[i]]$rL) == 0) {
    print(paste("No random levels to check correlations for in model:", names(model_list)[[i]]))
    
  } else {
    print(paste("Plotting residual correlations in random levels for model:", names(model_list)[[i]]))
    
    # Correlations
    OmegaCor <- computeAssociations(model_list[[i]])
    supportLevel = 0.75
    
    # These are the site level residual correlations
    toplotsite <-
      ((OmegaCor[[1]]$support > supportLevel) +
         (OmegaCor[[1]]$support < (1 - supportLevel)) > 0) * OmegaCor[[1]]$mean
    # Device settings for PNG
    png(
      filename = paste0(plotdirectory, "/SiteCor_", names(model_list)[[i]], ".png"),
      res = 320,
      units = "in",
      width = 7,
      height = 5
    )
    # Plot them
    corrplot(toplotsite,
             method = "color",
             typ = "lower",
             title = "Site Level Residual Correlations")
    # Devices off
    dev.off()
    
    # These are the rhyne level residual correlations
    toplotrhyne <-
      ((OmegaCor[[2]]$support > supportLevel) +
         (OmegaCor[[2]]$support < (1 - supportLevel)) > 0) * OmegaCor[[2]]$mean
    # Device settings for PNG
    png(
      filename = paste0(plotdirectory, "/RhyneCor_", names(model_list)[[i]], ".png"),
      res = 320,
      units = "in",
      width = 7,
      height = 5
    )
    # Plot
    corrplot(toplotrhyne,
             method = "color",
             typ = "lower",
             title = "Site Level Residual Correlations")
    
    # Devices off
    dev.off()
  }
}


# More plotting libraries
library(ggridges)

# Directory Creating for storing plots - If doesn't exist then create it
ifelse(!dir.exists(file.path("hmsc/hmscplots", "comparison")), dir.create(file.path("hmsc/hmscplots", "comparison")), FALSE)

comparisondir <- file.path("hmsc/hmscplots/comparison/")

# Effective samples comparison - Beta
effective_samples_Beta <- lapply(model_list, function(x) {
  mpost <- convertToCodaObject(x)
  es.beta <- effectiveSize(mpost$Beta)
  return(es.beta)
})

# Plot - Beta
bind_cols(effective_samples_Beta) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = name, y = value)) +
  geom_boxplot() +
  xlab("Model Type") +
  ylab("Effective Sample Size")
theme_minimal()

ggsave(paste0(comparisondir, "/ess_Beta.png"), width = 7, height = 5, dpi = 300)

# Effective samples comparison - V
effective_samples_V <- lapply(model_list, function(x) {
  mpost <- convertToCodaObject(x)
  es.V <- effectiveSize(mpost$V)
  return(es.V)
})

# Plot - V
bind_cols(effective_samples_V) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = name, y = value)) +
  geom_boxplot() +
  xlab("Model Type") +
  ylab("Effective Sample Size")
theme_minimal()

ggsave(paste0(comparisondir, "/ess_V.png"), width = 7, height = 5, dpi = 300)

# Effective samples comparison - Gamma
effective_samples_Gamma <- lapply(model_list, function(x) {
  mpost <- convertToCodaObject(x)
  es.Gamma <- effectiveSize(mpost$Gamma)
  return(es.Gamma)
})

# Plot - Gamma
bind_cols(effective_samples_Gamma) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = name, y = value)) +
  geom_boxplot() +
  xlab("Model Type") +
  ylab("Effective Sample Size")
theme_minimal()

ggsave(paste0(comparisondir, "/ess_Gamma.png"), width = 7, height = 5, dpi = 300)

# Model mixing and convergence
convergence <- lapply(model_list, function(x) {
  mpost <- convertToCodaObject(x)
  ge.beta <- gelman.diag(mpost$Beta, multivariate = FALSE)$psrf
  return(ge.beta)
})

# Plot - mixing
map(convergence, as.vector) %>%
  bind_rows(convergence) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = value, y = name, fill = name)) +
  geom_density_ridges() +
  scale_fill_viridis_d() +
  geom_vline(col = "red", xintercept = 1.01) +
  ylab("Model Type") +
  xlab("R Hat value of mixing chains") +
  xlim(0.99, 1.1) +
  theme_minimal()

ggsave(paste0(comparisondir, "/convergence.png"), width = 7, height = 5, dpi = 300)

