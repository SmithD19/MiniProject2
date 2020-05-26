library(Hmsc)
library(tidyverse)

# All models have the same name (Oops). Use this to change
load(file = "hmsc/pa2_models/PA2_Full_PredPred.RData")
PA_PredX <- m
load(file = "hmsc/pa2_models/PA2_Full_PredResp.RData")
PA_PredY <- m
# All models have the same name (Oops). Use this to change
load(file = "hmsc/abu2_models/ABU2_Full_PredPred.RData")
ABU_PredX <- m
load(file = "hmsc/abu2_models/ABU2_Full_PredResp.RData")
ABU_PredY <- m


# Remove temp model object
rm(m)

# List of list for loops
model_list <- list("PA_PredX" = PA_PredX, "PA_PredY" = PA_PredY,
                   "ABU_PredX" = ABU_PredX, "ABU_PredY" = ABU_PredY)


# For correlations in the following for loop
library(ggcorrplot)

# Directory Creating for storing plots - If doesn't exist then create it
ifelse(!dir.exists(file.path("hmsc/", "hmscplots2")), dir.create(file.path("hmsc/", "hmscplots2")), FALSE)

for (i in seq_along(model_list)) {
  plotdirectory <- file.path("hmsc/hmscplots2", names(model_list)[[i]])
  
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
    width = 10,
    height = 10
  )
  # Plot
  plotBeta(
    model_list[[i]],
    post = post,
    param = "Support",
    spNamesNumbers = c(TRUE, FALSE),
    supportLevel = 0.95,
    split = .3
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
    supportLevel = 0.95
    
    # These are the site level residual correlations
    toplotsite <-
      ((OmegaCor[[1]]$support > supportLevel) +
         (OmegaCor[[1]]$support < (1 - supportLevel)) > 0) * OmegaCor[[1]]$mean

    
    # ggcorrplot
    ggcorrplot(toplotsite,
               method = "square",
               typ = "lower",
               title = "Site Level Residual Correlations")
    ggsave(filename = paste0(plotdirectory, "/SiteCor_", names(model_list)[[i]], ".png"))
    
    
    # These are the rhyne level residual correlations
    toplotrhyne <-
      ((OmegaCor[[2]]$support > supportLevel) +
         (OmegaCor[[2]]$support < (1 - supportLevel)) > 0) * OmegaCor[[2]]$mean

    # Plot
    ggcorrplot(toplotrhyne,
               method = "square",
               typ = "lower",
               title = "Rhyne Level Residual Correlations")
    ggsave(filename = paste0(plotdirectory, "/RhyneCor_", names(model_list)[[i]], ".png"))
  }
}


# More plotting libraries
library(ggridges)

# Because they have different response matrix sizes we need a function to combine
# uneven vectors:

# A function to bind a list of uneven vectors together into a data frame
bind_uneven <- function(x = list) {
  vectorlist <- map(x, as.vector)
  lengthened <- map(vectorlist, `length<-`, max(lengths(vectorlist)))
  bind_cols(lengthened)
}

# Directory Creating for storing plots - If doesn't exist then create it
ifelse(!dir.exists(file.path("hmsc/hmscplots2", "comparison")), dir.create(file.path("hmsc/hmscplots2", "comparison")), FALSE)

comparisondir <- file.path("hmsc/hmscplots2/comparison/")

# Effective samples comparison - Beta
effective_samples_Beta <- lapply(model_list, function(x) {
  mpost <- convertToCodaObject(x)
  es.beta <- effectiveSize(mpost$Beta)
  return(es.beta)
})

# Plot - Beta
bind_uneven(effective_samples_Beta) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = name, y = value)) +
  geom_boxplot() +
  xlab("Model Type") +
  ylab("Effective Sample Size") + 
  theme_minimal()

ggsave(paste0(comparisondir, "/ess_Beta.png"), width = 7, height = 5, dpi = 300)

# Effective samples comparison - V
effective_samples_V <- lapply(model_list, function(x) {
  mpost <- convertToCodaObject(x)
  es.V <- effectiveSize(mpost$V)
  return(es.V)
})

# Plot - V
bind_uneven(effective_samples_V) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = name, y = value)) +
  geom_boxplot() +
  xlab("Model Type") +
  ylab("Effective Sample Size") +
  theme_minimal()

ggsave(paste0(comparisondir, "/ess_V.png"), width = 7, height = 5, dpi = 300)

# Effective samples comparison - Gamma
effective_samples_Gamma <- lapply(model_list, function(x) {
  mpost <- convertToCodaObject(x)
  es.Gamma <- effectiveSize(mpost$Gamma)
  return(es.Gamma)
})

# Plot - Gamma
bind_uneven(effective_samples_Gamma) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = name, y = value)) +
  geom_boxplot() +
  xlab("Model Type") +
  ylab("Effective Sample Size") +
  theme_minimal()

ggsave(paste0(comparisondir, "/ess_Gamma.png"), width = 7, height = 5, dpi = 300)

# Model mixing and convergence
convergence <- lapply(model_list, function(x) {
  mpost <- convertToCodaObject(x)
  ge.beta <- gelman.diag(mpost$Beta, multivariate = FALSE)$psrf
  return(ge.beta)
})

# Plot - mixing
bind_uneven(convergence) %>%
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

























