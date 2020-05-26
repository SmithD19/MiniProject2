library(Hmsc)
library(tidyverse)

# All models have the same name (Oops). Use this to change
load(file = "hmsc/pa2_models/PA2_Full_PredPred.RData")
PA_PredX <- m
load(file = "hmsc/pa2_models/PA2_Full_PredResp.RData")
PA_PredY <- m

# Remove temp model object
rm(m)

# List of list for loops
model_list <- list("PA_PredX" = PA_PredX, "PA_PredY" = PA_PredY)

# Model Error
model_errors <- lapply(model_list, function(x) {
  preds = computePredictedValues(x)
  MF = evaluateModelFit(hM = x, predY = preds)
})

# Get names for error values
Y = map_depth(model_list, 1, pluck, "Y")
Ynames = map(Y, colnames)

# Get RMSE ------
error = map_depth(model_errors, 1, pluck, "RMSE")

# Name the error values with their species
for (i in seq_along(error)) {
  names(error[[i]]) = Ynames[[i]]
}

# Convert named vector to a tibble or df then bind each together
RMSE <- lapply(error, enframe) %>% 
  bind_rows(.id = "Model")

# Plot it
RMSE %>%
  ggplot(aes(x = name, y = value)) +
  geom_col() +
  facet_wrap(~ Model, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Species") + ylab("RMSE")

ggsave(filename = file.path("hmsc/hmscplots2/RMSE.png"), dpi = 300,
       width = 7, height = 5)



# TjurR2 ------

# Get R2 values
R2 = map_depth(model_errors, 1, pluck, "TjurR2")

# Name the R2 values with their species
for (i in seq_along(R2)) {
  names(R2[[i]]) = Ynames[[i]]
}

# Convert named vector to a tibble or df then bind each together
TjurR2 <- lapply(R2, enframe) %>% 
  bind_rows(.id = "Model")

# Plot it
TjurR2 %>%
  ggplot(aes(x = name, y = value)) +
  geom_col() +
  facet_wrap(~ Model, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Species") + ylab("TjurR2")

ggsave(filename = file.path("hmsc/hmscplots2/TjurR2.png"), dpi = 300,
       width = 7, height = 5)




















