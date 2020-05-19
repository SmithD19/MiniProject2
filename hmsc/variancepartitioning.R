# Variance plotting

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



# Variance Plot Example ---------------------------------------------------


structure <- c("width", "waterboard", "shaded", "edgedepth", "centre_depth")
plant_cover <- c("total_cover")
chemistry <- c("wtemp", "turbidity", "salinity", "do", "ph")
management <- c("management3")
landuse <- c("landcoverrough pasture", "arable_horticulture", "broadleaved_woodland", "improved_grassland",
             "neutral_grassland", "rough_grassland", "freshwater")

groupnames <- c("Structure", "Plant Cover", "Water Chemistry", "Management", "Land Use")

groups1 <- rep(1, length.out = length(structure))
groups2 <- rep(2, length.out = length(plant_cover))
groups3 <- rep(3, length.out = length(chemistry))
groups4 <- rep(4, length.out = length(management))
groups5 <- rep(5, length.out = length(landuse))

groups <- c(1, groups1, groups2, groups3, groups4, groups5)

# Directory Creating for storing plots - If doesn't exist then create it
ifelse(!dir.exists(file.path("hmsc/", "hmscplots")), dir.create(file.path("hmsc/", "hmscplots")), FALSE)

plotdirectory <- file.path("hmsc/hmscplots/")

for (i in seq_along(model_list)) {
  vp <- computeVariancePartitioning(model_list[[i]], group = groups, groupnames = groupnames)
  pal <- viridis::plasma(n = length(groupnames) + 2)
  # Device settings for PNG
  png(
    filename = paste0(plotdirectory, "/VP_", names(model_list)[[i]], ".png"),
    res = 320,
    units = "in",
    width = 7,
    height = 5
  )
  # Plotting
  plotVariancePartitioning(hM = model_list[[i]], vp, col = pal)
  # Devices off
  dev.off()
}




























