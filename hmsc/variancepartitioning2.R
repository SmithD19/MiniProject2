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

# Ease transform function
vp2df <- function(vp) {
  # A function to take a variance partition and turn it into a df
  x <- as.data.frame(vp$vals)
  x <- rownames_to_column(x, "fixeff")
  return(x)
}

vp <- computeVariancePartitioning(PA_PredY)
                                  
# Groupings for plot
group <- vp$group
# Group for Structural + Intercept
group[1:6] <- 1
# Plant
group[7:10] <- 2
# Management
group[11] <- 3
# Season
group[12:13] <- 4
# Landcover
group[14:18] <- 5

# Groupnames
groupnames <- c("Structure", "Plants", "Management", "Seasons", "Landcover")

vp <- computeVariancePartitioning(PA_PredY, group = group, groupnames = groupnames)

plotVariancePartitioning(PA_PredY, vp)

vpPredY <- vp2df(vp)


### NOW WITH OTHER MODEL

vp <- computeVariancePartitioning(PA_PredX)

# Groupings for plot
group <- vp$group
# Group for Structural + Intercept
group[1:6] <- 1
# Plant
group[7:10] <- 2
# Management
group[11] <- 3
# Season
group[12:13] <- 4
# Landcover
group[14:18] <- 5
# Predators
group[19:34] <- 6

# Groupnames
groupnames <- c("Structure", "Plants", "Management", "Seasons", "Landcover", "Predators")

vp <- computeVariancePartitioning(PA_PredX, group = group, groupnames = groupnames)

plotVariancePartitioning(PA_PredX, vp)

vpPredX <- vp2df(vp)

pal1 <- viridisLite::inferno(7)

## Plot
p1 <- vpPredY %>% select(fixeff, cx_pipiens:cs_annulata) %>% 
  pivot_longer(-fixeff) %>% 
  ggplot(aes(x = name, y = value, fill = fixeff)) +
  geom_col() +
  scale_fill_manual(values = pal1)+
  xlab("Species") +
  ylab("Variance Proportion") +
  theme_classic()

pal2.1 <- pal1[1:3]
pal2.2 <- pal1[4:7]
pal2.3 <- "steelblue"
pal2 <- append(pal2.1, c(pal2.3, pal2.2))



## Plot
p2 <- vpPredX %>%
  pivot_longer(-fixeff) %>% 
  ggplot(aes(x = name, y = value, fill = fixeff)) +
  geom_col() +
  scale_fill_manual(values = pal2) +
  xlab("Species") +
  ylab("Variance Proportion") +
  theme_classic()

library(patchwork)

p1 / p2
ggsave(file="a4_output2.png", width = 210, height = 297, units = "mm", dpi = 300)



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


