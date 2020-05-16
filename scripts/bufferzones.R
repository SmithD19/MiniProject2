### Spatial Buffers, Land Use and Others ###

# Packages:
library(tidyverse)
library(raster)
library(rgdal)
library(rgeos)

# Land use map 2007: https://www.ceh.ac.uk/services/land-cover-map-2007
land2007 <- raster("spatial/lcm2007gb25m_.tif")
print(land2007)
OBGS <- projection(land2007)
OBGS <- crs(OBGS)

# Data
load("masterdf2.RDS")

# Unique site/plot data
df <- dftidy_wide

# Get the site and plot spatial points
sites <- df %>%
  select(site, plot, easting, northing) %>%
  # New Plot ID column
  unite("plot_id", site, plot) %>%
  # Only need unique instances
  distinct()

coords <- sites %>%
  select(x = easting, y = northing) %>%
  data.matrix()

# Turn into coordinates
points <- SpatialPointsDataFrame(coords = coords,
                                 data = sites,
                                 proj4string = OBGS)

# Make 100m Buffers
buff100 <-
  gBuffer(
    points,
    byid = TRUE,
    width = 100,
    capStyle = "ROUND",
    joinStyle = "ROUND"
  )

# Make 300m Buffers
buff300 <-
  gBuffer(
    points,
    byid = TRUE,
    width = 300,
    capStyle = "ROUND",
    joinStyle = "ROUND"
  )

# What is under 100m buffers?
luse_100 <- raster::extract(land2007, buff100)
names(luse_100) <- points$plot_id

# what is under 300m buffers?
luse_300 <- raster::extract(land2007, buff300)
names(luse_300) <- points$plot_id

# CEH Land Use 2007 Classes
land_types <- c(
  "broadleaved_woodland",
  "coniferous_woodland",
  "arable_horticulture",
  "improved_grassland",
  "rough_grassland",
  "neutral_grassland",
  "calcareous_grassland",
  "acid_grassland",
  "fen_marsh_swamp",
  "heather",
  "heather_grassland",
  "bog",
  "montane",
  "inland_rock",
  "salt_water",
  "freshwater",
  "supralittoral_rock",
  "supralittoral_sediment",
  "littoral_rock",
  "littoral_sediment",
  "saltmarsh",
  "urban",
  "suburban"
)

# Key of land values
land_key = 1:23 %>% as.character()

# Lookup table for easy replacing of variables
lookup <- data.frame(old_val = land_key,
                     new_val = land_types)

# Calculate whats in each plot_id area by %?
dict <- function(x) {
  y = lookup$new_val[match(unlist(x), lookup$old_val)]
  y = table(y)
  enframe(y)
}

foo <- luse_100 %>%
  map(dict) %>%
  bind_rows(.id = "plot_id") %>%
  group_by(plot_id) %>%
  mutate(freq = value / sum(value)) %>%
  filter(value > 0) %>%
  pivot_wider(
    id_cols = plot_id,
    names_from = name,
    values_from = freq,
    values_fill = list(freq = 0)
  ) %>% 
  separate(col = plot_id,
           into = c("site", "plot"),
           sep = "_")




# Create dataframes with this new function
df100m <-
  luse_100 %>%
  map(dict) %>%
  bind_rows(.id = "plot_id") %>%
  group_by(plot_id) %>%
  mutate(freq = value / sum(value)) %>%
  filter(value > 0) %>%
  pivot_wider(
    id_cols = plot_id,
    names_from = name,
    values_from = freq,
    values_fill = list(freq = 0)
  ) %>% 
  separate(col = plot_id,
           into = c("site", "plot"),
           sep = "_")


df300m <-
  luse_300 %>%
  map(dict) %>%
  bind_rows(.id = "plot_id") %>%
  group_by(plot_id) %>%
  mutate(freq = value / sum(value)) %>%
  filter(value > 0) %>%
  pivot_wider(
    id_cols = plot_id,
    names_from = name,
    values_from = freq,
    values_fill = list(freq = 0)
  ) %>% 
  separate(col = plot_id,
           into = c("site", "plot"),
           sep = "_")


# Save these  as .csv
write_csv(df100m, "data/buffer100m.csv")
write_csv(df300m, "data//buffer300m.csv")

# # Save easy formats as RDS
# library(gdata)
# gdata::keep(df100m, df300m, sure = T)
# save.image("data/buffers.RData")
