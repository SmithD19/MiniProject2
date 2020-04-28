## spatial data analysis
library(tidyverse)
library(raster)
library(rgdal)
library(ggmap)
library(sf)

# Convenience shortcuts for ONS to Lat/Lon projections
ukgrid <- crs("+init=epsg:27700")
latlong <- crs("+init=epsg:4326")

# Load in the raster layer and project to UKGRID
map <- raster("lcm2007gb25m_.tif")
plot(map)

# Read in UK shapefile
uk <- read_sf("Countries_December_2017_Full_Clipped_Boundaries_in_Great_Britain.shp")
read_sf("Countries_December_2017_Full_Clipped_Boundaries_in_Great_Britain.") %>% ggplot + geom_sf()

# Load in data frame
load("rdat/01-processing.RData")




