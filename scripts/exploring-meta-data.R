library(tidyverse)
full_data <- read_csv("data/full-data.csv")

non0 <- function(x) {
  y = colSums(x != 0, na.rm = TRUE)
  return(y)
}

cover_prox0 <- full_data %>% 
  select(starts_with("proximal_cover")) %>% 
  non0()

cover_cent0 <- full_data %>% 
  select(starts_with("central_cover")) %>% 
  non0()

cover_dist0 <- full_data %>% 
  select(starts_with("distal_cover")) %>% 
  non0()

cbind(cover_cent, cover_dist, cover_prox)


mosq <- full_data %>% 
  select(starts_with("totals_n_"))

mosq %>% non0()

central <- full_data %>% 
  select(starts_with("central_cover")) %>% 
  cbind(mosq) %>% 
  mutate(central_cover_glyceria_fluitans_percent = str_extract(central_cover_glyceria_fluitans_percent, "[:digit:]")) %>% 
  mutate_all(as.numeric) 
  # mutate_all(~ na_if(., 0)) %>% 
  # filter(totals_n_larvae > 0)

# non 0 values for the columns
central %>% non0

x <- central %>% pivot_longer(-totals_n_larvae) %>% filter(totals_n_larvae != 0 & value != 0) %>% 
  group_by(name) %>% 
  summarise(
    larv = sum(totals_n_larvae)/n(),
    mean_cov = mean(value)
  )

# top ten plants with larvae per observed
topten <- x %>% filter(!str_detect(name, "total")) %>% top_n(10, larv)

# read and 
trim <- read_csv("data/trimmed-data.csv")

 trim %>% cbind(full_data %>% select(topten$name)) %>% 
  write_csv("boral_data.csv")




