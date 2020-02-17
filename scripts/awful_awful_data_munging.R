library(tidyverse)
library(janitor)

dip <- 
  read_csv("files/larvae-dipping-totals.csv") %>% 
  # change the column names to something more legible
  clean_names() %>% 
  # get rid of columns that are empty 
  remove_empty()

glimpse(dip)

dip %>% write_csv("data/larvae-dipping.csv")

dip %>% summary()





meta1 <- 
  read_csv("files/larvae-meta1.csv") %>% 
  clean_names() %>% 
  remove_empty()

meta %>% summary.data.frame() %>% write.csv("test.csv")

meta2 <- read_csv("data/larvae-meta2.csv") %>% clean_names() %>% remove_empty()

complete <- full_join(dip, meta1, by = "interlink_larva_dippin_with_metadata") %>% full_join(meta2, by = "interlink_larva_dippin_with_metadata")
