library(tidyverse)
library(janitor)

bind1 <- read_csv("data/larvae-dipping.csv") %>% clean_names() 
bind2 <- read_csv("data/larvae-meta1.csv") %>% clean_names() 
bind3 <- read_csv("data/larvae-meta2.csv") %>% clean_names() 

dat <- 
  # join these together with an inner join make sure not to duplicate obs
  left_join(bind1, unique(bind2), by = "interlink_larva_dippin_with_metadata") %>% 
  left_join(unique(bind3), by = "interlink_larva_dippin_with_metadata") %>% 
  # for some reason som rows are duplicated - axe these
  distinct() %>% 
  # also get rid of rows that cant be linked to meta data
  drop_na(interlink_larva_dippin_with_metadata) %>% 
  # any empty columns should go too 
  remove_empty("cols")

# I wrote this to a csv file an manually deleted variables I didnt want to look at
# much quicker
dat <- read_csv("data/trimmed-data.csv")
# replace this silly NA value for a proper one
dat[dat == "nr"] <- NA


cor_dat <- 
  dat %>% 
  select(site = site.x,
         year = year.x,
         dip_point = dip_point.x,
         plotid = plot_id.x,
         total_larvae = totals_n_larvae,
         66:87) %>%
  # turn all to numeric columns
  sapply(as.numeric)

cor_dat[is.na(cor_dat)] <- 0

x <- 
  cor_dat %>% 
  as.tibble() %>% 
  select(-dip_point, -plotid, -year) %>% 
  group_by(site) %>% 
  summarise_all(sum)

x$site <- NULL
x[x > 0] <- 1

x <- t(x) %>% as.data.frame()

library(cooccur)

foo <- x %>% cooccur(spp_names = TRUE, thresh = FALSE,
              type = "spp_site", only_effects = TRUE) 
  
foo2 <- foo %>% 
  pivot_wider(names_from = sp2, values_from = effects) %>% 
  as.matrix()

rownames(foo2) <- foo2[,1]
foo2 <- foo2[,-1]
foo2 <- foo2 %>% apply(FUN = as.numeric, MARGIN = c(1,2))
foo2[is.na(foo2)] <- 0

# foo2 
foo2 %>% heatmap()
# No co-occurance between anything and the larvae it seems... What other vars to use?


