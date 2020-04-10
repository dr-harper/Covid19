

library(here)
library(readr)
library(tidyverse)
library(sf)
library(maps)
library(gganimate)
library(patchwork)
library(ggthemr)
library(grid)

# devtools::install_github('cttobin/ggthemr')
ggthemr("light")

# Load Data
df_all_extra <- read_csv(here("data/global/covid_data_global.csv"),
                         col_types = cols())
world_map <- sf::read_sf(here("data/global/covid_data_global_boundaries.geojson"))

df_all_spatial <-
  df_all_extra %>%
  left_join(world_map)




df_all_extra %>%
  filter(type == "cases") %>%
ggplot() +
  geom_point(aes(x = daysSinceOutbreak, y = value, group = region, colour = Income)) +
  stat_smooth(aes(x = daysSinceOutbreak, y = value, colour = Income))
  scale_y_log10()
