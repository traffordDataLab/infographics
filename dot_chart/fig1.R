## Road Safety: Casualties by ward and mode of travel, 2016 ##

# Source: Greater Manchester Police
# Publisher URL: https://data.gov.uk/dataset/road-accidents-safety-data
# Licence: Open Government Licence

# load R packages  ---------------------------
library(tidyverse) ; library(sf); library(ggplot2)

# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data  ---------------------------
casualties <- read_csv("https://www.trafforddatalab.io/open_data/road_casualties/2016/STATS19_casualty_data_2016.csv") %>% 
  filter(area_name == "Trafford") %>% 
  select(-area_name) %>% 
  st_as_sf(crs = 4326, coords = c("long", "lat"))

# load geospatial data  ---------------------------
wards <- st_read("https://www.trafforddatalab.io/spatial_data/ward/2017/trafford_ward_full_resolution.geojson") %>% 
  st_as_sf(crs = 4326, coords = c("long", "lat")) %>% 
  select(-lat, -lon)

# manipulate data ---------------------------
sf <- st_intersection(casualties, wards) %>% 
  st_set_geometry(value = NULL)

results <- sf %>% 
  mutate(mode = fct_recode(mode, 
                           "Other" = "Bus or Coach",
                           "Other" = "Goods Vehicle",
                           "Other" = "Other Vehicle",
                           "Other" = "Taxi")) %>% 
  group_by(area_name, mode) %>% 
  summarise(n = n()) %>% 
  ungroup() %>%
  complete(area_name, mode, fill = list(n = 0)) %>% 
  spread(mode, n) %>% 
  mutate(Total = rowSums(.[2:6])) %>% 
  gather(mode, n, -area_name) %>% 
  mutate(mode = factor(mode, 
                      levels = c("Car","Pedal Cycle", "Pedestrian",
                                 "Powered 2 Wheeler", "Other", "Total")))

# plot data  ---------------------------
ggplot(results, aes(x = n, y = area_name, colour = mode)) +
  geom_segment(aes(x = 0, xend = n, 
                   y = area_name, yend = area_name), linetype = "dotted", colour = "#212121", size = 0.5) + 
  geom_point(size = 4) +
  geom_text(aes(label = n), size = 3, colour = "white") +
  scale_fill_brewer(palette = "Set2") +
  scale_y_discrete(limits = rev(unique(sort(results$area_name)))) +
  labs(x = NULL, y = NULL, colour = NULL,
       caption = "Source: Greater Manchester Police  |  @traffordDataLab") +
  facet_grid(.~mode, scales = "free_x") +
  theme_lab() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.grid.major = element_blank(),
        panel.spacing = unit(3, "lines"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(hjust = 0),
        strip.text.x = element_text(size = 10, face = "bold", angle = 0, hjust = 0, vjust = 1),
        legend.position = "none")

# save plot / data  ---------------------------
ggsave(file = "output/figures/fig1.svg", scale = 1.8, width = 10, height = 5)
ggsave(file = "output/figures/fig1.png", scale = 1.5, width = 10, height = 5)

write_csv(results, "output/data/fig1.csv")
