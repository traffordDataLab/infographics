# IMD 2015 : choropleth map (statistical areas) ##

# Source: Ministry of Housing, Communities & Local Government
# Publisher URL: http://opendatacommunities.org/data/societal-wellbeing/imd/indices
# Licence: Open Government Licence 3.0

# load packages  ---------------------------
library(tidyverse) ; library(sf)

# load data  ---------------------------
imd <- read_csv("https://www.traffordDataLab.io/open_data/imd_2015/IMD_2015_long.csv") %>% 
  filter(measure == "Decile" & index_domain == "Index of Multiple Deprivation") %>% 
  rename(area_code = lsoa11cd)

lsoa <- st_read("https://github.com/traffordDataLab/spatial_data/raw/master/lsoa/2011/trafford_lsoa_generalised.geojson") %>% 
  left_join(., imd, by = "area_code")

tc <- st_read("http://trafforddatalab.io/spatial_data/town_centres/trafford_town_centres.geojson")

# plot data  ---------------------------
ggplot() +
  geom_sf(data = lsoa, aes(fill = factor(value)), alpha = 1, size = 0.1) +
  geom_text(data = tc, aes(lon, lat, label = name), size = 2.5, color = "white", alpha = 1) +
  scale_fill_manual(breaks = 1:10,
                      values = c("#A31A31", "#D23B33", "#EB6F4A", "#FCB562", "#F4D78D", 
                                 "#D8E9EC", "#AAD1DE", "#75A8C8", "#4D77AE", "#353B91"),
                      labels = c("Most \ndeprived", 2:9, "Least \n deprived")) +
  labs(x = NULL, y = NULL, 
       title = "Deprivation in Trafford by LSOA: areas", 
       subtitle = "Source: English Indices of Deprivation 2015, MHCLG",
       caption = "Contains OS data @ Crown copyright 2018  |  @traffordDataLab",
       fill = 'IMD decile') +
  theme(plot.title = element_text(size = 14, colour = "#757575", hjust = 0.5),
        plot.subtitle = element_text(size = 8, colour = "#757575", hjust = 0.5),
        plot.caption = element_text(size = 6, colour = "#757575", hjust = 1, margin = margin(b = 2)),
        legend.position = c(0.25, 0.05),
        legend.title = element_text(size = 8, colour = "#757575"),
        legend.text = element_text(size = 7, colour = "#757575"),
        legend.background = element_rect(fill = "#212121", color = NA),
        legend.key = element_rect(fill = "#212121", colour = NA),
        plot.background = element_rect(fill = "#212121", color = NA), 
        panel.background = element_rect(fill = "#212121", color = NA)) +
  guides(fill = guide_legend(override.aes = list(size = 1),
                               title.position = "top", 
                               label.position = "bottom", 
                               title.hjust = 0.5,
                               label.hjust = 0.5,
                               direction = "horizontal",
                               nrow = 1,
                               keyheight = unit(0.5, units = "mm"), 
                               keywidth = unit(5, units = "mm"))) +
  coord_sf(datum = NA)

# save plot  ---------------------------
#ggsave(file = "IMD_2015_choropleth_map_areas.svg", dpi = 300, width = 6, height = 6)
ggsave(file = "IMD_2015_choropleth_map_areas.png", dpi = 300, width = 6, height = 6)
