# hexmaps #

# load libraries
library(fingertipsR) ; library(sf) ; library(hexmapr) ; library(ggplot2) ; library(viridis) ; library(gridExtra)

# load lab_theme()
source("https://trafforddatalab.github.io/assets/theme/ggplot2/theme_lab.R")

# load data (Hospital admissions for self-harm)
df <- fingertips_data(IndicatorID = 92584, AreaTypeID = 8, stringsAsFactors = F) %>% 
  filter(ParentName == "Trafford") %>% 
  rename(wd16cd = AreaCode)

# load spatial data (Trafford wards)
sf <- st_read("https://opendata.arcgis.com/datasets/afcc88affe5f450e9c03970b237a7999_3.geojson") %>% 
  filter(lad16nm == "Trafford") %>%
  st_as_sf(crs = 4326, coords = c("long", "lat"))
plot(sf)

# merge data
sf_df <- left_join(sf, df, by = "wd16cd")

# plot
plot <- ggplot(sf_df) +
  geom_sf(aes(fill = Value), colour = "white") +
  scale_fill_viridis(option = "viridis", name = 'Standardised\nAdmission\nRatio') +
  labs(x = NULL, y = NULL, 
       title = "Self-harm admissions in Trafford wards",
       caption = "Source: https://fingertips.phe.org.uk  |  @traffordDataLab") +
  theme_lab() +
  theme(axis.text = element_blank())
plot
ggsave("self-harm_map.png", dpi = 300, scale = 1)

# create grid
sp_df <- as(sf_df, 'Spatial') # convert to SpatialPolygonsDataFrame
sp_details <- get_shape_details(sp_df)
par(mfrow=c(3,3), mar = c(0,0,2,0))
for (i in 1:9){
  new_cells <-  calculate_cell_size(sp_df, sp_details, 0.03, 'hexagonal', i)
  plot(new_cells[[2]], main = paste("Seed",i, sep=" "))
}

# create final grid
raw_hex <-  calculate_cell_size(sp_df, sp_details, 0.03, 'hexagonal', 3)
sp_df_hex <- assign_polygons(sp_df, raw_hex)
sf_df_hex <- st_as_sf(sp_df_hex) # convert back to simple features

# plot hexmap
hexplot <- ggplot(sf_df_hex) +
  geom_sf(aes(fill = Value), colour = "white") +
  geom_text(aes(V1,V2, label = substr(wd16nm,1,4)), size = 3,color = "white") +
  scale_fill_viridis(option = "viridis", name = 'Standardised\nAdmission\nRatio') +
  labs(x = NULL, y = NULL, 
       title = "Self-harm admissions in Trafford wards",
       caption = "Source: https://fingertips.phe.org.uk  |  @traffordDataLab") +
  theme_lab() +
  theme(axis.text = element_blank())
hexplot
ggsave("self-harm_hexmap.png", dpi = 300, scale = 1)

# plot side-by-side
g <- grid.arrange(plot, hexplot, nrow = 1, ncol = 2)
ggsave("self-harm_maps.png", dpi = 300, scale = 1, g)
