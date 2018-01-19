## geofaceting ##

# load necessary packages
library(geofacet) ; library(ggplot2)

# create grid for Trafford's wards
traff_grid <- data.frame(
  row = c(1, 2, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7),
  col = c(5, 4, 6, 5, 3, 4, 3, 2, 3, 4, 5, 1, 2, 3, 1, 4, 2, 3, 4, 3, 2),
  code = c("E05000829", "E05000826", "E05000825", "E05000832", "E05000827", "E05000836", "E05000838", "E05000828", "E05000820", "E05000833", "E05000835", "E05000824", "E05000834", "E05000823", "E05000821", "E05000839", "E05000822", "E05000837", "E05000830", "E05000831", "E05000819"),
  name = c("Gorse Hill", "Davyhulme East", "Clifford", "Longford", "Davyhulme West", "Stretford", "Urmston", "Flixton", "Ashton upon Mersey", "Priory", "Sale Moor", "Bucklow-St Martins", "St Mary's", "Brooklands", "Bowdon", "Village", "Broadheath", "Timperley", "Hale Barns", "Hale Central", "Altrincham"),
  stringsAsFactors = FALSE
)

# load number of households (Census 2011)
hlds <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_619_1.data.csv?date=latest&geography=1237320482...1237320496,1237320498,1237320497,1237320499...1237320502,1946157089,1937768449&rural_urban=0&cell=0&measures=20100&select=date_name,geography_name,geography_code,rural_urban_name,cell_name,measures_name,obs_value,obs_status_name") %>% 
  select(area_code = GEOGRAPHY_CODE, area_name = GEOGRAPHY_NAME, households = OBS_VALUE)

# load crime data and calculate rate
df <- read_csv("https://github.com/traffordDataLab/open_data/raw/master/police_recorded_crime/data/trafford.csv") %>% filter(category != "Anti-social behaviour") %>% 
  filter(category == "Burglary") %>% 
  group_by(month, area_code, area_name) %>% 
  count() %>% 
  ungroup() %>% 
  left_join(., hlds, by = "area_name") %>% 
  mutate(rate = round((n/households)*1000,1)) %>% 
  select(month, name = area_name, rate)

# plot with the Lab's style
source("https://trafforddatalab.github.io/assets/theme/ggplot2/theme_lab.R")
ggplot(df, aes(month, rate)) +
  geom_line(colour = "#fc6721", size = 0.8, alpha = 0.8) +
  scale_x_date(date_labels = "%Y") +
  scale_y_continuous(breaks = c(0,2,4)) +
  facet_geo(~name, grid = traff_grid) +
  labs(x = NULL, y = NULL,
       title = "Burglaries in Trafford's wards, 2015-2017",
       subtitle = "Crimes per 1,000 households",
       caption = "Sources: data.police.uk; 2011 Census  |  @traffordDataLab") +
  theme_lab() +
  theme(plot.background = element_rect(fill = 'white'),
        panel.grid.major.x = element_blank(),
        strip.text = element_text(size = 8, face = "bold"),
        axis.text = element_text(size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file = "geofacet_plot.png", dpi = 300, scale = 1)