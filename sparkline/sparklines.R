## sparklines ##

# load necessary packages
library(tidyverse); library(ggplot2)
source("https://trafforddatalab.github.io/assets/theme/ggplot2/theme_lab.R")

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
  select(month, area_name, rate)

mins <- group_by(df, area_name) %>% slice(which.min(rate))
maxs <- group_by(df, area_name) %>% slice(which.max(rate))
ends <- group_by(df, area_name) %>% filter(month == max(month))
quarts <- df %>%
  group_by(area_name) %>%
  summarize(quart1 = quantile(rate, 0.25),
            quart2 = quantile(rate, 0.75)) %>%
  right_join(df)

ggplot(df, aes(x = month, y = rate)) + 
  geom_ribbon(data = quarts, aes(ymin = quart1, max = quart2), fill = '#f0f0f0') +
  geom_line(colour = "#757575", size = 0.3) +
  geom_point(data = mins, col = 'red', size = 0.5) +
  geom_text(data = mins, aes(label = rate), size = 3, vjust = 2) +
  geom_point(data = maxs, col = 'blue', size = 0.5) +
  geom_text(data = maxs, aes(label = rate), size = 3, hjust = -0.25, vjust = 0.5) +
  geom_text(data = ends, aes(label = rate), size = 3, hjust = -0.2) +
  expand_limits(x = max(df$month) + (0.25 * (max(df$month) - min(df$month)))) +
  scale_x_date(date_labels = "'%y") +
  scale_y_continuous(limits = c(-2, NA), expand = c(0.1, 0)) +
  facet_wrap(~area_name, nrow = 7, strip.position = "top") + 
  labs(title = "Burglary rates in Trafford's wards",
       subtitle = "December 2014 - November 2017") +
  theme_lab() +
  theme(panel.spacing = unit(0, "lines"),
        axis.title=element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_text(hjust = 0.02),
        plot.subtitle = element_text(hjust = 0.02, vjust = 70),
        strip.text = element_text(size = 8, face = "bold", angle = 0, hjust = 0.05, vjust = 1))

ggsave(file = "sparkline_plot.svg", dpi = 300, scale = 1)
ggsave(file = "sparkline_plot.png", dpi = 300, scale = 1)
