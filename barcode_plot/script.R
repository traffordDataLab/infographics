## Barcode plot ##

# load libraries ---------------------------
library(tidyverse) ; library(janitor) ; library(scales)

# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data ---------------------------

# English Indices of Deprivation 2019 #
# Source: Ministry of Housing, Communities and Local Government
# Publisher URL: https://www.gov.uk/government/statistics/announcements/english-indices-of-deprivation-2019
# Licence: Open Government Licence 3.0

df <- read_csv("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833982/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators.csv") %>% 
  clean_names() %>% 
  filter(local_authority_district_name_2019 %in% c("England", "Greater Manchester Met County", "Bolton", "Bury", "Manchester", "Oldham", "Rochdale", "Salford", "Stockport", "Tameside", "Trafford", "Wigan")) %>% 
  select(lsoa11cd = 1, lad19nm = 4, 5:34) %>% 
  gather(variable, value, -lsoa11cd, -lad19nm) %>% 
  mutate(measure = case_when(str_detect(variable, "score") ~ "score", 
                             str_detect(variable, "decile") ~ "decile", 
                             str_detect(variable, "rank") ~ "rank"),
         index_domain = case_when(str_detect(variable, "index_of_multiple_deprivation") ~ "Index of Multiple Deprivation", 
                                  str_detect(variable, "employment") ~ "Employment",
                                  str_detect(variable, "education") ~ "Education",
                                  str_detect(variable, "health") ~ "Health",
                                  str_detect(variable, "crime") ~ "Crime",
                                  str_detect(variable, "barriers") ~ "Barriers to Housing and Services",
                                  str_detect(variable, "living") ~ "Living Environment",
                                  str_detect(variable, "idaci") ~ "Income Deprivation Affecting Children",
                                  str_detect(variable, "idaopi") ~ "Income Deprivation Affecting Older People",
                                  TRUE ~ "Income"),
         index_domain = fct_relevel(factor(index_domain), 
                                    "Index of Multiple Deprivation", 
                                    "Income", "Employment", "Education", "Health", "Crime",
                                    "Barriers to Housing and Services", "Living Environment",
                                    "Income Deprivation Affecting Older People",
                                    "Income Deprivation Affecting Children")) %>% 
  select(lsoa11cd, lad19nm, measure, value, index_domain) %>% 
  spread(measure, value) %>% 
  mutate(year = "2019")

# plot data ---------------------------
anchors <- data.frame(
  x = c(0, 3284, 6569, 9853, 13138, 16422, 19706, 22991, 26275, 29560, 32844),
  y = -0.08)

ggplot(filter(df, index_domain == "Index of Multiple Deprivation")) + 
  geom_segment(aes(x = rank, xend = rank, y = 0, yend = 1), 
               colour = "#000000", size = 0.25, alpha = 1) +
  geom_point(data = anchors, aes(x = x, y = y), pch = 2, colour = "#999999", size = 0.25) +
  scale_x_continuous(limits = c(0, 32844), expand = c(0.005, 0.005), 
                     breaks = c(3284, 6569, 9853, 13138, 16422, 19706, 22991, 26275, 29560, 32844),
                     labels = c(1:10)) +
  facet_wrap(~lad19nm, ncol = 1, scales = "free_y", dir = "v", strip.position = "top") +
  labs(x = NULL, y = NULL,
       title = "Relative deprivation in Greater Manchester",
       subtitle = "2019 Index of Multiple Deprivation decile from most to least deprived",
       caption = "Source: 2019 Indices of Deprivation, MHCLG | @traffordDataLab",
       tag = "Each line represents a Lower-layer Super Output Area") +
  theme_lab() +
  theme(panel.spacing = unit(0.6, "lines"),
        plot.background = element_rect(fill = "#E8E3CF"),
        panel.grid.major = element_blank(),
        plot.title = element_text(size = 26, face = "plain", hjust = 0.5, margin = margin(10, 0, 10, 0)),
        plot.subtitle = element_text(hjust = 0.5, vjust = 5),
        strip.text = element_text(face = "italic", hjust = 0.005, vjust = 1),
        axis.text.x = element_text(face = "bold"),
        axis.text.y = element_blank(),
        legend.position = "none",
        plot.tag.position = c(0.01, 0.02),
        plot.tag = element_text(size = 10, colour = "#757575", hjust = 0))

# write output ---------------------------
ggsave("GM_imd19.svg", dpi = 300, scale = 1.5)
ggsave("GM_imd19.png", dpi = 300, scale = 1.5)

