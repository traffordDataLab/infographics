# Small multiples #
# data: English Index of Multiple Deprivation 2015
# source: DCLG, 2015 (https://www.gov.uk/government/statistics/english-indices-of-deprivation-2015)

# Load data ---------------------------
library(tidyverse) ; library(forcats) ; library(scales) ; library(sf) ; library(ggplot2) ; library(cowplot)
source("https://trafforddatalab.github.io/assets/theme/ggplot2/theme_lab.R")
imd_2015 <- st_read("https://github.com/traffordDataLab/assets/raw/master/theme/leaflet/data/IMD_2015.geojson")

# Plot data ---------------------------
fill <- c("#A31A31", "#D23B33", "#EB6F4A", "#FCB562", "#F4D78D", "#D8E9EC", "#AAD1DE", "#75A8C8", "#4D77AE", "#353B91")

p1 <- imd_2015 %>%
  st_set_geometry(value = NULL) %>% 
  count(lad15nm, decile) %>%
  group_by(lad15nm) %>%
  mutate(pct = n/sum(n)) %>% 
  ggplot(aes(fct_rev(factor(decile)), n)) +
  geom_col(aes(fill = factor(decile)), alpha = 0.8, show.legend = FALSE) +
  geom_text(aes(label = paste(percent(pct), " (", n, ")", sep = "")), colour = "#212121", size = 3, hjust = 0, nudge_y = 5) +
  scale_fill_manual(values = fill) +
  scale_y_continuous(limits=c(0, 250), expand = c(0,0)) +
  coord_flip() +
  facet_wrap(~lad15nm, nrow = 2) +
  labs(x = NULL, y = NULL,
       title = "% of LSOAs* in GM Local Authorities by Index of Multiple Deprivation (2015) decile",
       subtitle = "1 = most deprived, 10 = least deprived",
       caption = "Source: DCLG, 2015  |  @traffordDataLab") +
  theme_lab() +
  theme(plot.margin = unit(c(rep(1.5, 4)), "cm"),
        panel.spacing = unit(1, "lines"),
        panel.grid.major = element_blank(),
        plot.title = element_text(vjust = 2),
        plot.subtitle = element_text(vjust = 5),
        axis.text.y = element_text(face = "bold"),
        axis.text.x = element_blank(),
        strip.text.x = element_text(size = 10, face = "bold", angle = 0, hjust = 0, vjust = 1))

p2 <- add_sub(p1, "*Each LSOA has an average of approximately 1,500 residents", size = 10,
              x = 0, hjust = 0)
ggsave("GM_IMD_2015.png", dpi = 300, scale = 1)


