## Landmarks in Data Visualisation ##

library(ggplot2)

ggplot() +
  geom_segment(aes(x = 1775, xend = 2025, y = 0, yend = 0), colour = "#fc6721", linetype = 1, size = 1) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(limits = c(1775, 2025), breaks =c(seq(0, 2025, by = 25))) +
  labs(title = "Landmarks in Data Visualisation",
       caption = "Source: Friendly (2009)  |  @traffordDataLab") + 
  # Line chart
  geom_segment(aes(x = 1786, xend = 1786, y = 0, yend = 0.75), colour = "#212121") +
  geom_segment(aes(x = 1786, xend = 1786+2, y = 0.75, yend = 0.75), colour = "#212121") +
  annotate(geom = "point", x = 1786, y = 0, size = 2) +
  annotate(geom = "text", x = 1786, y = 0.75, hjust = -0.1, label = "Playfair's line chart", size = 2.9) +
  # Bar chart
  geom_segment(aes(x = 1786, xend = 1786, y = 0, yend = 0.65), colour = "#212121") +
  geom_segment(aes(x = 1786, xend = 1786+2, y = 0.65, yend = 0.65), colour = "#212121") +
  annotate(geom = "point", x = 1786, y = 0, size = 2) +
  annotate(geom = "text", x = 1786, y = 0.65, hjust = -0.1, label = "Playfair's bar chart", size = 2.9) +
  # Pie chart
  geom_segment(aes(x = 1801, xend = 1801, y = 0, yend = 0.55), colour = "#212121") +
  geom_segment(aes(x = 1801, xend = 1801+2, y = 0.55, yend = 0.55), colour = "#212121") +
  annotate(geom = "point", x = 1801, y = 0, size = 2) +
  annotate(geom = "text", x = 1801, y = 0.55, hjust = -0.1, label = "Playfair's pie chart", size = 2.9) +
  # Choropleth map
  geom_segment(aes(x = 1826, xend = 1826, y = 0, yend = 0.85), colour = "#212121") +
  geom_segment(aes(x = 1826, xend = 1826+2, y = 0.85, yend = 0.85), colour = "#212121") +
  annotate(geom = "point", x = 1826, y = 0, size = 2) +
  annotate(geom = "text", x = 1826, y = 0.85, hjust = -0.1, label = "Dupin's thematic map", size = 2.9) +
  # Dot density map
  geom_segment(aes(x = 1830, xend = 1830, y = 0, yend = 0.65), colour = "#212121") +
  geom_segment(aes(x = 1830, xend = 1830+2, y = 0.65, yend = 0.65), colour = "#212121") +
  annotate(geom = "point", x = 1830, y = 0, size = 2) +
  annotate(geom = "text", x = 1830, y = 0.65, hjust = -0.1, label = "Fr\u{E9}re de Montizon's dot density map", size = 2.9) +
  # Scatterplot
  geom_segment(aes(x = 1833, xend = 1833, y = 0, yend = 0.55), colour = "#212121") +
  geom_segment(aes(x = 1833, xend = 1833+2, y = 0.55, yend = 0.55), colour = "#212121") +
  annotate(geom = "point", x = 1833, y = 0, size = 2) +
  annotate(geom = "text", x = 1833, y = 0.55, hjust = -0.1, label = "Herschel's scatterplot", size = 2.9) +
  # Dot map
  geom_segment(aes(x = 1855, xend = 1855, y = 0, yend = 0.45), colour = "#212121") +
  geom_segment(aes(x = 1855, xend = 1855+2, y = 0.45, yend = 0.45), colour = "#212121") +
  annotate(geom = "point", x = 1855, y = 0, size = 2) +
  annotate(geom = "text", x = 1855, y = 0.45, hjust = -0.1, label = "Snow's cholera map of London", size = 2.9) +
  # Coxcomb
  geom_segment(aes(x = 1857, xend = 1857, y = 0, yend = 0.35), colour = "#212121") +
  geom_segment(aes(x = 1857, xend = 1857+2, y = 0.35, yend = 0.35), colour = "#212121") +
  annotate(geom = "point", x = 1857, y = 0, size = 2) +
  annotate(geom = "text", x = 1857, y = 0.35, hjust = -0.1, label = "Nightingale's Coxcomb", size = 2.9) +
  # Flow map
  geom_segment(aes(x = 1869, xend = 1869, y = 0, yend = 0.25), colour = "#212121") +
  geom_segment(aes(x = 1869, xend = 1869+2, y = 0.25, yend = 0.25), colour = "#212121") +
  annotate(geom = "point", x = 1869, y = 0, size = 2) +
  annotate(geom = "text", x = 1869, y = 0.25, hjust = -0.1, label = "Minard's flow map", size = 2.9) + 
  # Gantt chart
  geom_segment(aes(x = 1917, xend = 1917, y = 0, yend = 0.15), colour = "#212121") +
  geom_segment(aes(x = 1917, xend = 1917+2, y = 0.15, yend = 0.15), colour = "#212121") +
  annotate(geom = "point", x = 1917, y = 0, size = 2) +
  annotate(geom = "text", x = 1917, y = 0.15, hjust = -0.1, label = "Gantt's Gantt chart", size = 2.9) + 
  # Boxplot
  geom_segment(aes(x = 1977, xend = 1977, y = 0, yend = 0.45), colour = "#212121") +
  geom_segment(aes(x = 1977, xend = 1977+2, y = 0.45, yend = 0.45), colour = "#212121") +
  annotate(geom = "point", x = 1977, y = 0, size = 2) +
  annotate(geom = "text", x = 1977, y = 0.45, hjust = -0.1, label = "Tukey's Boxplot", size = 2.9) +
  # Dot chart
  geom_segment(aes(x = 1985, xend = 1985, y = 0, yend = 0.25), colour = "#212121") +
  geom_segment(aes(x = 1985, xend = 1985+2, y = 0.25, yend = 0.25), colour = "#212121") +
  annotate(geom = "point", x = 1985, y = 0, size = 2) +
  annotate(geom = "text", x = 1985, y = 0.25, hjust = -0.1, label = "Cleveland's Dot chart", size = 2.9) +
  # Sparklines
  geom_segment(aes(x = 2004, xend = 2004, y = 0, yend = 0.15), colour = "#212121") +
  geom_segment(aes(x = 2004, xend = 2004+2, y = 0.15, yend = 0.15), colour = "#212121") +
  annotate(geom = "point", x = 2004, y = 0, size = 2) +
  annotate(geom = "text", x = 2004, y = 0.15, hjust = -0.1, label = "Tufte's Sparklines", size = 2.9) +
  theme(plot.background = element_rect(fill = '#EFE8D6'),
        panel.background = element_rect(fill = '#EFE8D6'),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", family = "Roboto"),
        plot.caption = element_text(size = 8, hjust = 0.95, vjust = -1.3, family = "Roboto"),
        axis.title = element_blank(), 
        axis.text.x = element_text(colour = "#212121"),  
        axis.text.y = element_blank(),  
        axis.ticks = element_blank(),
        text = element_text(family = "Roboto", colour = "#212121"),
        aspect.ratio = 0.2)

ggsave("timeline.png", dpi = 300, scale = 1.5, width = 8, height = 2)
