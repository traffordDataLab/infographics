# Histograms #
# data: Antidepressant prescriptions by CCG
# source: Fingertips, Public Health England (https://fingertips.phe.org.uk/)

# Load libraries ---------------------------
library(fingertipsR) ; library(tidyverse) ; library(ggplot2)

# Load lab_theme() ---------------------------
source("https://trafforddatalab.github.io/assets/theme/ggplot2/theme_lab.R")

# Load data ---------------------------
query <- fingertips_data(IndicatorID = 90527, AreaTypeID = 153)

# Tidy data ---------------------------
df <- query %>% 
  select(IndicatorID, AreaCode, AreaName, AreaType, Timeperiod, Value) %>% 
  filter(AreaType == "CCGs (pre 4/2017)" & Timeperiod == "2016/17")   

# Plot data ---------------------------
df_unit <- filter(df, AreaName == "NHS Trafford CCG")
df_background <- df

ggplot(df_unit, aes(x = Value)) +
  geom_histogram(data = df_background, 
                 bins = 30, 
                 fill = "#757575", 
                 alpha = 0.6) +
  geom_vline(aes(xintercept = median(df$Value)), colour = "#757575", linetype = "dashed") +
  geom_histogram(fill = "#fc6721", colour = "white") +
  scale_x_continuous(limits = c(0, 2.3)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Units of antidepressant prescribed per person", y = "Number of CCGs",
       title = "Trafford has an average rate of antidepressant prescribing",
       subtitle = "Public Health England, 2016-17",
       caption = "Source: Fingertips  |  @traffordDataLab") +
  theme_lab() + 
  theme(panel.grid.major.x = element_blank())
ggsave("histogram.png", scale = 0.8, dpi = 300)
       