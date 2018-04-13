#Proportion of claimants' age group by ward
#Source: Claimant Count by sex and age, DWP
#Publisher: https://www.nomisweb.co.uk/datasets/ucjsa

# load R packages  ---------------------------
library(tidyverse); library(ggplot2); library(svglite); library(forcats) ;

# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data  ---------------------------
# February 2018, 2011 Census Frozen Wards within Trafford, Age quinary, Claimant Count, Total
df <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_162_1.data.csv?geography=989862149...989862169&date=latest&gender=0&age=10...20&measure=1&measures=20100&select=date_name,geography_name,geography_code,gender_name,age_name,measure_name,measures_name,obs_value,obs_status_name")

# manipulate data ---------------------------
results <- df %>%
  select(date= DATE_NAME, area_name = GEOGRAPHY_NAME, area_code = GEOGRAPHY_CODE, age = AGE_NAME, measure = MEASURE_NAME, count = OBS_VALUE)%>%
  group_by(area_name,date,measure)%>%
  spread(age,count)%>%
  mutate("25-34" = `Aged 25-29`+`Aged 30-34`, "35-44" = `Aged 35-39`+`Aged 40-44`, "45-54" = `Aged 45-49`+`Aged 50-54`, "55-64" = `Aged 55-59`+`Aged 60-64`) %>%
  rename("18-24" = "Aged 18-24")%>%
  select(-starts_with("Aged"))%>%
  gather(age,count,`18-24`:`55-64`)%>%
  mutate(age = factor(age, levels = c("55-64","45-54","35-44","25-34","18-24")))%>%
  group_by(area_name)%>%
  mutate(total=sum(count),percent=count*100/total)

# plot data ---------------------------
ggplot(results,
       aes(x = area_name, y = percent, width = total, fill = age)) +
       geom_col(position = "stack", colour = "#757575", alpha=0.8) +
       facet_grid(~fct_reorder(area_name,total, .desc=T), scales = "free_x", space = "free_x") +
       scale_y_continuous(breaks = c(0, 50, 100), labels = function(y){ paste0(y, "%") })+
       scale_fill_brewer(palette = "Set2") +
          labs(x = NULL, y = NULL, fill = NULL, 
          title = "Age of claimants by Trafford ward*",
          subtitle = "February, 2018",
          caption = "*The width of the columns is proportional to the number of claimants\n\nSource: Claimant Count, DWP  |  @traffordDataLab") +
       theme_lab() +
       theme(panel.grid.major = element_blank(),
          strip.text.x = element_blank(),
          axis.text.x = element_text(hjust = 1, vjust = 0.4, angle = 90))
  
# save plot / data  ---------------------------
ggsave(file = "marimekko_plot.png", width = 9, height = 6)

