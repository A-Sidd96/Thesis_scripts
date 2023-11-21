# Packages-----
library(tidyverse)
library(readxl)
library(lubridate)
#__________________----

# Load data----
setwd("~/Documents/Documents/Thesis_Figures")
stressed_medfly_death_data <- read_csv("stressed-medfly-death-improved-dataset.csv")

#__________________----

# analysing data-------

stressed_plot <- stressed_medfly_death_data %>%
  mutate(Cage = case_when(`Date cage set-up` == "01/11/2022" ~ "A",
                          `Date cage set-up` == "02/11/2022" ~ "B", 
                          `Date cage set-up` == "03/11/2022" ~ "C"), 
         .after = `Date cage set-up`) %>%
  pivot_longer(cols = -c(`Date cage set-up`:Line), names_to = "Date", values_to = "Deaths") %>%
  mutate(`Date` = lubridate::dmy(`Date`),
         `Date cage set-up`= lubridate::dmy(`Date cage set-up`)) %>%
  mutate(`Day` = days(`Date`) - days(`Date cage set-up`)) %>% 
  drop_na() %>% 
  ggplot(aes(x = `Date`, y = Deaths, colour = `Line`))+
  geom_jitter()+
  geom_smooth(aes(group = Line), se = FALSE)+
  scale_x_date()+
  theme_set(theme_bw())+
  theme(legend.position = "right")

final_stressed_plot <- stressed_plot +  scale_color_manual(values=c("#D55E00","#009E73","#000000"))
print(final_stressed_plot)