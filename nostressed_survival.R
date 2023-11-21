# Packages-----
library(tidyverse)
library(readxl)
library(lubridate)

#__________________----

# Load data----

setwd("~/Documents/Documents/Thesis_Figures")
total_nonstressed_deaths_data <- read_csv("corrected-non-stressed-medfly-deaths.csv")

# analysing data-------

nonstressed_plot <- total_nonstressed_deaths_data %>%
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

final_nonstressed_plot <- nonstressed_plot +  scale_color_manual(values=c("#D55E00","#009E73","#000000"))
print(final_nonstressed_plot)

#stats tests

total_nonstressed_deaths_data %>%
  mutate(Cage = case_when(`Date cage set-up` == "01/11/2022" ~ "A",
                          `Date cage set-up` == "02/11/2022" ~ "B", 
                          `Date cage set-up` == "03/11/2022" ~ "C"), 
         .after = `Date cage set-up`) %>%
  pivot_longer(cols = -c(`Date cage set-up`:Line), names_to = "Date", values_to = "Deaths") %>%
  mutate(`Date` = lubridate::dmy(`Date`),
         `Date cage set-up`= lubridate::dmy(`Date cage set-up`)) %>%
  mutate(`Day` = days(`Date`) - days(`Date cage set-up`)) %>% 
  drop_na() -> nonstressed_clean

nonstressed_clean[nonstressed_clean$Line %in% c('BENAVI', '003.3'),] -> nonstressed_data_003.3
t.test(nonstressed_data_003.3$Deaths ~ nonstressed_data_003.3$Line)

nonstressed_clean[nonstressed_clean$Line %in% c('BENAVI', '004.2C'),] -> nonstressed_data_004.2C
t.test(nonstressed_data_004.2C$Deaths ~ nonstressed_data_004.2C$Line)