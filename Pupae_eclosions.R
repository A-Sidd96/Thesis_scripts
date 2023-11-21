# Packages-----
library(tidyverse)
library(readxl)
library(lubridate)

#__________________----

# Load data----

total_eclosion_data <- read_csv("total-eclosion-data.csv")

# analysing data-------

total_eclosion_data %>%
  mutate(Cage = case_when(`Date cage set-up` == "01/11/2022" ~ "A",
                          `Date cage set-up` == "02/11/2022" ~ "B", 
                          `Date cage set-up` == "03/11/2022" ~ "C"), 
         .after = `Date cage set-up`) %>%
  pivot_longer(cols = -c(`Date cage set-up`:Line), names_to = "Date", values_to = "Eclosions") %>%
  mutate(`Date` = lubridate::dmy(`Date`),
         `Date cage set-up`= lubridate::dmy(`Date cage set-up`)) %>%
  mutate(`Day` = days(`Date`) - days(`Date cage set-up`)) %>% 
  drop_na() -> eclosion_data

eclosion_data[eclosion_data$Line %in% c('BENAVI', '003.3'),] -> eclosion_data_003.3
t.test(eclosion_data_003.3$Eclosions ~ eclosion_data_003.3$Line)

eclosion_data[eclosion_data$Line %in% c('BENAVI', '004.2C'),] -> eclosion_data_004.2C
t.test(eclosion_data_004.2C$Eclosions ~ eclosion_data_004.2C$Line)