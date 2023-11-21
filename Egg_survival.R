# Packages-----
library(tidyverse)
library(readxl)
install.packages("janitor")
library(janitor)
#__________________----

# Load data----
setwd("~/Documents/Documents/Thesis_Figures")
egg_data <- read_excel("Egg-Collection-Data.xlsx")


#_____________________----

# Tidy data----

tidy_egg <- egg_data %>% 
  janitor::clean_names (.)%>% 
  fill(data = ., date) %>% 
  fill(., batch) %>% 
  mutate(line = case_when(line == "003.3!" ~ "003.3",
                          line == "004.2C" ~ "004.2C",
                          line == "BENAVI" ~ "Benavi")) %>% 
  drop_na()

#____________________----

# Analyse data----

eggplot <- tidy_egg %>% 
  ggplot(aes(x = line,
             y = number_of_eggs, color = line))+
  geom_boxplot()+
  theme_bw() +
  labs(y = "Number of eggs laid in one hour",
       x = "Medfly line ")

final_eggplot <- eggplot + scale_color_manual(values=c("#D55E00","#009E73","#000000"))
print(final_eggplot)