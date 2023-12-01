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
  mutate(`Day` = day(`Date`) - day(`Date cage set-up`)) %>% 
  drop_na() %>% 
  ggplot(aes(x = `Day`, y = Deaths, colour = `Line`))+
  geom_jitter()+
  geom_smooth(aes(group = Line), se = FALSE)+
  #scale_x_date()+
  theme_set(theme_bw())+
  theme(legend.position = "right") +
  labs(y= "Live flies",
       x = "Day", fill = 'Line')

final_stressed_plot <- stressed_plot +  scale_color_manual(values=c("#D55E00","#009E73","#000000"))
print(final_stressed_plot)

#stats tests

df <-stressed_medfly_death_data %>%
  mutate(Cage = case_when(`Date cage set-up` == "01/11/2022" ~ "A",
                          `Date cage set-up` == "02/11/2022" ~ "B", 
                          `Date cage set-up` == "03/11/2022" ~ "C"), 
         .after = `Date cage set-up`) %>%
  pivot_longer(cols = -c(`Date cage set-up`:Line), names_to = "Date", values_to = "Deaths") %>%
  mutate(`Date` = lubridate::dmy(`Date`),
         `Date cage set-up`= lubridate::dmy(`Date cage set-up`)) %>%
  mutate(`Day` = day(`Date`) - day(`Date cage set-up`)) %>% 
  drop_na() %>%
  group_by(Cage,Line)%>%
  mutate(Alive = Deaths) %>%
  mutate(Deaths = (50 - Alive)) %>%
  mutate(DailyDeaths = Deaths - lag(Deaths, default = 0)) %>%
  mutate(Line = factor(Line, levels = c("Benavi", "003.3", "004.2C")))
long_df <- df %>% ungroup() %>% slice(rep(1:n(), df$DailyDeaths))
  -> stressed_clean

library(survival)


stressed_model<-coxph(Surv(Day) ~ Line, data = long_df)
summary(stressed_model)


survfit(Surv(Day) ~ Line, data = long_df) %>%
  ggsurvfit()+ coord_cartesian(xlim = c(20,25)) + scale_color_manual(values=c("#000000","#D55E00","#009E73")) + theme_bw() + 
  labs(y= "Survival Probability",
       x = "Day", fill = 'Line') +
  scale_fill_discrete(labels=c("Line=Benavi"="Benavi","Line=003.3"="003.3","Line=004.2C"="004.2C"))