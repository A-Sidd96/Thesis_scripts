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
  mutate(`Day` = day(`Date`) - day(`Date cage set-up`)) %>% 
  drop_na() -> nonstressed_clean


df1 <-total_nonstressed_deaths_data %>%
  mutate(Cage = case_when(`Date cage set-up` == "01/11/2022" ~ "A",
                          `Date cage set-up` == "02/11/2022" ~ "B", 
                          `Date cage set-up` == "03/11/2022" ~ "C"), 
         .after = `Date cage set-up`) %>%
  pivot_longer(cols = -c(`Date cage set-up`:Line), names_to = "Date", values_to = "Deaths") %>%
  mutate(`Date` = lubridate::dmy(`Date`),
         `Date cage set-up`= lubridate::dmy(`Date cage set-up`)) %>%
  mutate(Day = as.numeric(difftime(`Date`, `Date cage set-up`, units="days"))) %>% 
  drop_na() %>%
  group_by(Cage,Line)%>%
  mutate(Alive = Deaths) %>%
  mutate(Deaths = (50 - Alive)) %>%
  mutate(DailyDeaths = Deaths - lag(Deaths, default = 0)) %>%
  mutate(Line = factor(Line, levels = c("BENAVI", "003.3", "004.2C")))
long_df1 <- df1 %>% ungroup() %>% slice(rep(1:n(), df1$DailyDeaths))


library(survival)


nonstressed_model<-coxph(Surv(Day) ~ Line, data = long_df1)
summary(nonstressed_model)


survfit(Surv(Day) ~ Line, data = long_df1) %>%
  ggsurvfit() + scale_color_manual(values=c("#000000","#D55E00","#009E73")) + theme_bw() + 
  labs(y= "Survival Probability",
       x = "Day", fill = 'Line') + coord_cartesian(xlim = c(25,51))
 