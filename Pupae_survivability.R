setwd("~/Documents/Documents/Thesis_Figures")
pupae_collection_data <- read_csv("pupae-collection-improved-cumulative.csv")

#__________________----

# tidying the data------

pupae_survival <- pupae_collection_data %>% 
  mutate(`Date cage set-up` = lubridate::mdy(`Date cage set-up`)) %>% 
  mutate(Cage = case_when(`Date cage set-up` == "2022-11-01" ~ "A",
                          `Date cage set-up` == "2022-11-02" ~ "B", 
                          `Date cage set-up` == "2022-11-03" ~ "C"), 
         .after = `Date cage set-up`)%>% 
  pivot_longer(cols = -c(`Date cage set-up`:Line), names_to = "Date", values_to = "Pupae") %>% 
  mutate(`Date` = lubridate::dmy(`Date`)) %>% 
  drop_na()%>% 
  mutate(`Day` = days(`Date`) - days(`Date cage set-up`)) %>% 
  ggplot(aes(x = `Date`, y = Pupae, colour = `Line`))+
  geom_jitter()+
  geom_smooth(aes(group = Line), se = FALSE)+
  scale_x_date()+ 
  theme_set(theme_bw())+
  theme(legend.position = "right")

pupae_survival_done <- pupae_survival + scale_color_manual(values=c("#D55E00","#009E73","#000000"))
print(pupae_survival_done)

#stats tests

pupae_collection_data %>% 
  mutate(`Date cage set-up` = lubridate::mdy(`Date cage set-up`)) %>% 
  mutate(Cage = case_when(`Date cage set-up` == "2022-11-01" ~ "A",
                          `Date cage set-up` == "2022-11-02" ~ "B", 
                          `Date cage set-up` == "2022-11-03" ~ "C"), 
         .after = `Date cage set-up`)%>% 
  pivot_longer(cols = -c(`Date cage set-up`:Line), names_to = "Date", values_to = "Pupae") %>% 
  mutate(`Date` = lubridate::dmy(`Date`)) %>% 
  drop_na()%>% 
  mutate(`Day` = days(`Date`) - days(`Date cage set-up`)) -> pupae_survival_tidy

pupae.survival.one.way <- aov(pupae_survival_tidy$Pupae ~ pupae_survival_tidy$Line)
summary(pupae.survival.one.way)

pupae_survival_tidy[pupae_survival_tidy$Line %in% c('Benavi', '003.3'),] -> data_003.3
t.test(data_003.3$Pupae ~ data_003.3$Line)

pupae_survival_tidy[pupae_survival_tidy$Line %in% c('Benavi', '004.2C'),] -> data_004.2C
t.test(data_004.2C$Pupae ~ data_004.2C$Line)
