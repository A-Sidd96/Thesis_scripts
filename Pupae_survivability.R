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
  theme(legend.position = "bottom")

pupae_survival_done <- pupae_survival + scale_color_manual(values=c("#D55E00","#009E73","#000000"))
print(pupae_survival_done)
