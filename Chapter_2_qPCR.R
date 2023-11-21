install.packages("ggsci")
install.packages('colorBlindness')
library("ggsci")
setwd("~/Documents/Documents/Lab Work/qPCR_Results/csv")
RPE <- read.csv('RPE_standard.csv')
RPL19 <- read.csv('RPL19_standard.csv')
Cas9 <- read.csv('Cas9_standard.csv')
library(ggplot2)
library(tidyverse)
library(lubridate)
library(ggpmisc)
library(colorBlindness)
colorBlindness::cvdPlot()

#Standard curves 
#RPE Standard curve
RPE_Standard_Curve <- ggplot(RPE, aes(x=Log.Concentration, y=Cq)) +
  geom_point() +
  geom_smooth(method=lm, formula = y ~ x, se = FALSE) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE, label.x.npc = 'right', coef.digits = 5, f.digits = 5, rr.digits = 5) +
  labs(title = 'RPE Standard Curve', x='Log Concentration', y='Ct value') +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5))

print(RPE_Standard_Curve)

#RPL19 Standard Curve
RPL19_Standard_Curve <- ggplot(RPL19, aes(x=Log.Concentration, y=Cq)) +
  geom_point() +
  geom_smooth(method=lm, formula = y ~ x, se = FALSE) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE, label.x.npc = 'right', coef.digits = 5, f.digits = 5, rr.digits = 5) +
  labs(title = 'RPL19 Standard Curve', x='Log Concentration', y='Ct value') +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5))

print(RPL19_Standard_Curve)

#Cas9 Standard Curve
Cas9_Standard_Curve <- ggplot(Cas9, aes(x=Log.Concentration, y=Cq)) +
  geom_point() +
  geom_smooth(method=lm, formula = y ~ x, se = FALSE) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE, label.x.npc = 'right', coef.digits = 5, f.digits = 5, rr.digits = 5) +
  labs(title = 'Cas9 Standard Curve', x='Log Concentration', y='Ct value') +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5))

print(Cas9_Standard_Curve)



#Reference gene analysis
#Standardised
#RPE expression
qPCR <- read_csv('qPCR_results_summary.csv')
standardised_RPE <- ggplot(qPCR, aes(x=qPCR$Tissue, y=qPCR$`RPE Standardised`)) + 
  geom_boxplot() +
  labs(title = 'RPE Expression', x='Tissue Type', y='Ct value') +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))
print(standardised_RPE)

#RPL19 expresion
standardised_RPL19 <- ggplot(qPCR, aes(x=qPCR$Tissue, y=qPCR$`RPL19 Standardised`)) + 
  geom_boxplot() +
  labs(title = 'RPL19 Expression', x='Tissue Type', y='Ct value') +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))
print(standardised_RPL19)

#Unstandardised
#RPE
unstandardised_RPE <- ggplot(qPCR, aes(x=qPCR$Tissue, y=qPCR$`RPE Ct`)) + 
  geom_boxplot() +
  labs(title = 'RPE Expression', x='Tissue Type', y='Ct value') +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))
print(unstandardised_RPE)

#RPL19
unstandardised_RPL19 <- ggplot(qPCR, aes(x=qPCR$Tissue, y=qPCR$`RPL19 Ct`)) + 
  geom_boxplot() +
  labs(title = 'RPL19 Expression', x='Tissue Type', y='Ct value') +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))
print(unstandardised_RPL19)


#Cas9 Expression
Everything_boxplot <- ggplot(qPCR, aes(x=qPCR$Tissue, y=qPCR$`DELTA STANDARDISED`, color=qPCR$Line)) +
  geom_boxplot() +
  labs(title = 'Cas9 Expression', x='Tissue Type', y='Relative Expression', color='Line') +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))
Everything_boxplot_done <- Everything_boxplot + scale_color_manual(values=c("#56B4E9","#009E73","#D55E00","#F0E442"), labels=c('004.2A','004.2C','003.3','003.4'))
print(Everything_boxplot_done)

#Anova of RPE
RPE.one.way <- aov(qPCR$`RPE Ct` ~ qPCR$Tissue)
summary(RPE.one.way)

#Anova of RPL19
RPL19.one.way <- aov(qPCR$`RPL19 Ct` ~ qPCR$Tissue)
summary(RPL19.one.way)

#Relative standard deviation
df1 <- qPCR[complete.cases(qPCR[16]),]
df3 <- df1 %>% group_by(Line, Tissue) %>% summarise(mean=mean(`DELTA STANDARDISED`),sd=sd(`DELTA STANDARDISED`), .groups = 'drop') %>% as.data.frame()

rsd = (df3$sd/df3$mean)*100
df3$rsd = rsd

Relative_standard_deviation <- ggplot(df3, aes(x=Tissue, y=rsd, fill=Line)) +
  geom_bar(stat='identity', position = "dodge") +
  labs(title = 'Relative Standard Deviation', x='Tissue Type', y='Relative standard deviation (%)', color='Line') +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5)) 
Relative_standard_deviation_done <- Relative_standard_deviation + scale_fill_manual(values=c("#56B4E9","#009E73","#D55E00","#F0E442"), labels=c('004.2A','004.2C','003.3','003.4'))
print(Relative_standard_deviation_done)
