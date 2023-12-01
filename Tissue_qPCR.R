getwd()
library(pcr)
library(ggplot2)

#####pcr analysis#####
######tra line######
Shibo_T <- read.csv("/Users/sad/Desktop/Shibo T.csv")
group_var <- rep(c('Tra_t', 'WT'), each = 3)
Tra_T <- pcr_analyze(Shibo_T,
                     group_var = group_var,
                     reference_gene = 'Rpl19',
                     reference_group = 'WT')
Tra_testes<-subset(Tra_T,Tra_T$group!="WT")

Shibo_O <- read.csv("/Users/sad/Desktop/Shibo_O.csv")
group_var <- rep(c('Tra_o', 'WT'), each = 3)
Tra_O <- pcr_analyze(Shibo_O,
                     group_var = group_var,
                     reference_gene = 'Rpl19',
                     reference_group = 'WT')
Tra_ovaries<-subset(Tra_O,Tra_O$group!="WT")
# export to excel and change values to 0 as there are no values at all
Shibo_O_table <- read.csv("/Users/sad/Desktop/Shibo_O_table.csv")

Shibo_C <- read.csv("/Users/sad/Desktop/Shibo C.csv")
group_var <- rep(c('Tra_mc', 'WT'), each = 3)
Tra_MC <- pcr_analyze(Shibo_C,
                     group_var = group_var,
                     reference_gene = 'Rpl19',
                     reference_group = 'WT')
Tra_mcarcass<-subset(Tra_MC,Tra_MC$group!="WT")

Shibo_FC <- read.csv("/Users/sad/Desktop/Shibo_FC.csv")
group_var <- rep(c('Tra_fc', 'WT'), each = 3)
Tra_FC <- pcr_analyze(Shibo_FC,
                      group_var = group_var,
                      reference_gene = 'Rpl19',
                      reference_group = 'WT')
Tra_fcarcass<-subset(Tra_FC,Tra_FC$group!="WT")
# export to excel and change values to 0 as there are no values at all
Shibo_FC_table <- read.csv("/Users/sad/Desktop/Shibo_FC_table.csv")

######WE tra line######
wetraT <- read.csv("/Users/sad/Desktop/wetraT.csv")
group_var <- rep(c('WEtra_t', 'WT'), each = 3)
WEtra_T <- pcr_analyze(wetraT,
                     group_var = group_var,
                     reference_gene = 'Rpl19',
                     reference_group = 'WT')
WEtra_testes<-subset(WEtra_T,WEtra_T$group!="WT")

wetraO <- read.csv("/Users/sad/Desktop/wetraO.csv")
group_var <- rep(c('WEtra_o', 'WT'), each = 3)
WEtra_O <- pcr_analyze(wetraO,
                       group_var = group_var,
                       reference_gene = 'Rpl19',
                       reference_group = 'WT')
WEtra_ovaries<-subset(WEtra_O,WEtra_O$group!="WT")

wetraMC <- read.csv("/Users/sad/Desktop/wetraMC.csv")
group_var <- rep(c('WEtra_mc', 'WT'), each = 3)
WEtra_MC <- pcr_analyze(wetraMC,
                       group_var = group_var,
                       reference_gene = 'Rpl19',
                       reference_group = 'WT')
WEtra_mcarcass<-subset(WEtra_MC,WEtra_MC$group!="WT")

wetra_FC <- read.csv("/Users/sad/Desktop/wetraFC.csv")
group_var <- rep(c('WEtra_fc', 'WT'), each = 3)
WEtra_FC <- pcr_analyze(wetraFC,
                       group_var = group_var,
                       reference_gene = 'Rpl19',
                       reference_group = 'WT')
WEtra_fcarcass<-subset(WEtra_FC,WEtra_FC$group!="WT")

#####ggplot####

data<-rbind(Tra_testes, Shibo_O_table, Tra_mcarcass, Shibo_FC_table, WEtra_testes, WEtra_ovaries, WEtra_mcarcass, WEtra_fcarcass)
data$tissue<-c("Testes", "Ovaries", "MCarcass", "FCarcass", "Testes", "Ovaries", "MCarcass", "FCarcass")
data$strain<-c("Vasa_Tra","Vasa_Tra", "Vasa_Tra","Vasa_Tra", "Vasa_WE_tra", "Vasa_WE_tra", "Vasa_WE_tra", "Vasa_WE_tra")
  
  
ggplot(data, aes(x = tissue, y = relative_expression,fill=strain)) +
    geom_bar(stat="identity",position="dodge",color="black", alpha=0.8) +
    geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position=position_dodge(.9))+
    coord_flip()+
    scale_fill_manual(values=c("#A034F0", "#159090"))+
    labs(y= "relative expression",
         x = "tissue")+
    scale_x_discrete(labels=c("Testes"="testes", "Ovaries"="ovaries", "MCarcass"="male \n carcass", "FCarcass" ="female \n carcass"))+
    guides(fill = guide_legend(reverse = TRUE))+
    theme(legend.text = element_text(face = "italic"))+
    theme_bw()

                