#==================================================================================================
# Title:                Food Climate Results
# Date:                 5 April 2022
#==================================================================================================
rm(list=ls())
library(openxlsx)
library(ggpubr)
library(reshape2)
library(Hmisc)
library(zoo)
library(gridExtra)
library(svglite)
load("FoodCrisis.RData")
#--------------------------------------------------------------------------------------------------
baseline                 = subset(lumdata,scenario=="Baseline" & year==1,
                                  select=c("region","commodity","item","value"))
colnames(baseline)       = c("region","commodity","item","basevalue")
scenario                 = subset(lumdata,scenario !="Baseline" & year==1,
                                  select=c("region","commodity","item","scenario","value"))
colnames(scenario)       = c("region","commodity","item","scenario","value")
lumdata                  = merge(scenario,baseline,by=c("region","commodity","item"))
lumdata$diff             = lumdata$value-lumdata$basevalue
lumdata$change           = lumdata$value/lumdata$basevalue-1
#--------------------------------------------------------------------------------------------------
lumdata                  = na.omit(lumdata)
rm(baseline,scenario)
#==================================================================================================
# Price Times Series Plot
#==================================================================================================
df                       = subset(lumdata,item %in% c("Price","Production") & region=="World",
                                  select=c("commodity","scenario","change","item")) 
#--------------------------------------------------------------------------------------------------
ggplot(df,aes(x=commodity,y=change*100,fill=commodity))+facet_grid(vars(item),vars(scenario))+
     geom_bar(stat="identity",position=position_dodge())+theme_bw()+
     ylab("% Change from Baseline")+
     scale_fill_manual(values=c("#a6cee3","#1f78b4","#b2df8a","#33a02c"))+
     scale_y_continuous(breaks=c(0,5,10),labels=c("0","5","10"),limits=c(-1,10))+
        theme(legend.position="none",
              legend.title=element_blank(),
              panel.grid.minor=element_blank(),
              axis.title.x=element_blank())
ggsave("priceproduction.pdf",width=6,height=3)
#==================================================================================================
# End of File
#==================================================================================================