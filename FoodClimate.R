#==================================================================================================
# Title:                Food Climate Results
# Date:                 14 June 2022
#==================================================================================================
rm(list=ls())
library(openxlsx)
library(ggpubr)
library(reshape2)
library(Hmisc)
library(zoo)
library(gridExtra)
library(svglite)
#--------------------------------------------------------------------------------------------------
load("FoodClimate.RData")
#==================================================================================================
# Food Security: Per-capita food consumption and self-sufficiency ratio (SSR)
# Source: https://fdc.nal.usda.gov/ (values per 100 g)
#==================================================================================================
core                     = c("(A1) No UA Exports","(A2) RU 50% Exports","(A3) EU-US 50% Bio",
                             "(A4) UA Production 50%")
energy                   = data.frame(commodity=c("Barley","Maize","Rice","Sorghum","Wheat"),
                                      energy=c(1480,1527,1506,1380,1370)*10)
df                       = subset(lumdata,item=="Per-Capita Food Consumption")
df                       = merge(df,energy,by=c("commodity"))
df$value                 = df$value*df$energy
df                       = aggregate(df$value,FUN=sum,by=list(df$region,df$scenario))
colnames(df)             = c("region","scenario","value")
df$commodity             = "Aggregate (5 Grains)"
df$item                  = "Per-Capita Food Consumption"
#--------------------------------------------------------------------------------------------------
#lumdata                  = subset(lumdata,item != "Per-Capita Food Consumption")
lumdata                  = rbind(lumdata,df)
#--------------------------------------------------------------------------------------------------
df                       = subset(lumdata,
                                  commodity %in% c("Barley","Maize","Rice","Sorghum","Wheat") &
                                  item %in% c("Production","Net Exports"))
df                       = merge(df,energy,by="commodity")
df$value                 = df$value*df$energy
df                       = aggregate(df$value,FUN=sum,by=list(df$region,df$scenario,df$item))
colnames(df)             = c("region","scenario","item","value")
df                       = dcast(df,region+scenario~item,value.var="value")
df$ssr                   = 100*df$Production/(df$Production-df$`Net Exports`)
df                       = df[c("region","scenario","ssr")]
colnames(df)             = c("region","scenario","value")
df$commodity             = "Aggregate (5 Grains)"
df$item                  = "SSR"
lumdata                  = rbind(lumdata,df)
rm(df,energy)
#--------------------------------------------------------------------------------------------------
baseline                 = subset(lumdata,scenario=="Baseline",
                                  select=c("region","commodity","item","value"))
colnames(baseline)       = c("region","commodity","item","basevalue")
scenario                 = subset(lumdata,scenario !="Baseline",
                                  select=c("region","commodity","item","scenario","value"))
colnames(scenario)       = c("region","commodity","item","scenario","value")
lumdata                  = merge(scenario,baseline,by=c("region","commodity","item"))
lumdata$diff             = lumdata$value-lumdata$basevalue
lumdata$change           = lumdata$value/lumdata$basevalue-1
#--------------------------------------------------------------------------------------------------
lumdata                  = na.omit(lumdata)
rm(baseline,scenario)
write.csv(lumdata,"foodcrisis.csv",row.names=FALSE)
#==================================================================================================
# Price and Production Plot (Core Scenarios)
#==================================================================================================
df                       = subset(lumdata,
                                  item %in% c("Price","Production") &
                                       region=="World" & commodity != "Sunflower" &
                                       scenario %in% core,
                                  select=c("commodity","scenario","change","item")) 
#--------------------------------------------------------------------------------------------------
ggplot(df,aes(x=commodity,y=change*100,fill=commodity))+facet_grid(vars(item),vars(scenario))+
     geom_bar(stat="identity",position=position_dodge())+theme_bw()+
     ylab("% Change from Baseline")+scale_fill_brewer(palette="Paired")+
     theme(legend.position="none",legend.title=element_blank(),panel.grid.minor=element_blank(),
           axis.title.x=element_blank(),axis.text.x=element_text(angle=45,hjust=1))
ggsave("priceproduction.pdf",width=8,height=4)
#==================================================================================================
# Price Plot (Non Core Scenarios)
#==================================================================================================
df_price                 = subset(lumdata,
                                  item %in% c("Price") &
                                       region=="World" & commodity != "Sunflower" &
                                       !scenario %in% core,
                                  select=c("commodity","scenario","change","item")) 
#--------------------------------------------------------------------------------------------------
df_production            = subset(lumdata,
                                  item %in% c("Production") &
                                       region=="World" & commodity != "Sunflower" &
                                       !scenario %in% core,
                                  select=c("commodity","scenario","change","item"))
#--------------------------------------------------------------------------------------------------
ggplot(df_price,aes(x=commodity,y=change*100,fill=commodity))+
     facet_wrap(vars(scenario),ncol=3)+
     geom_bar(stat="identity",position=position_dodge())+theme_bw()+
     ylab("% Change in Price from Baseline")+scale_fill_brewer(palette="Paired")+
     theme(legend.position="none",legend.title=element_blank(),panel.grid.minor=element_blank(),
           axis.title.x=element_blank(),axis.text.x=element_text(angle=45,hjust=1))
ggsave("priceOM.pdf",width=8.5,height=7)
#--------------------------------------------------------------------------------------------------
ggplot(df_production,aes(x=commodity,y=change*100,fill=commodity))+
     facet_wrap(vars(scenario),ncol=3)+
     geom_bar(stat="identity",position=position_dodge())+theme_bw()+
     ylab("% Change in Production from Baseline")+scale_fill_brewer(palette="Paired")+
     theme(legend.position="none",legend.title=element_blank(),panel.grid.minor=element_blank(),
           axis.title.x=element_blank(),axis.text.x=element_text(angle=45,hjust=1))
ggsave("productionOM.pdf",width=8.5,height=7)
#==================================================================================================
# Per-Capita Food Consumption (Core Scenarios)
#==================================================================================================
df                       = subset(lumdata,item %in% c("Per-Capita Food Consumption") &
                                       scenario %in% core & !region %in% c("Russia","Ukraine"),
                                  select=c("region","scenario","change")) 
#--------------------------------------------------------------------------------------------------
ggplot(df,aes(x=scenario,y=change*100,fill=scenario))+facet_wrap(vars(region))+
     geom_bar(stat="identity",position=position_dodge())+theme_bw()+
     ylab("% Change from Baseline")+scale_fill_brewer(palette="Paired")+
     theme(legend.position="none",legend.title=element_blank(),panel.grid.minor=element_blank(),
           axis.title.x=element_blank(),axis.text.x=element_text(angle=90,hjust=1))
ggsave("percapitafood.pdf",width=6,height=8)
#==================================================================================================
# Per-Capita Food Consumption (Non Core Scenarios)
#==================================================================================================
df                       = subset(lumdata,item %in% c("Per-Capita Food Consumption") &
                                       !scenario %in% core & !region %in% c("Russia","Ukraine"),
                                  select=c("region","scenario","change")) 
#--------------------------------------------------------------------------------------------------
ggplot(df,aes(x=scenario,y=change*100,fill=scenario))+facet_wrap(vars(region))+
     geom_bar(stat="identity",position=position_dodge())+theme_bw()+
     ylab("% Change from Baseline")+scale_fill_brewer(palette="Paired")+
     theme(legend.position="none",legend.title=element_blank(),panel.grid.minor=element_blank(),
           axis.title.x=element_blank(),axis.text.x=element_text(angle=90,hjust=1))
ggsave("percapitafoodOM.pdf",width=8,height=10)
#==================================================================================================
# End of File
#==================================================================================================