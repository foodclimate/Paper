#==================================================================================================
# Title:                 Food Climate: Carbon Emissions
# Date:                  14 June 2022
#==================================================================================================
rm(list=ls())
library(openxlsx)
library(reshape2)
library(gridExtra)
library(ggplot2)
library(ggpubr)
core                     = c("(A1) No UA Exports","(A2) RU 50% Exports","(A3) EU-US 50% Bio",
                             "(A4) UA Production 50%")
#--------------------------------------------------------------------------------------------------
LandUseNonDynamic        = dget("LandUseNonDynamic.R")
CarbonEmissions          = dget("CarbonEmissions.R")
#==================================================================================================
# Load GHG Model Parameters
#==================================================================================================
load("GHGData.RData")
cellcount               = data.frame(table(gis$region))
colnames(cellcount)     = c("region","cellcount")
index2                  = cumsum(cellcount$cellcount)
index1                  = c(1,index2[1:21]+1)
index                   = as.matrix(cbind(index1,index2))
#--------------------------------------------------------------------------------------------------
# Calculating Crop Area and Crop Share (of a Particular Country) in Each Cell
#--------------------------------------------------------------------------------------------------
gis                      = as.data.frame(gis[order(gis$region),])
row.names(gis)           = NULL
for(i in 1:22){
     for(j in 1:9){
        id1                   = as.numeric(index[i,1])
        id2                   = as.numeric(index[i,2])
        temp                  = gis[c(id1:id2),5+j]
        temp                  = temp/sum(temp)
        temp[is.na(temp)]     = 0
        gis[id1:id2,5+j]      = temp}}
rm(index1,index2,id1,id2,i,j)
#==================================================================================================
# Land-Use Non-Dynamic
#==================================================================================================
lumdata                  = subset(lumdata,
                                  item=="Area Harvested" & region != "World" & 
                                       commodity != "All crops",
                                  select=c("commodity","region","value","scenario"))
lumdata$value            = lumdata$value*1000
#--------------------------------------------------------------------------------------------------
inputS0                  = subset(lumdata,scenario=="Baseline")
inputA1                  = subset(lumdata,scenario=="(A1) No UA Exports")
inputA2                  = subset(lumdata,scenario=="(A2) RU 50% Exports")
inputA3                  = subset(lumdata,scenario=="(A3) EU-US 50% Bio")
inputA4                  = subset(lumdata,scenario=="(A4) UA Production 50%")
inputB1                  = subset(lumdata,scenario=="(B1) RU 25% Exports")
inputB2                  = subset(lumdata,scenario=="(B2) RU 75% Exports")
inputB3                  = subset(lumdata,scenario=="(B3) No RU-UA Exports")
inputB4                  = subset(lumdata,scenario=="(B4) EU 50% Bio")
inputB5                  = subset(lumdata,scenario=="(B5) US 50% Bio")
inputB6                  = subset(lumdata,scenario=="(B6) RU 50% Exports and EU 50% Bio")
inputB7                  = subset(lumdata,scenario=="(B7) RU 50% Exports and US 50% Bio")
inputB8                  = subset(lumdata,scenario=="(B8) RU 50% Exports and EU-US 50% Bio")
inputB9                  = subset(lumdata,scenario=="(B9) UA Production 25%")
#--------------------------------------------------------------------------------------------------
inputS0                  = LandUseNonDynamic(inputS0)
inputA1                  = LandUseNonDynamic(inputA1)
inputA2                  = LandUseNonDynamic(inputA2)
inputA3                  = LandUseNonDynamic(inputA3)
inputA4                  = LandUseNonDynamic(inputA4)
inputB1                  = LandUseNonDynamic(inputB1)
inputB2                  = LandUseNonDynamic(inputB2)
inputB3                  = LandUseNonDynamic(inputB3)
inputB4                  = LandUseNonDynamic(inputB4)
inputB5                  = LandUseNonDynamic(inputB5)
inputB6                  = LandUseNonDynamic(inputB6)
inputB7                  = LandUseNonDynamic(inputB7)
inputB8                  = LandUseNonDynamic(inputB8)
inputB9                  = LandUseNonDynamic(inputB9)
#--------------------------------------------------------------------------------------------------
carbon                   = CarbonEmissions(inputS0,inputA1)
carbon$scenario          = "(A1) No UA Exports"
temp                     = CarbonEmissions(inputS0,inputA2)
temp$scenario            = "(A2) RU 50% Exports"
carbon                   = rbind(carbon,temp)
temp                     = CarbonEmissions(inputS0,inputA3)
temp$scenario            = "(A3) EU-US 50% Bio"
carbon                   = rbind(carbon,temp)
temp                     = CarbonEmissions(inputS0,inputA4)
temp$scenario            = "(A4) UA Production 50%" 
carbon                   = rbind(carbon,temp)
temp                     = CarbonEmissions(inputS0,inputB1)
temp$scenario            = "(B1) RU 25% Exports"
carbon                   = rbind(carbon,temp)
temp                     = CarbonEmissions(inputS0,inputB2)
temp$scenario            = "(B2) RU 75% Exports"
carbon                   = rbind(carbon,temp)
temp                     = CarbonEmissions(inputS0,inputB3)
temp$scenario            = "(B3) No RU-UA Exports"
carbon                   = rbind(carbon,temp)
temp                     = CarbonEmissions(inputS0,inputB4)
temp$scenario            = "(B4) EU 50% Bio"
carbon                   = rbind(carbon,temp)
temp                     = CarbonEmissions(inputS0,inputB5)
temp$scenario            = "(B5) US 50% Bio"
carbon                   = rbind(carbon,temp)
temp                     = CarbonEmissions(inputS0,inputB6)
temp$scenario            = "(B6) RU 50% Exports and EU 50% Bio"
carbon                   = rbind(carbon,temp)
temp                     = CarbonEmissions(inputS0,inputB7)
temp$scenario            = "(B7) RU 50% Exports and US 50% Bio"
carbon                   = rbind(carbon,temp)
temp                     = CarbonEmissions(inputS0,inputB8)
temp$scenario            = "(B8) RU 50% Exports and EU-US 50% Bio"
carbon                   = rbind(carbon,temp)
temp                     = CarbonEmissions(inputS0,inputB9)
temp$scenario            = "(B9) UA Production 25%"
carbon                   = rbind(carbon,temp)
carbon                   = melt(carbon,id=c("region","scenario"))
#--------------------------------------------------------------------------------------------------
rm(inputA1,inputA2,inputA3,inputA4,inputB1,inputB2,inputB3,inputB4,inputB5,inputB6,inputB7,
   inputB8,inputB9,inputS0,cellcount,gis,index,lumdata,temp)
#--------------------------------------------------------------------------------------------------
carbon                   = subset(carbon,!(region %in% c("Ukraine","Russia")))
#--------------------------------------------------------------------------------------------------
aggregion                = c("Argentina","Australia","Chile","Egypt","Indonesia","Japan",
                             "Malaysia","New Zealand","Nigeria","Peru","South Africa","Vietnam")
carbon$region[which(carbon$region %in% aggregion)] = "Other Countries"
carbon                   = aggregate(carbon$value,FUN=sum,by=list(carbon$region,
                                                                  carbon$variable,
                                                                  carbon$scenario))
colnames(carbon)         = c("region","variable","scenario","value")
totalcarbon              = aggregate(carbon$value,FUN=sum,by=list(carbon$variable,carbon$scenario))
colnames(totalcarbon)    = c("coefficient","scenario","value")
totalcarbon
#--------------------------------------------------------------------------------------------------
# Carbon Plots (Core Scenarios)
#--------------------------------------------------------------------------------------------------
legend_title             = "Carbon Coefficients"
df                       = subset(carbon,scenario %in% core)
ggplot(df,aes(x=region,y=value,fill=variable))+
     geom_bar(position='dodge',stat="identity")+theme_bw()+
     ylab(bquote('in Million Mg '~CO[2]~'-e'))+facet_grid(vars(scenario))+
     theme(axis.text.x=element_text(angle=90,hjust=1),axis.title.x=element_blank(),
           legend.position="bottom",
           panel.grid.minor=element_blank())+
           scale_fill_brewer(legend_title,palette="Paired")
ggsave("emissions.pdf",width=6,height=8)
#--------------------------------------------------------------------------------------------------
# Carbon Plots (Core Scenarios)
#--------------------------------------------------------------------------------------------------
legend_title             = "Carbon Coefficients"
df                       = subset(carbon,!scenario %in% core)
ggplot(df,aes(x=region,y=value,fill=variable))+
     geom_bar(position='dodge',stat="identity")+theme_bw()+
     ylab(bquote('in Million Mg '~CO[2]~'-e'))+facet_wrap(vars(scenario),ncol=1)+
     theme(axis.text.x=element_text(angle=90,hjust=1),axis.title.x=element_blank(),
           legend.position="bottom",
           panel.grid.minor=element_blank())+
     scale_fill_brewer(legend_title,palette="Paired")
ggsave("emissionsOM.pdf",width=6,height=10)
#==================================================================================================
# End of File
#==================================================================================================