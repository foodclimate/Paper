#==================================================================================================
# Title:                 Food Crisis: Carbon Emissions
# Date:                  5 April 2022
#==================================================================================================
rm(list=ls())
library(openxlsx)
library(reshape2)
library(gridExtra)
library(ggplot2)
library(ggpubr)
load("FoodClimate.RData")
#--------------------------------------------------------------------------------------------------
LandUseNonDynamic       = dget("LandUseNonDynamic.R")
CarbonEmissions         = dget("CarbonEmissions.R")
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
     for(j in 1:4){
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
                                  item=="Area harvested" & 
                                  commodity %in% c("Maize","Rice","Soybeans",'Wheat') &
                                  region != "World" &
                                  year==1,
                                  select=c("commodity","region","value","year","scenario"))
lumdata$value            = lumdata$value*1000
#--------------------------------------------------------------------------------------------------
inputS0                  = subset(lumdata,scenario=="Baseline")
inputS1                  = subset(lumdata,scenario=="No Exports")
inputS2                  = subset(lumdata,scenario=="No Exports and 50% Less US Bio.")
inputS0                  = LandUseNonDynamic(inputS0)
inputS1                  = LandUseNonDynamic(inputS1)
inputS2                  = LandUseNonDynamic(inputS2)
#--------------------------------------------------------------------------------------------------
carbon                   = CarbonEmissions(inputS0,inputS1)
carbon$scenario          = "No Exports"
temp                     = CarbonEmissions(inputS0,inputS2)
temp$scenario            = "No Exports and 50% Less US Bio."
carbon                   = rbind(carbon,temp)
carbon                   = melt(carbon,id=c("region","scenario"))
#--------------------------------------------------------------------------------------------------
rm(inputS0,inputS1,inputS2,cellcount,gis,index,lumdata,temp)
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
ggplot(subset(carbon),aes(x=region,y=value,fill=variable))+
     geom_bar(position='dodge',stat="identity")+theme_bw()+
     ylab(bquote('in Million Mg '~CO[2]~'-e'))+facet_grid(vars(scenario))+
     theme(axis.text.x=element_text(angle=45,hjust=1),axis.title.x=element_blank(),
           legend.position="bottom",legend.title=element_blank(),
           panel.grid.minor=element_blank())+
           scale_fill_brewer(palette="Paired")
ggsave("emissions.pdf",width=6,height=6)
#==================================================================================================
# End of File
#==================================================================================================