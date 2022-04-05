function(L1,L2){
     carbon                        = data.frame(region=gis$region)
     carbonmin                     = as.matrix(gis$carbonmin)
     carbonmed                     = as.matrix(gis$carbonmed)
     carbonmax                     = as.matrix(gis$carbonmax)
     carbonpot                     = as.matrix(gis$carbonstorage)
     cmin                          = (carbonmin-6)+0.25*gis$soilc
     cmed                          = (carbonmed-8)+0.25*gis$soilc
     cmax                          = (carbonmax-20)+0.25*gis$soilc
     cpot                          = (carbonmax-8)
     cmin[cmin<0]                  = 0
     cmed[cmed<0]                  = 0
     cmax[cmax<0]                  = 0
     cmax[cpot<0]                  = 0
     carbon$min                    = (L2[,1]-L1[,1])*cmin*3.67/1e6
     carbon$med                    = (L2[,1]-L1[,1])*cmed*3.67/1e6
     carbon$max                    = (L2[,1]-L1[,1])*cmax*3.67/1e6
     carbon$pot                    = (L2[,1]-L1[,1])*cpot*3.67/1e6
     carbon                        = aggregate(.~region,data=carbon,FUN=sum)
     colnames(carbon)              = c("region","Minimum","Mean","Maximum","Potential")
     return(carbon)}