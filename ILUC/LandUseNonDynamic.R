function(inputFC){
     cropland           = matrix(0,851474,9)
     commodities        = c("Barley","Maize","Oilpalm","Rice","Rapeseed","Soybeans","Sorghum","Sunflower","Wheat")
     for(i in 1:22){
          name          = as.character(cellcount$region[i])
          id1           = index[i,1]
          id2           = index[i,2]
          for(j in 1:9){
               t1                       = subset(inputFC,inputFC$commodity==commodities[j] & 
                                                         region == name)[c("value")] 
               t2                       = gis[which(gis$region==name),commodities[j]]
               cropland[id1:id2,j]      = as.numeric(t1)*as.matrix(t2)}}
     cropland[is.na(cropland)]          = 0
     cropland[is.infinite(cropland)]    = 0
     cropland                           = rowSums(cropland)
     out                                = data.frame(cropland)
     return(out)}