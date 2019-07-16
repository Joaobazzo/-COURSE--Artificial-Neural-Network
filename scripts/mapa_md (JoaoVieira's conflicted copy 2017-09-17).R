mapa<-function(input,cluster,pts){
  #
  #
  library(rworldmap)
  library(RColorBrewer)
  #
  # criterio de ordem
  #
  nome <- c()
  for(i in (1:dim(pts)[1])){
    nome[i] <- paste0("(",pts[i,1],",",pts[i,2],")")
  }
  xx <- length(pts[,1])-length(which(duplicated(pts[,1])))
  yy <- length(pts[,2])-length(which(duplicated(pts[,2])))
  #
  #
  malDF <- data.frame(country = input,
                      Clusterizacao = cluster)
  # malDF is a data.frame with the ISO3 country names plus a variable to
  # merge to the map data
  
  malMap <- joinCountryData2Map(malDF, joinCode = "NAME",
                                nameJoinColumn = "country")
  # This will join your malDF data.frame to the country map data
  n <- xx*yy
  print(n)
  
  lista_cor <- heat.colors(n*2)[(1:n)*2][order(heat.colors(n*2)[(1:n)*2],decreasing = T)]
  export <- mapCountryData(malMap,borderCol="black",lwd=0.6,
                           oceanCol = "lightcyan",
                           nameColumnToPlot="Clusterizacao",
                           catMethod = "categorical",
                 missingCountryCol = gray(.8),addLegend=TRUE,
                 colourPalette = lista_cor)
  tamanho <- length(which(duplicated(cluster)==F))
  
 legend(legend = nome,x="bottomleft",cex = 12,pch = 15,ncol=yy,
        col=lista_cor,bg="grey89",pt.cex=15,box.col="black")
 #x = "bottomleft",
  #                  legendText = 1:tamanho,
   #                 pt.cex = 4,colourVector = terrain.colors(tamanho))
  
  
  #addMapLegendBoxes(cutVector = tamanho,cex = 5,x = "bottomleft",
  #                  legendText = 1:tamanho,
  #                  pt.cex = 4,colourVector = terrain.colors(tamanho))
  
  return(export)
}
##
##
##
##
##
mapa_idh<-function(input,cluster,lista){
  #
  #
  library(rworldmap)
  library(RColorBrewer)
  #
  #
  malDF <- data.frame(country = input,
                      Clusterizacao = cluster)
  # malDF is a data.frame with the ISO3 country names plus a variable to
  # merge to the map data
  
  malMap <- joinCountryData2Map(malDF, joinCode = "NAME",
                                nameJoinColumn = "country")
  # This will join your malDF data.frame to the country map data
  lista_cor <- heat.colors(4*2)[(1:4)*2][order(heat.colors(4*2)[(1:4)*2],decreasing = T)]
  
  export <- mapCountryData(malMap,borderCol="black",oceanCol = "lightcyan",nameColumnToPlot="Clusterizacao",catMethod = "categorical",
                           missingCountryCol = gray(.8),addLegend=TRUE,
                          colourPalette  = lista_cor)
  
  legend(legend = lista,x="bottomleft",cex = 12,pch = 15,ncol=4,
         col=lista_cor,bg="grey89",pt.cex=17,box.col="black")
  #x = "bottomleft",
  #                  legendText = 1:tamanho,
  #                 pt.cex = 4,colourVector = terrain.colors(tamanho))
  
  
  #addMapLegendBoxes(cutVector = tamanho,cex = 5,x = "bottomleft",
  #                  legendText = 1:tamanho,
  #                  pt.cex = 4,colourVector = terrain.colors(tamanho))
  
  return(export)
}

