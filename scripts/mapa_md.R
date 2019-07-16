mapa<-function(input,cluster){
  #
  #
  library(rworldmap)
  library(RColorBrewer)
  malDF <- data.frame(country = input,
                      Clusterizacao = cluster)
  # malDF is a data.frame with the ISO3 country names plus a variable to
  # merge to the map data
  
  malMap <- joinCountryData2Map(malDF, joinCode = "NAME",
                                nameJoinColumn = "country")
  # This will join your malDF data.frame to the country map data
  
  export <- mapCountryData(malMap,borderCol="black",nameColumnToPlot="Clusterizacao",catMethod = "categorical",
                 missingCountryCol = gray(.8),addLegend=TRUE)
  #addMapLegendBoxes(cutVector=cluster,horiz=TRUE,x="bottomright")
  
  return(export)
}

