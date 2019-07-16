#
# Rede de Kohonem
#
setwd("/Users/Joao Vieira/Dropbox/PPGEA/2 TRIM/REDES NEURAIS/")
source("ex3/JP_ex_03_mod.R") # funcao 'norma'
source("artigo/scripts/mapa_md.R") #funcao 'mapa'
require(SOMbrero)
require(kohonen)
require(rworldmap)
#
# le arquivo
#
input1 <- read.table("artigo/cut1/input_som.txt",sep="\t",header=T)
input <- norma(input1[,2:dim(input1)[2]])
#input <- input1[,1]
#
#
dimm <- list(c(1,4),c(2,2),c(3,1),c(1,3),c(3,2),c(3,3))
dimm <- list(c(2,1))
for(i in (1:length(dimm))){
  gradesom <- somgrid(xdim = dimm[[i]][1],
                    ydim = dimm[[i]][2],
                    topo = c("rectangular"),
                    neighbourhood.fct = "gaussian",
                    toroidal = FALSE)
  unit.distances(grid = gradesom,toroidal = FALSE)
  objsom <- supersom(data = input,grid = gradesom,rlen=500,
         alpha = c(0.05,0.01),radius = c(2,1),
         keep.data = TRUE,mode="online",
         normalizeDataLayers = TRUE)
  #
  #
  fig_mapa1 <- paste0("artigo/simulacoes/sim_02/mapa_",
                    dimm[[i]][1],"x",dimm[[i]][2],".jpg")
  salveplot <-jpeg(fig_mapa1,
                 width = 20,
                 height=10,
                 pointsize=1.0,
                 res = 1000,units = "cm")
  mapa(input1[,1],objsom$unit.classif,objsom$grid$pts)
  #addMapLegendBoxes(cutVector = tamanho,cex = 5,x = "bottomleft",
   #                 legendText = 1:tamanho,
    #                pt.cex = 4,colourVector = heat.colors(tamanho))
  
  dev.off()
  }

