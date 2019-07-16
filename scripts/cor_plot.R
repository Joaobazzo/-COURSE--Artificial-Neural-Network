#
# Correlation Plot
#
setwd("/Users/Joao Vieira/Dropbox/PPGEA/2 TRIM/REDES NEURAIS/")
source("ex3/JP_ex_03_mod.R") # funcao 'norma'
source("artigo/scripts/mapa_md.R") #funcao 'mapa'
require(ggplot2)
library(corrplot)
library(RColorBrewer)
require(stringr)
#
# le arquivo
#
input1 <- read.table("artigo/cut1/input_som.txt",sep="\t",header=T)
input <- input1[,-1]
#
# plot
#
mcor <- as.matrix(cor(input))
mor <- round(mcor, digits=2)
col <- heat.colors(dim(mcor)[1])
#
#
salveplot <-jpeg("artigo/imagens/corr.jpg",
                 width = 10,
                 height=10,
                 pointsize=0.5,
                 res = 300,units = "cm")
par(mai=c(5,5,7,5))
corrplot(mcor,tl.srt=45,tl.cex = 0.5)
dev.off()